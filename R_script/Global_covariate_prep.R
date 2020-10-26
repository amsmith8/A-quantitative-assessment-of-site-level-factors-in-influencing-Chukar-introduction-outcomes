###################################################
### code chunk number 1: set up environment 
###################################################


# Run the following BEFORE uploading any package.  This will avoid the "ava.lang.OutOfMemoryError: Java heap space"
options(java.parameters = "-Xmx4g" ) # Gigs

######## ########  Uploaded Packages ######## ########

# data processing

library("raster")
library("tictoc")


# Elevation data 
# elevation <- raster("wc2.1_2.5m_elev.tif")
# 
# # Create new raster opbjects to extract values. 
# # ***NOTE: terrain function broken in  R ver3.5.2. May need later edition***
# asp <- terrain(elevation, opt = "aspect", unit = "degrees", df = F)
# slo <- terrain(elevation, opt = "slope", unit = "degrees", df = F)
# tri <- terrain(elevation, opt ="TRI", unit = "degrees", df = F) #Terrain Roughness Index
# 
# # write rasters
# writeRaster(asp, filename = "aspect_2-5m_calculated_wc2-1",format="GTiff")
# writeRaster(slo, filename = "slope_2-5m_calculated_wc2-1",format="GTiff")
# writeRaster(tri, filename = "TRI_32-5m_calculated_wc2-1",format="GTiff")






# Create list to call files to make raster stacks 
topo_list <- list.files(path = "./Raster_files", full.names= TRUE, pattern = ".tif")
bioclim_list <- list.files(path = "./Raster_files/wc2",full.names= TRUE, pattern = ".tif")

# Function 1 - creates stack from list of files
list_to_stack <- function(tiff.list){
  model.stack <- stack()
  for (i in 1:length(tiff.list)){
    r <- raster(tiff.list[i])
    model.stack <- stack(model.stack, r)}
  model.stack
}


# Create stacks
topo_stack <- list_to_stack(topo_list)
names(topo_stack) <- c("aspect", "slope", "TRI", "elevation")

bioclim_stack <- list_to_stack(bioclim_list)
names(bioclim_stack) <- c( "bio1" , "bio10" , "bio11" , "bio12" , "bio13" , "bio14" , "bio15" , "bio16" , "bio17" , "bio18" , 
                           "bio19" , "bio2" , "bio3" , "bio4" , "bio5" , "bio6 ", "bio7" , "bio8" , "bio9"  )


super_stack <- stack(topo_stack,bioclim_stack)



# NORMILIZATION OF RASTERS

# Function: normalize

# Normalize raster layer
normalize <- function(x) {
  min <- raster::minValue(x)
  max <- raster::maxValue(x)
  return((x - min) / (max - min))
}

tic("normalize")
super_stack <- normalize(super_stack)
toc()



# Citations for Data used:



# Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate 
# surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.