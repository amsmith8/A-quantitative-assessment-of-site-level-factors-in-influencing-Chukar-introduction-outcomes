library("dismo")
library("ggplot2")
library("ggpubr")
library("maptools")
library("raster")
library("RColorBrewer")

gbm_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_gbm_folds_CA", full.names= TRUE, pattern = ".tif")

#Function 1
#--------------------------------------------------
# function to stack raster layers
listToStack <- function(tiff_list){
  model_stack <- stack()
  for (i in 1:length(tiff_list)){
    r <- raster(tiff_list[i])
    model_stack <- stack(model_stack, r)}
  model_stack
}

#compile tiffs to stacks 
model_stack_gbm<- listToStack(gbm_tiffs)




# http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat

states <- rgdal::readOGR("/Users/austinsmith/Downloads/ne_110m_admin_1_states_provinces/ne_110m_admin_1_states_provinces.shp") 
crs(states) <- crs(model_stack_gbm$gbmFold1)

# Ac_poly <- rgdal::readOGR("/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp") # via Bird Life
# crs(Ac_poly) <- crs(model_stack_gbm$gbmFold1)
# #r <- rasterize(Ac.poly, Final.model, field=1)
# 
# 
# ### Seperate the polygon into native and naturalized regions
# native <- subset(Ac_poly, Ac_poly$OBJECTID == 36 )  # historical  native range for A. chukar. Similar to Christensen 1970
# naturalized <- subset(Ac_poly, Ac_poly$OBJECTID != 36 )
# 
# fort_native <- fortify(native)
# fort_nat <- fortify(naturalized)



source( "R_script/eBird_data_cleaning.R" )


naturalized <- circles(us_pts, d = d , dissolve=TRUE, lonlat=TRUE) #60km is the average distance recorded 
naturalized  <- polygons(naturalized )


#fort_native <- fortify(native)
fort_nat <- fortify(naturalized)


# import threshholds 
sensSpec_scores <- readRDS("./RDS_objects/CA/sensSpec_scores_CA.rds")

# Compute means for averaged model 

gbm_mean <- mean(model_stack_gbm)
gbm_mean_sensSpec <- rowMeans(sensSpec_scores[2,-1])

# -------------------------------------------------------------------------------------------------------------------------------


# Create fold data frames for ggplots 

# Raw

# gbm_fold1_raw
gbm_fold1_raw_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold1 ) )
colnames(gbm_fold1_raw_df) <-c("long", "lat", "Score")
# gbm_fold1_raw
gbm_fold2_raw_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold2 ) )
colnames(gbm_fold2_raw_df) <-c("long", "lat", "Score")
# gbm_fold1_raw
gbm_fold3_raw_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold3 ) )
colnames(gbm_fold3_raw_df) <-c("long", "lat", "Score")
# gbm_fold1_raw
gbm_fold4_raw_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold4 ) )
colnames(gbm_fold4_raw_df) <-c("long", "lat", "Score")
# gbm_fold1_raw
gbm_fold5_raw_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold5 ) )
colnames(gbm_fold5_raw_df) <-c("long", "lat", "Score")
# AVERAGE
gbm_mean_raw_df <- data.frame( rasterToPoints( mean(model_stack_gbm ) ) )
colnames(gbm_mean_raw_df) <-c("long", "lat", "Score")

# Binary Classification 

# gbm_fold1_binary
gbm_fold1_binary_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold1 > sensSpec_scores[2,2]  ) )
colnames(gbm_fold1_binary_df) <-c("long", "lat", "Score")
# gbm_fold1_binary
gbm_fold2_binary_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold2 > sensSpec_scores[2,3] ) )
colnames(gbm_fold2_binary_df) <-c("long", "lat", "Score")
# gbm_fold1_binary
gbm_fold3_binary_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold3 > sensSpec_scores[2,4] ) )
colnames(gbm_fold3_binary_df) <-c("long", "lat", "Score")
# gbm_fold1_binary
gbm_fold4_binary_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold4 > sensSpec_scores[2,5] ) )
colnames(gbm_fold4_binary_df) <-c("long", "lat", "Score")
# gbm_fold1_binary
gbm_fold5_binary_df <- data.frame( rasterToPoints( model_stack_gbm$gbmFold5 > sensSpec_scores[2,6] ) )
colnames(gbm_fold5_binary_df) <-c("long", "lat", "Score")



###  GG OBJECTS 

GBM_fold1_raw <- 
  ggplot(data = gbm_fold1_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 1 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) 

# remove df
rm( gbm_fold1_raw_df )
gc()

GBM_fold2_raw <- 
  ggplot(data = gbm_fold2_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 2 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( gbm_fold2_raw_df )
gc()

GBM_fold3_raw <- 
  ggplot(data = gbm_fold3_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 3 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( gbm_fold3_raw_df )
gc()

GBM_fold4_raw <- 
  ggplot(data = gbm_fold4_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 4 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( gbm_fold4_raw_df )
gc()

GBM_fold5_raw <- 
  ggplot(data = gbm_fold5_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 5 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( gbm_fold5_raw_df )
gc()

# binary 


GBM_fold1_binary <- 
  ggplot(data = gbm_fold1_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 1 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) #+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA) 

# remove df
rm( gbm_fold1_binary_df )
gc()

GBM_fold2_binary <- 
  ggplot(data = gbm_fold2_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 2 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) #+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( gbm_fold2_binary_df )
gc()

GBM_fold3_binary <- 
  ggplot(data = gbm_fold3_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 3 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) #+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( gbm_fold3_binary_df )
gc()

GBM_fold4_binary <- 
  ggplot(data = gbm_fold4_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 4 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) #+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( gbm_fold4_binary_df )
gc()

GBM_fold5_binary <- 
  ggplot(data = gbm_fold5_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM fold 5 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) #+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( gbm_fold5_binary_df )
gc()


###  OUTPUT 1

ggarrange(
  GBM_fold1_raw , GBM_fold1_binary,
  GBM_fold2_raw , GBM_fold2_binary, 
  GBM_fold3_raw , GBM_fold3_binary, 
  GBM_fold4_raw , GBM_fold4_binary, 
  GBM_fold5_raw , GBM_fold5_binary, 
  ncol = 2, nrow = 5
)


remove( GBM_fold1_raw , GBM_fold1_binary,
        GBM_fold2_raw , GBM_fold2_binary, 
        GBM_fold3_raw , GBM_fold3_binary, 
        GBM_fold4_raw , GBM_fold4_binary, 
        GBM_fold5_raw , GBM_fold5_binary )
gc()

# -------------------------------------------------------------------------------------------------------------------------------


# Ensembles 


gbm_mean_raw_df <- data.frame( rasterToPoints( gbm_mean ) ) 
colnames(gbm_mean_raw_df) <-c("long", "lat", "Score")

gbm_mean_binary_df <- data.frame( rasterToPoints( gbm_mean > gbm_mean_sensSpec  ) ) 
colnames(gbm_mean_binary_df) <-c("long", "lat", "Score")


gbm_sum <- sum(model_stack_gbm$gbmFold1 > sensSpec_scores[2,2], 
               model_stack_gbm$gbmFold2 > sensSpec_scores[2,3],
               model_stack_gbm$gbmFold3 > sensSpec_scores[2,4],
               model_stack_gbm$gbmFold4 > sensSpec_scores[2,5],
               model_stack_gbm$gbmFold5 > sensSpec_scores[2,6])


gbm_mv <- gbm_sum >= 3
gbm_ud <- gbm_sum == 5

gbm_mv_df <- data.frame( rasterToPoints( gbm_mv ) )
colnames( gbm_mv_df )  <-c("long", "lat", "Score")

gbm_ud_df <- data.frame( rasterToPoints( gbm_ud ) )
colnames( gbm_ud_df ) <-c("long", "lat", "Score")



GBM_mean_raw <- 
  ggplot(data = gbm_mean_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM mean - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)#+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( gbm_mean_raw_df )
gc()

GBM_mean_binary <- 
  ggplot(data = gbm_mean_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM mean - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)#+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( gbm_mean_binary_df )
gc()

# Majority Native 
GBM_mv <- 
  ggplot(data = gbm_mv_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM majority vote - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) #+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( gbm_mv_df )
gc()

# UD Native 
GBM_ud <- 
  ggplot(data = gbm_ud_df , aes(y=lat, x=long)) + 
  geom_raster( aes(fill = Score ) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("GBM unanimous decision - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) #+ 
#geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( gbm_ud_df  )
gc()



ggarrange(
  GBM_mean_raw, GBM_mean_binary,
  GBM_mv, GBM_ud,
  ncol = 2, nrow = 2
)

gc()
