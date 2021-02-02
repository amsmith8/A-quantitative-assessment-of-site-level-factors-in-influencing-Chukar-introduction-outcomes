# eBird data cleaning & subsetting


# DATA CLEANING


# import eBird data
ebird <- read.csv( "/Users/austinsmith/Downloads/ML_2020-11-30T09-41_Alectoris_chukar_Photo.csv" ) # as of: November 30, 2020

# determine if there are duplicate points 
dups <- duplicated(ebird [, c('Latitude', 'Longitude')])
sum(dups)

# remove duplicates 
ebird <- ebird[!dups,] # 1302 


# DATA SUBSETTING


library(dplyr)

# native range
native_pts <- ebird %>% 
  filter(Country != "United States") %>%
  filter(Country != "Canada") %>%
  filter(Country != "New Zealand") %>%
  filter(Country != "South Africa") %>%
  filter(Country != "Australia")
native_pts <- native_pts[,18:17]
colnames(native_pts) <- c("lon","lat")
native_pts$group <- "native"

# naturalized range
naturalized_countries <- c("United States", "Canada","New Zealand", "South Africa", "Australia" )
naturalized_pts <- filter(ebird, Country %in% naturalized_countries)
naturalized_pts <- naturalized_pts[,18:17]
colnames(naturalized_pts) <- c("lon","lat") 
naturalized_pts$group <- "naturalized"

  
# contiguous US 
us_pts <- ebird %>%
  filter(Country == "United States") %>%
  filter(State != "Hawaii")
us_pts <- us_pts[,18:17]
colnames(us_pts) <- c("lon","lat")

# California points 
CA_pts <-  ebird %>%
  filter(Country == "United States") %>%
  filter(State == "California")
CA_pts <- CA_pts[,18:17]
colnames(CA_pts) <- c("lon","lat")


# CALCULATE BUFFER SIZE 


# Measured in New Mexico study ( Bohl 1957) 
bohl <- c( 10, 11, 22, 38, 15, 20) # miles airline
# Convert Bohl measuremnts to match other studies matched 
bohl <- 1.15 * bohl # "land" miles 
# All recorded distances
bohl <- c( bohl, 
           22 , 42, 48, 52, # Galbreath & Moreland 1950
           17, 21, # Christensen 1954
           33, # CA ?
           60 # Williamson 1950 NZ
)
# create rough meaure 
d <- round( mean( bohl) * 1.609344 ) * 1000  # miles to kilometers ---> meters for 'circles' function 


# PLOTS


# library(dismo)
# library(maptools)
# library(raster)
# 
# 
# # background map
# data( "wrld_simpl" )
# 
# Ac_poly <- rgdal::readOGR("/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp") # via Bird Life
# crs(Ac_poly) <- crs(wrld_simpl)
# 
# 
# # http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
# top = 49.3457868 # north lat
# left = -124.7844079 # west long
# right = -66.9513812 # east long
# bottom =  24.7433195 # south lat
# 
# 
# 
# plot(wrld_simpl, xlim = c(left,right), ylim = c(bottom, top) )
# points(ebird, col = "red")
# 
# x <- circles( us_pts , d = d, dissolve=TRUE , lonlat=TRUE )
# # Loading required namespace: rgeos
# pol <- polygons(x)
# 
# plot( wrld_simpl , main = "World distribution", xlim = c(left,right), ylim = c(bottom, top))
# plot(pol, add = T, col = "blue")
# plot(Ac_poly, , add = T, border = "red")
# 
# 
# plot(wrld_simpl, main = "cont. US distribution", xlim = c(left,right), ylim = c(bottom, top) )
# #points(ebird, col = "red")
# plot(pol, add = T, col = "gray")
# plot(Ac_poly, , add = T, border = "red")


