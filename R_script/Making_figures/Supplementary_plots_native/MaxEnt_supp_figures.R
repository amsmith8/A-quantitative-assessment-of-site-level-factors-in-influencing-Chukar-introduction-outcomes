library("ggplot2")
library("ggpubr")
library("maptools")
library("raster")
library("RColorBrewer")

maxent_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions/model_maxent_folds", full.names= TRUE, pattern = ".tif")

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
model_stack_maxent<- listToStack(maxent_tiffs)




# http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat

states <- rgdal::readOGR("/Users/austinsmith/Downloads/ne_110m_admin_1_states_provinces/ne_110m_admin_1_states_provinces.shp") 
crs(states) <- crs(model_stack_maxent$maxentFold1)

Ac_poly <- rgdal::readOGR("/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp") # via Bird Life
crs(Ac_poly) <- crs(model_stack_maxent$maxentFold1)
#r <- rasterize(Ac.poly, Final.model, field=1)


### Seperate the polygon into native and naturalized regions
native <- subset(Ac_poly, Ac_poly$OBJECTID == 36 )  # historical  native range for A. chukar. Similar to Christensen 1970
naturalized <- subset(Ac_poly, Ac_poly$OBJECTID != 36 )

fort_native <- fortify(native)
fort_nat <- fortify(naturalized)


# import threshholds 
sensSpec_scores <- readRDS("./RDS_objects/Native/sensSpec_scores.rds")

# Compute means for averaged model 

maxent_mean <- mean(model_stack_maxent)
maxent_mean_sensSpec <- rowMeans(sensSpec_scores[3,-1])

# -------------------------------------------------------------------------------------------------------------------------------


# Create data frames for ggplots 

# Raw

# maxent_fold1_raw
maxent_fold1_raw_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold1 ) )
colnames(maxent_fold1_raw_df) <-c("long", "lat", "Score")
# maxent_fold1_raw
maxent_fold2_raw_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold2 ) )
colnames(maxent_fold2_raw_df) <-c("long", "lat", "Score")
# maxent_fold1_raw
maxent_fold3_raw_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold3 ) )
colnames(maxent_fold3_raw_df) <-c("long", "lat", "Score")
# maxent_fold1_raw
maxent_fold4_raw_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold4 ) )
colnames(maxent_fold4_raw_df) <-c("long", "lat", "Score")
# maxent_fold1_raw
maxent_fold5_raw_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold5 ) )
colnames(maxent_fold5_raw_df) <-c("long", "lat", "Score")
# AVERAGE
maxent_mean_raw_df <- data.frame( rasterToPoints( mean(model_stack_maxent ) ) )
colnames(maxent_mean_raw_df) <-c("long", "lat", "Score")

# Binary Classification 

# maxent_fold1_binary
maxent_fold1_binary_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold1 > sensSpec_scores[3,2]  ) )
colnames(maxent_fold1_binary_df) <-c("long", "lat", "Score")
# maxent_fold1_binary
maxent_fold2_binary_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold2 > sensSpec_scores[3,3] ) )
colnames(maxent_fold2_binary_df) <-c("long", "lat", "Score")
# maxent_fold1_binary
maxent_fold3_binary_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold3 > sensSpec_scores[3,4] ) )
colnames(maxent_fold3_binary_df) <-c("long", "lat", "Score")
# maxent_fold1_binary
maxent_fold4_binary_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold4 > sensSpec_scores[3,5] ) )
colnames(maxent_fold4_binary_df) <-c("long", "lat", "Score")
# maxent_fold1_binary
maxent_fold5_binary_df <- data.frame( rasterToPoints( model_stack_maxent$maxentFold5 > sensSpec_scores[3,6] ) )
colnames(maxent_fold5_binary_df) <-c("long", "lat", "Score")



###  GG OBJECTS 

MaxEnt_fold1_raw <- 
  ggplot(data = maxent_fold1_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 1 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) 

# remove df
rm( maxent_fold1_raw_df )
gc()

MaxEnt_fold2_raw <- 
  ggplot(data = maxent_fold2_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 2 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( maxent_fold2_raw_df )
gc()

MaxEnt_fold3_raw <- 
  ggplot(data = maxent_fold3_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 3 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( maxent_fold3_raw_df )
gc()

MaxEnt_fold4_raw <- 
  ggplot(data = maxent_fold4_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 4 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( maxent_fold4_raw_df )
gc()

MaxEnt_fold5_raw <- 
  ggplot(data = maxent_fold5_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 5 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( maxent_fold5_raw_df )
gc()

# binary 


MaxEnt_fold1_binary <- 
  ggplot(data = maxent_fold1_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 1 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA) 

# remove df
rm( maxent_fold1_binary_df )
gc()

MaxEnt_fold2_binary <- 
  ggplot(data = maxent_fold2_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 2 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( maxent_fold2_binary_df )
gc()

MaxEnt_fold3_binary <- 
  ggplot(data = maxent_fold3_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 3 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( maxent_fold3_binary_df )
gc()

MaxEnt_fold4_binary <- 
  ggplot(data = maxent_fold4_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 4 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( maxent_fold4_binary_df )
gc()

MaxEnt_fold5_binary <- 
  ggplot(data = maxent_fold5_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt fold 5 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( maxent_fold5_binary_df )
gc()


###  OUTPUT 1

ggarrange(
  MaxEnt_fold1_raw , MaxEnt_fold1_binary,
  MaxEnt_fold2_raw , MaxEnt_fold2_binary, 
  MaxEnt_fold3_raw , MaxEnt_fold3_binary, 
  MaxEnt_fold4_raw , MaxEnt_fold4_binary, 
  MaxEnt_fold5_raw , MaxEnt_fold5_binary, 
  ncol = 2, nrow = 5
)


remove( MaxEnt_fold1_raw , MaxEnt_fold1_binary,
        MaxEnt_fold2_raw , MaxEnt_fold2_binary, 
        MaxEnt_fold3_raw , MaxEnt_fold3_binary, 
        MaxEnt_fold4_raw , MaxEnt_fold4_binary, 
        MaxEnt_fold5_raw , MaxEnt_fold5_binary )
gc()
# -------------------------------------------------------------------------------------------------------------------------------


# Ensembles 


maxent_mean_raw_df <- data.frame( rasterToPoints( maxent_mean ) ) 
colnames(maxent_mean_raw_df) <-c("long", "lat", "Score")

maxent_mean_binary_df <- data.frame( rasterToPoints( maxent_mean > maxent_mean_sensSpec  ) ) 
colnames(maxent_mean_binary_df) <-c("long", "lat", "Score")


maxent_sum <- sum(model_stack_maxent$maxentFold1 > sensSpec_scores[3,2], 
               model_stack_maxent$maxentFold2 > sensSpec_scores[3,3],
               model_stack_maxent$maxentFold3 > sensSpec_scores[3,4],
               model_stack_maxent$maxentFold4 > sensSpec_scores[3,5],
               model_stack_maxent$maxentFold5 > sensSpec_scores[3,6])


maxent_mv <- maxent_sum >= 3
maxent_ud <- maxent_sum == 5

maxent_mv_df <- data.frame( rasterToPoints( maxent_mv ) )
colnames( maxent_mv_df )  <-c("long", "lat", "Score")

maxent_ud_df <- data.frame( rasterToPoints( maxent_ud ) )
colnames( maxent_ud_df ) <-c("long", "lat", "Score")



MaxEnt_mean_raw <- 
  ggplot(data = maxent_mean_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt mean - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( maxent_mean_raw_df )
gc()

MaxEnt_mean_binary <- 
  ggplot(data = maxent_mean_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt mean - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( maxent_mean_binary_df )
gc()

# Majority Native 
MaxEnt_mv <- 
  ggplot(data = maxent_mv_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt majority vote - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( maxent_mv_df )
gc()

# UD Native 
MaxEnt_ud <- 
  ggplot(data = maxent_ud_df , aes(y=lat, x=long)) + 
  geom_raster( aes(fill = Score ) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("MaxEnt unanimous decision - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( maxent_ud_df  )
gc()



ggarrange(
  MaxEnt_mean_raw, MaxEnt_mean_binary,
  MaxEnt_mv, MaxEnt_ud,
  ncol = 2, nrow = 2
)

gc()
