library("ggplot2")
library("ggpubr")
library("maptools")
library("raster")
library("RColorBrewer")

rf_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_rf_folds_CA", full.names= TRUE, pattern = ".tif")

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
model_stack_rf<- listToStack(rf_tiffs)




# http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat

states <- rgdal::readOGR("/Users/austinsmith/Downloads/ne_110m_admin_1_states_provinces/ne_110m_admin_1_states_provinces.shp") 
crs(states) <- crs(model_stack_rf$rfFold1)

Ac_poly <- rgdal::readOGR("/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp") # via Bird Life
crs(Ac_poly) <- crs(model_stack_rf$rfFold1)
#r <- rasterize(Ac.poly, Final.model, field=1)


### Seperate the polygon into native and naturalized regions
native <- subset(Ac_poly, Ac_poly$OBJECTID == 36 )  # historical  native range for A. chukar. Similar to Christensen 1970
naturalized <- subset(Ac_poly, Ac_poly$OBJECTID != 36 )

fort_native <- fortify(native)
fort_nat <- fortify(naturalized)


# import threshholds 
sensSpec_scores <- readRDS("./RDS_objects/CA/sensSpec_scores_CA.rds")

# Compute means for averaged model 

rf_mean <- mean(model_stack_rf)
rf_mean_sensSpec <- rowMeans(sensSpec_scores[4,-1])

# -------------------------------------------------------------------------------------------------------------------------------


# Create data frames for ggplots 

# Raw

# rf_fold1_raw
rf_fold1_raw_df <- data.frame( rasterToPoints( model_stack_rf$rfFold1 ) )
colnames(rf_fold1_raw_df) <-c("long", "lat", "Score")
# rf_fold1_raw
rf_fold2_raw_df <- data.frame( rasterToPoints( model_stack_rf$rfFold2 ) )
colnames(rf_fold2_raw_df) <-c("long", "lat", "Score")
# rf_fold1_raw
rf_fold3_raw_df <- data.frame( rasterToPoints( model_stack_rf$rfFold3 ) )
colnames(rf_fold3_raw_df) <-c("long", "lat", "Score")
# rf_fold1_raw
rf_fold4_raw_df <- data.frame( rasterToPoints( model_stack_rf$rfFold4 ) )
colnames(rf_fold4_raw_df) <-c("long", "lat", "Score")
# rf_fold1_raw
rf_fold5_raw_df <- data.frame( rasterToPoints( model_stack_rf$rfFold5 ) )
colnames(rf_fold5_raw_df) <-c("long", "lat", "Score")
# AVERAGE
rf_mean_raw_df <- data.frame( rasterToPoints( mean(model_stack_rf ) ) )
colnames(rf_mean_raw_df) <-c("long", "lat", "Score")

# Binary Classification 

# rf_fold1_binary
rf_fold1_binary_df <- data.frame( rasterToPoints( model_stack_rf$rfFold1 > sensSpec_scores[4,2]  ) )
colnames(rf_fold1_binary_df) <-c("long", "lat", "Score")
# rf_fold1_binary
rf_fold2_binary_df <- data.frame( rasterToPoints( model_stack_rf$rfFold2 > sensSpec_scores[4,3] ) )
colnames(rf_fold2_binary_df) <-c("long", "lat", "Score")
# rf_fold1_binary
rf_fold3_binary_df <- data.frame( rasterToPoints( model_stack_rf$rfFold3 > sensSpec_scores[4,4] ) )
colnames(rf_fold3_binary_df) <-c("long", "lat", "Score")
# rf_fold1_binary
rf_fold4_binary_df <- data.frame( rasterToPoints( model_stack_rf$rfFold4 > sensSpec_scores[4,5] ) )
colnames(rf_fold4_binary_df) <-c("long", "lat", "Score")
# rf_fold1_binary
rf_fold5_binary_df <- data.frame( rasterToPoints( model_stack_rf$rfFold5 > sensSpec_scores[4,6] ) )
colnames(rf_fold5_binary_df) <-c("long", "lat", "Score")



###  GG OBJECTS 

RF_fold1_raw <- 
  ggplot(data = rf_fold1_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 1 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) 

# remove df
rm( rf_fold1_raw_df )
gc()

RF_fold2_raw <- 
  ggplot(data = rf_fold2_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 2 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( rf_fold2_raw_df )
gc()

RF_fold3_raw <- 
  ggplot(data = rf_fold3_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 3 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( rf_fold3_raw_df )
gc()

RF_fold4_raw <- 
  ggplot(data = rf_fold4_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 4 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( rf_fold4_raw_df )
gc()

RF_fold5_raw <- 
  ggplot(data = rf_fold5_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 5 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( rf_fold5_raw_df )
gc()

# binary 


RF_fold1_binary <- 
  ggplot(data = rf_fold1_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 1 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA) 

# remove df
rm( rf_fold1_binary_df )
gc()

RF_fold2_binary <- 
  ggplot(data = rf_fold2_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 2 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( rf_fold2_binary_df )
gc()

RF_fold3_binary <- 
  ggplot(data = rf_fold3_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 3 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( rf_fold3_binary_df )
gc()

RF_fold4_binary <- 
  ggplot(data = rf_fold4_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 4 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( rf_fold4_binary_df )
gc()

RF_fold5_binary <- 
  ggplot(data = rf_fold5_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF fold 5 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( rf_fold5_binary_df )
gc()


###  OUTPUT 1

ggarrange(
  RF_fold1_raw , RF_fold1_binary,
  RF_fold2_raw , RF_fold2_binary, 
  RF_fold3_raw , RF_fold3_binary, 
  RF_fold4_raw , RF_fold4_binary, 
  RF_fold5_raw , RF_fold5_binary, 
  ncol = 2, nrow = 5
)


remove( RF_fold1_raw , RF_fold1_binary,
        RF_fold2_raw , RF_fold2_binary, 
        RF_fold3_raw , RF_fold3_binary, 
        RF_fold4_raw , RF_fold4_binary, 
        RF_fold5_raw , RF_fold5_binary )
gc()


# -------------------------------------------------------------------------------------------------------------------------------


# Ensembles 


rf_mean_raw_df <- data.frame( rasterToPoints( rf_mean ) ) 
colnames(rf_mean_raw_df) <-c("long", "lat", "Score")

rf_mean_binary_df <- data.frame( rasterToPoints( rf_mean > rf_mean_sensSpec  ) ) 
colnames(rf_mean_binary_df) <-c("long", "lat", "Score")


rf_sum <- sum(model_stack_rf$rfFold1 > sensSpec_scores[4,2], 
              model_stack_rf$rfFold2 > sensSpec_scores[4,3],
              model_stack_rf$rfFold3 > sensSpec_scores[4,4],
              model_stack_rf$rfFold4 > sensSpec_scores[4,5],
              model_stack_rf$rfFold5 > sensSpec_scores[4,6])


rf_mv <- rf_sum >= 3
rf_ud <- rf_sum == 5

rf_mv_df <- data.frame( rasterToPoints( rf_mv ) )
colnames( rf_mv_df )  <-c("long", "lat", "Score")

rf_ud_df <- data.frame( rasterToPoints( rf_ud ) )
colnames( rf_ud_df ) <-c("long", "lat", "Score")



RF_mean_raw <- 
  ggplot(data = rf_mean_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF mean - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( rf_mean_raw_df )
gc()

RF_mean_binary <- 
  ggplot(data = rf_mean_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF mean - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( rf_mean_binary_df )
gc()

# Majority California 
RF_mv <- 
  ggplot(data = rf_mv_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF majority vote - California points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( rf_mv_df )
gc()

# UD California 
RF_ud <- 
  ggplot(data = rf_ud_df , aes(y=lat, x=long)) + 
  geom_raster( aes(fill = Score ) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("RF unanimous decision - California points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( rf_ud_df  )
gc()



ggarrange(
  RF_mean_raw, RF_mean_binary,
  RF_mv, RF_ud,
  ncol = 2, nrow = 2
)

gc()
