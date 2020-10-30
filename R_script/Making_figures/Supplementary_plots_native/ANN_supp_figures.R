library("ggplot2")
library("ggpubr")
library("maptools")
library("raster")
library("RColorBrewer")

ann_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions/model_ann_folds", full.names= TRUE, pattern = ".tif")

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
model_stack_ann<- listToStack(ann_tiffs)




# http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat

states <- rgdal::readOGR("/Users/austinsmith/Downloads/ne_110m_admin_1_states_provinces/ne_110m_admin_1_states_provinces.shp") 
crs(states) <- crs(model_stack_ann$annFold1)

Ac_poly <- rgdal::readOGR("/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp") # via Bird Life
crs(Ac_poly) <- crs(model_stack_ann$annFold1)
#r <- rasterize(Ac.poly, Final.model, field=1)


### Seperate the polygon into native and naturalized regions
native <- subset(Ac_poly, Ac_poly$OBJECTID == 36 )  # historical  native range for A. chukar. Similar to Christensen 1970
naturalized <- subset(Ac_poly, Ac_poly$OBJECTID != 36 )

fort_native <- fortify(native)
fort_nat <- fortify(naturalized)


# import threshholds 
sensSpec_scores <- readRDS("./RDS_objects/Native/sensSpec_scores.rds")

# Compute means for averaged model 

ann_mean <- mean(model_stack_ann)
ann_mean_sensSpec <- rowMeans(sensSpec_scores[1,-1])


# -------------------------------------------------------------------------------------------------------------------------------


# Create data frames for ggplots 

# Raw

# ann_fold1_raw
ann_fold1_raw_df <- data.frame( rasterToPoints( model_stack_ann$annFold1 ) )
colnames(ann_fold1_raw_df) <-c("long", "lat", "Score")
# ann_fold1_raw
ann_fold2_raw_df <- data.frame( rasterToPoints( model_stack_ann$annFold2 ) )
colnames(ann_fold2_raw_df) <-c("long", "lat", "Score")
# ann_fold1_raw
ann_fold3_raw_df <- data.frame( rasterToPoints( model_stack_ann$annFold3 ) )
colnames(ann_fold3_raw_df) <-c("long", "lat", "Score")
# ann_fold1_raw
ann_fold4_raw_df <- data.frame( rasterToPoints( model_stack_ann$annFold4 ) )
colnames(ann_fold4_raw_df) <-c("long", "lat", "Score")
# ann_fold1_raw
ann_fold5_raw_df <- data.frame( rasterToPoints( model_stack_ann$annFold5 ) )
colnames(ann_fold5_raw_df) <-c("long", "lat", "Score")
# AVERAGE
ann_mean_raw_df <- data.frame( rasterToPoints( mean(model_stack_ann ) ) )
colnames(ann_mean_raw_df) <-c("long", "lat", "Score")

# Binary Classification 

# ann_fold1_binary
ann_fold1_binary_df <- data.frame( rasterToPoints( model_stack_ann$annFold1 > sensSpec_scores[1,2]  ) )
colnames(ann_fold1_binary_df) <-c("long", "lat", "Score")
# ann_fold1_binary
ann_fold2_binary_df <- data.frame( rasterToPoints( model_stack_ann$annFold2 > sensSpec_scores[1,3] ) )
colnames(ann_fold2_binary_df) <-c("long", "lat", "Score")
# ann_fold1_binary
ann_fold3_binary_df <- data.frame( rasterToPoints( model_stack_ann$annFold3 > sensSpec_scores[1,4] ) )
colnames(ann_fold3_binary_df) <-c("long", "lat", "Score")
# ann_fold1_binary
ann_fold4_binary_df <- data.frame( rasterToPoints( model_stack_ann$annFold4 > sensSpec_scores[1,5] ) )
colnames(ann_fold4_binary_df) <-c("long", "lat", "Score")
# ann_fold1_binary
ann_fold5_binary_df <- data.frame( rasterToPoints( model_stack_ann$annFold5 > sensSpec_scores[1,6] ) )
colnames(ann_fold5_binary_df) <-c("long", "lat", "Score")



###  GG OBJECTS 

ANN_fold1_raw <- 
  ggplot(data = ann_fold1_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 1 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) 

# remove df
rm( ann_fold1_raw_df )


ANN_fold2_raw <- 
  ggplot(data = ann_fold2_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 2 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( ann_fold2_raw_df )

ANN_fold3_raw <- 
  ggplot(data = ann_fold3_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 3 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( ann_fold3_raw_df )

ANN_fold4_raw <- 
  ggplot(data = ann_fold4_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 4 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( ann_fold4_raw_df )

ANN_fold5_raw <- 
  ggplot(data = ann_fold5_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 5 - native points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( ann_fold5_raw_df )


# binary 


ANN_fold1_binary <- 
  ggplot(data = ann_fold1_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 1 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA) 

# remove df
rm( ann_fold1_binary_df )

ANN_fold2_binary <- 
  ggplot(data = ann_fold2_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 2 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( ann_fold2_binary_df )

ANN_fold3_binary <- 
  ggplot(data = ann_fold3_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 3 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( ann_fold3_binary_df )

ANN_fold4_binary <- 
  ggplot(data = ann_fold4_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 4 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( ann_fold4_binary_df )

ANN_fold5_binary <- 
  ggplot(data = ann_fold5_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN fold 5 - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( ann_fold5_binary_df )



###  OUTPUT 1

ggarrange(
  ANN_fold1_raw , ANN_fold1_binary,
  ANN_fold2_raw , ANN_fold2_binary, 
  ANN_fold3_raw , ANN_fold3_binary, 
  ANN_fold4_raw , ANN_fold4_binary, 
  ANN_fold5_raw , ANN_fold5_binary, 
  ncol = 2, nrow = 5
)


remove( ANN_fold1_raw , ANN_fold1_binary,
        ANN_fold2_raw , ANN_fold2_binary, 
        ANN_fold3_raw , ANN_fold3_binary, 
        ANN_fold4_raw , ANN_fold4_binary, 
        ANN_fold5_raw , ANN_fold5_binary )
gc()


 # -------------------------------------------------------------------------------------------------------------------------------


# Ensembles 


ann_mean_raw_df <- data.frame( rasterToPoints( mean(model_stack_ann ) ) )
colnames(ann_mean_raw_df) <-c("long", "lat", "Score")

ann_mean_binary_df <- data.frame( rasterToPoints( mean(model_stack_ann ) > rowMeans( sensSpec_scores[1,-1] ) ) )
colnames(ann_mean_binary_df) <-c("long", "lat", "Score")


ann_sum <- sum(model_stack_ann$annFold1 > sensSpec_scores[1,2], 
               model_stack_ann$annFold2 > sensSpec_scores[1,3],
               model_stack_ann$annFold3 > sensSpec_scores[1,4],
               model_stack_ann$annFold4 > sensSpec_scores[1,5],
               model_stack_ann$annFold5 > sensSpec_scores[1,6])


ann_mv <- ann_sum >= 3
ann_ud <- ann_sum == 5

ann_mv_df <- data.frame( rasterToPoints( ann_mv ) )
colnames( ann_mv_df )  <-c("long", "lat", "Score")

ann_ud_df <- data.frame( rasterToPoints( ann_ud ) )
colnames( ann_ud_df ) <-c("long", "lat", "Score")



ANN_mean_raw <- 
  ggplot(data = ann_mean_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN mean - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( ann_mean_raw_df )

ANN_mean_binary <- 
  ggplot(data = ann_mean_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN mean - native points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( ann_mean_binary_df )

# Majority Native 
ANN_mv <- 
  ggplot(data = ann_mv_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN majority vote - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( ann_mv_df )

# UD Native 
ANN_ud <- 
  ggplot(data = ann_ud_df , aes(y=lat, x=long)) + 
  geom_raster( aes(fill = Score ) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("ANN unanimous decision - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)




ggarrange(
  ANN_mean_raw, ANN_mean_binary,
  ANN_mv, ANN_ud,
  ncol = 2, nrow = 2
)

gc()
