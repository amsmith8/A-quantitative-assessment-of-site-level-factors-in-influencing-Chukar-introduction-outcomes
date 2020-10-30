library("ggplot2")
library("ggpubr")
library("maptools")
library("raster")
library("RColorBrewer")

svm_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_svm_folds_CA", full.names= TRUE, pattern = ".tif")

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
model_stack_svm <- listToStack( svm_tiffs  )




# http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat

states <- rgdal::readOGR("/Users/austinsmith/Downloads/ne_110m_admin_1_states_provinces/ne_110m_admin_1_states_provinces.shp") 
crs(states) <- crs(model_stack_svm$svmFold1)

Ac_poly <- rgdal::readOGR("/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp") # via Bird Life
crs(Ac_poly) <- crs(model_stack_svm$svmFold1)
#r <- rasterize(Ac.poly, Final.model, field=1)


### Seperate the polygon into native and naturalized regions
native <- subset(Ac_poly, Ac_poly$OBJECTID == 36 )  # historical  native range for A. chukar. Similar to Christensen 1970
naturalized <- subset(Ac_poly, Ac_poly$OBJECTID != 36 )

fort_native <- fortify(native)
fort_nat <- fortify(naturalized)


# import threshholds 
sensSpec_scores <- readRDS("./RDS_objects/CA/sensSpec_scores_CA.rds")

# Compute means for averaged model 

svm_mean <- mean(model_stack_svm)
svm_mean_sensSpec <- rowMeans(sensSpec_scores[5,-1])


# -------------------------------------------------------------------------------------------------------------------------------


# Create data frames for ggplots 

# Raw

# svm_fold1_raw
svm_fold1_raw_df <- data.frame( rasterToPoints( model_stack_svm$svmFold1 ) )
colnames(svm_fold1_raw_df) <-c("long", "lat", "Score")
# svm_fold1_raw
svm_fold2_raw_df <- data.frame( rasterToPoints( model_stack_svm$svmFold2 ) )
colnames(svm_fold2_raw_df) <-c("long", "lat", "Score")
# svm_fold1_raw
svm_fold3_raw_df <- data.frame( rasterToPoints( model_stack_svm$svmFold3 ) )
colnames(svm_fold3_raw_df) <-c("long", "lat", "Score")
# svm_fold1_raw
svm_fold4_raw_df <- data.frame( rasterToPoints( model_stack_svm$svmFold4 ) )
colnames(svm_fold4_raw_df) <-c("long", "lat", "Score")
# svm_fold1_raw
svm_fold5_raw_df <- data.frame( rasterToPoints( model_stack_svm$svmFold5 ) )
colnames(svm_fold5_raw_df) <-c("long", "lat", "Score")
# AVERAGE
svm_mean_raw_df <- data.frame( rasterToPoints( mean(model_stack_svm ) ) )
colnames(svm_mean_raw_df) <-c("long", "lat", "Score")

# Binary Classification 

# svm_fold1_binary
svm_fold1_binary_df <- data.frame( rasterToPoints( model_stack_svm$svmFold1 > sensSpec_scores[5,2]  ) )
colnames(svm_fold1_binary_df) <-c("long", "lat", "Score")
# svm_fold1_binary
svm_fold2_binary_df <- data.frame( rasterToPoints( model_stack_svm$svmFold2 > sensSpec_scores[5,3] ) )
colnames(svm_fold2_binary_df) <-c("long", "lat", "Score")
# svm_fold1_binary
svm_fold3_binary_df <- data.frame( rasterToPoints( model_stack_svm$svmFold3 > sensSpec_scores[5,4] ) )
colnames(svm_fold3_binary_df) <-c("long", "lat", "Score")
# svm_fold1_binary
svm_fold4_binary_df <- data.frame( rasterToPoints( model_stack_svm$svmFold4 > sensSpec_scores[5,5] ) )
colnames(svm_fold4_binary_df) <-c("long", "lat", "Score")
# svm_fold1_binary
svm_fold5_binary_df <- data.frame( rasterToPoints( model_stack_svm$svmFold5 > sensSpec_scores[5,6] ) )
colnames(svm_fold5_binary_df) <-c("long", "lat", "Score")



###  GG OBJECTS 

SVM_fold1_raw <- 
  ggplot(data = svm_fold1_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 1 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) 

# remove df
rm( svm_fold1_raw_df )
gc()

SVM_fold2_raw <- 
  ggplot(data = svm_fold2_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 2 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( svm_fold2_raw_df )
gc()

SVM_fold3_raw <- 
  ggplot(data = svm_fold3_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 3 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( svm_fold3_raw_df )
gc()

SVM_fold4_raw <- 
  ggplot(data = svm_fold4_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 4 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( svm_fold4_raw_df )
gc()

SVM_fold5_raw <- 
  ggplot(data = svm_fold5_raw_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 5 - California points (raw values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)

# remove df
rm( svm_fold5_raw_df )
gc()

# binary 


SVM_fold1_binary <- 
  ggplot(data = svm_fold1_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 1 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA) 

# remove df
rm( svm_fold1_binary_df )
gc()

SVM_fold2_binary <- 
  ggplot(data = svm_fold2_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 2 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( svm_fold2_binary_df )
gc()

SVM_fold3_binary <- 
  ggplot(data = svm_fold3_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 3 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( svm_fold3_binary_df )
gc()

SVM_fold4_binary <- 
  ggplot(data = svm_fold4_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 4 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( svm_fold4_binary_df )
gc()

SVM_fold5_binary <- 
  ggplot(data = svm_fold5_binary_df, aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM fold 5 - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( svm_fold5_binary_df )
gc()


###  OUTPUT 1

ggarrange(
  SVM_fold1_raw , SVM_fold1_binary,
  SVM_fold2_raw , SVM_fold2_binary, 
  SVM_fold3_raw , SVM_fold3_binary, 
  SVM_fold4_raw , SVM_fold4_binary, 
  SVM_fold5_raw , SVM_fold5_binary, 
  ncol = 2, nrow = 5
)


remove( SVM_fold1_raw , SVM_fold1_binary,
        SVM_fold2_raw , SVM_fold2_binary, 
        SVM_fold3_raw , SVM_fold3_binary, 
        SVM_fold4_raw , SVM_fold4_binary, 
        SVM_fold5_raw , SVM_fold5_binary )
gc()


# -------------------------------------------------------------------------------------------------------------------------------


# Ensembles 


svm_mean_raw_df <- data.frame( rasterToPoints( svm_mean ) ) 
colnames(svm_mean_raw_df) <-c("long", "lat", "Score")

svm_mean_binary_df <- data.frame( rasterToPoints( svm_mean > svm_mean_sensSpec  ) ) 
colnames(svm_mean_binary_df) <-c("long", "lat", "Score")


svm_sum <- sum(model_stack_svm$svmFold1 > sensSpec_scores[5,2], 
               model_stack_svm$svmFold2 > sensSpec_scores[5,3],
               model_stack_svm$svmFold3 > sensSpec_scores[5,4],
               model_stack_svm$svmFold4 > sensSpec_scores[5,5],
               model_stack_svm$svmFold5 > sensSpec_scores[5,6])


svm_mv <- svm_sum >= 3
svm_ud <- svm_sum == 5

svm_mv_df <- data.frame( rasterToPoints( svm_mv ) )
colnames( svm_mv_df )  <-c("long", "lat", "Score")

svm_ud_df <- data.frame( rasterToPoints( svm_ud ) )
colnames( svm_ud_df ) <-c("long", "lat", "Score")



SVM_mean_raw <- 
  ggplot(data = svm_mean_raw_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM mean - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( svm_mean_raw_df )
gc()

SVM_mean_binary <- 
  ggplot(data = svm_mean_binary_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM mean - California points (binary values)") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)+ 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

# remove df
rm( svm_mean_binary_df )
gc()

# Majority California 
SVM_mv <- 
  ggplot(data = svm_mv_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Score, fill = Score) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM majority vote - California points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( svm_mv_df )
gc()

# UD California 
SVM_ud <- 
  ggplot(data = svm_ud_df , aes(y=lat, x=long)) + 
  geom_raster( aes(fill = Score ) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("SVM unanimous decision - California points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

rm( svm_ud_df  )
gc()



ggarrange(
  SVM_mean_raw, SVM_mean_binary,
  SVM_mv, SVM_ud,
  ncol = 2, nrow = 2
)

gc()
