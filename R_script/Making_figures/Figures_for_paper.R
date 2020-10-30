library("ggplot2")
library("ggpubr")
library("maptools")
library("raster")
library("RColorBrewer")





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





library( "rnaturalearth" )
world_land <- ne_countries( returnclass = c( "sp", "sf" ) )

# Remove bioclimatic extremes
antarctica <- ne_countries( continent = 'antarctica' )
greenland <- ne_countries( country = 'greenland' )

world_land <- world_land - antarctica - greenland
#crs( world_land ) <- Model_CRS
wl <-fortify(world_land )

# ggplot() +   
#   theme( panel.background = element_rect( fill = "lightblue",
#                                                      colour = "lightblue",
#                                                      size = 0.5, linetype = "solid")) +
#   geom_polygon(aes(x = long, y = lat, group=group), data = fort_native, colour="black", fill="white") +
#   geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="black", fill="blue") +
#   geom_polygon(aes(x = long, y = lat, group=group), data = wl, colour="black", fill="red") 

par(mfrow = c(1,1))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(world_land, main = "Chukar world distribution")
plot(native, add = TRUE , col = "blue")
plot(naturalized, add = TRUE, col = "red")
legend("bottom", legend=c("Native ", "Naturalized"),bty = "n", # turn off legend border
       col=c( "blue", "red"), pch = 15, cex=0.8)


kmeans <- readRDS("/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./RDS_objects/Native/k_means_clusters.rds")
chukar_present_pts <- readRDS("/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./RDS_objects/Native/chukar_present_pts.rds")

occ_grp <- kmeans$cluster

plot(native, col ="gray", main = "Native range spatial bins" )
points(chukar_present_pts[,1:2], pch=21, bg=occ_grp)
plot(world_land, add = T)
#legend("bottom", inset=.02, box.col="white",
#       c("Fold 4","Fold 6","Fold 8","Fold 8","Fold 8"), fill=occ_grp, horiz=TRUE, cex=0.8)

CA <- states[states$name == "California",]
CA_chukar_area <- intersect(CA, Ac_poly)

plot( states, xlim = c( left, right ), ylim =c( bottom , top ), main = " US naturalized range" )
plot(naturalized, add = TRUE, col = "lightblue1")
plot(CA_chukar_area, add = TRUE, col = "red")
legend("bottom", legend=c("CA subspecies range"), bty = "n", # turn off legend border
       col=c( "red"), pch = 15, cex=0.8)



######## Figure 2



# import threshoolds
auc_scores <- readRDS("./RDS_objects/Native/auc_scores.rds")
sensSpec_scores <- readRDS("./RDS_objects/Native/sensSpec_scores.rds")
all_cm_results <- readRDS( "./RDS_objects/Native/all_cm_results.rds" )


# Reconfigure for plotting
all_cm_gglist <- reshape2::melt( all_cm_results , id.var='Model' )
all_cm_gglist$Stage <- "Predicition"

# Reconfigure the auc matrix to match the spatial statistics
auc_melt <- reshape2::melt( auc_scores , id.var = 'model' )
colnames( auc_melt )[1] <- "Model"
auc_melt[ , 2 ] <- "AUC"
auc_melt$Stage <- "Evaluation"

# All data 
final_table <- rbind( auc_melt , all_cm_gglist )
library(ggpubr)
library("RColorBrewer")
plot1 <- ggbarplot(final_table, x = "variable", y = "value", ,position = position_dodge(0.8),
                   #title = "Model statistics - native points",
                   xlab = "",
                   ylab = "Score",
                   fill = "Model",
                   palette  = brewer.pal(n = 5, name = "Blues"),
                   legend = "bottom",
                   facet.by = "Stage",
                   add = c("mean_sd"),
                   rremove("x.text")
)
A <- plot1 +  font("legend.title",  face = "bold")
A <- A +   theme(plot.title = element_text(hjust = 0.5) ) + facet_grid(~Stage, space = "free_x", scales = "free_x") 
A

# CA 


# import threshoolds
auc_scores_CA <- readRDS("./RDS_objects/CA/auc_scores_CA.rds")
sensSpec_scores_CA <- readRDS("./RDS_objects/CA/sensSpec_scores_CA.rds")
all_cm_results_CA <- readRDS( "./RDS_objects/CA/all_cm_results_CA.rds" )


# Reconfigure for plotting
all_cm_gglist_CA <- reshape2::melt( all_cm_results_CA , id.var='Model' )
all_cm_gglist_CA$Stage <- "Predicition"

# Reconfigure the auc matrix to match the spatial statistics
auc_melt_CA <- reshape2::melt( auc_scores_CA , id.var = 'model' )
colnames( auc_melt_CA )[1] <- "Model"
auc_melt_CA[ , 2 ] <- "AUC"
auc_melt_CA$Stage <- "Evaluation"

# All data 
final_table_CA <- rbind( auc_melt_CA , all_cm_gglist_CA )
library(ggpubr)
library("RColorBrewer")
plot2 <- ggbarplot(final_table_CA, x = "variable", y = "value", ,position = position_dodge(0.8),
                   #title = "Model statistics - native points",
                   xlab = "",
                   ylab = "Score",
                   fill = "Model",
                   palette  = brewer.pal(n = 5, name = "Oranges"),
                   legend = "bottom",
                   facet.by = "Stage",
                   add = c("mean_sd"),
                   rremove("x.text")
)
B <- plot2 +  font("legend.title",  face = "bold")
B <- B +   theme(plot.title = element_text(hjust = 0.5) ) + facet_grid(~Stage, space = "free_x", scales = "free_x")
B

GG <- ggarrange(A+ rremove("legend"), B, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
GG 




##### FIGURE 3 



# Native 

ann_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions/model_ann_folds", full.names= TRUE, pattern = ".tif")
gbm_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions/model_gbm_folds", full.names= TRUE, pattern = ".tif")
maxent_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions/model_maxent_folds", full.names= TRUE, pattern = ".tif")
rf_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions/model_rf_folds", full.names= TRUE, pattern = ".tif")
svm_tiffs <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions/model_svm_folds", full.names= TRUE, pattern = ".tif")

# Function 1
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
model_stack_gbm <- listToStack(gbm_tiffs)
model_stack_maxent <- listToStack(maxent_tiffs)
model_stack_rf <- listToStack(rf_tiffs)
model_stack_svm <- listToStack(svm_tiffs)


# CA 


# Stored folders of tiff files

ann_tiffs_CA <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_ann_folds_CA", full.names= TRUE, pattern = ".tif")
gbm_tiffs_CA <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_gbm_folds_CA", full.names= TRUE, pattern = ".tif")
maxent_tiffs_CA <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_maxent_folds_CA", full.names= TRUE, pattern = ".tif")
rf_tiffs_CA <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_rf_folds_CA", full.names= TRUE, pattern = ".tif")
svm_tiffs_CA <- list.files(path = "/Users/austinsmith/Documents/GitHub/A-quantitative-assessment-of-site-level-factors-in-influencing-Chukar-introduction-outcomes./Predictions_CA/model_svm_folds_CA", full.names= TRUE, pattern = ".tif")



#compile tiffs to stacks 
model_stack_ann_CA <- listToStack(ann_tiffs_CA)
model_stack_gbm_CA <- listToStack(gbm_tiffs_CA)
model_stack_maxent_CA <- listToStack(maxent_tiffs_CA)
model_stack_rf_CA <- listToStack(rf_tiffs_CA)
model_stack_svm_CA <- listToStack(svm_tiffs_CA)




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

fort_nat <- fortify(naturalized)



# Native


ann_sum <- sum(model_stack_ann$annFold1 > sensSpec_scores[1,2], 
               model_stack_ann$annFold2 > sensSpec_scores[1,3],
               model_stack_ann$annFold3 > sensSpec_scores[1,4],
               model_stack_ann$annFold4 > sensSpec_scores[1,5],
               model_stack_ann$annFold5 > sensSpec_scores[1,6])

gbm_sum <- sum(model_stack_gbm$gbmFold1 > sensSpec_scores[2,2], 
               model_stack_gbm$gbmFold2 > sensSpec_scores[2,3],
               model_stack_gbm$gbmFold3 > sensSpec_scores[2,4],
               model_stack_gbm$gbmFold4 > sensSpec_scores[2,5],
               model_stack_gbm$gbmFold5 > sensSpec_scores[2,6])

maxent_sum <- sum(model_stack_maxent$maxentFold1 > sensSpec_scores[3,2], 
               model_stack_maxent$maxentFold2 > sensSpec_scores[3,3],
               model_stack_maxent$maxentFold3 > sensSpec_scores[3,4],
               model_stack_maxent$maxentFold4 > sensSpec_scores[3,5],
               model_stack_maxent$maxentFold5 > sensSpec_scores[3,6])

rf_sum <- sum(model_stack_rf$rfFold1 > sensSpec_scores[4,2], 
               model_stack_rf$rfFold2 > sensSpec_scores[4,3],
               model_stack_rf$rfFold3 > sensSpec_scores[4,4],
               model_stack_rf$rfFold4 > sensSpec_scores[4,5],
               model_stack_rf$rfFold5 > sensSpec_scores[4,6])

svm_sum <- sum(model_stack_svm$svmFold1 > sensSpec_scores[5,2], 
               model_stack_svm$svmFold2 > sensSpec_scores[5,3],
               model_stack_svm$svmFold3 > sensSpec_scores[5,4],
               model_stack_svm$svmFold4 > sensSpec_scores[5,5],
               model_stack_svm$svmFold5 > sensSpec_scores[5,6])


all_folds <- sum( ann_sum , gbm_sum , maxent_sum , rf_sum , svm_sum )
all_folds_13 <- all_folds >= 13
all_folds_25 <- all_folds == 25


ensemble_native_df <- data.frame( rasterToPoints( all_folds ) )
colnames(ensemble_native_df) <-c("long", "lat", "Votes")


majority_native_df <- data.frame( rasterToPoints( all_folds_13 ) )
colnames(majority_native_df)  <-c("long", "lat", "Votes")

ud_native_df <- data.frame( rasterToPoints( all_folds_25 ) )
colnames(ud_native_df) <-c("long", "lat", "Votes")


# 
all_votes_native <- 
  ggplot(data =ensemble_native_df , aes(y=lat, x=long)) + 
  geom_raster(aes(fill = Votes )) +
  coord_cartesian(xlim = c( left, right ), ylim =c( bottom , top )) +
  ggtitle("All model election - native points") +
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)



# Majority Native 
majority_votes_native <- 
  ggplot(data = majority_native_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Votes, fill = Votes) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("Majority vote - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)

 

# UD Native 
ud_votes_native <- 
  ggplot(data = ud_native_df , aes(y=lat, x=long)) + 
  geom_raster( aes(fill = Votes ) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("Unanimous decision - native points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)




# California
ann_sum_CA <- sum(model_stack_ann_CA$annFold1 > sensSpec_scores_CA[1,2], 
                  model_stack_ann_CA$annFold2 > sensSpec_scores_CA[1,3],
                  model_stack_ann_CA$annFold3 > sensSpec_scores_CA[1,4],
                  model_stack_ann_CA$annFold4 > sensSpec_scores_CA[1,5],
                  model_stack_ann_CA$annFold5 > sensSpec_scores_CA[1,6])

gbm_sum_CA <- sum(model_stack_gbm_CA$gbmFold1 > sensSpec_scores_CA[2,2], 
                  model_stack_gbm_CA$gbmFold2 > sensSpec_scores_CA[2,3],
                  model_stack_gbm_CA$gbmFold3 > sensSpec_scores_CA[2,4],
                  model_stack_gbm_CA$gbmFold4 > sensSpec_scores_CA[2,5],
                  model_stack_gbm_CA$gbmFold5 > sensSpec_scores_CA[2,6])

maxent_sum_CA <- sum(model_stack_maxent_CA$maxentFold1 > sensSpec_scores_CA[3,2], 
                  model_stack_maxent_CA$maxentFold2 > sensSpec_scores_CA[3,3],
                  model_stack_maxent_CA$maxentFold3 > sensSpec_scores_CA[3,4],
                  model_stack_maxent_CA$maxentFold4 > sensSpec_scores_CA[3,5],
                  model_stack_maxent_CA$maxentFold5 > sensSpec_scores_CA[3,6])

rf_sum_CA <- sum(model_stack_rf_CA$rfFold1 > sensSpec_scores_CA[4,2], 
                  model_stack_rf_CA$rfFold2 > sensSpec_scores_CA[4,3],
                  model_stack_rf_CA$rfFold3 > sensSpec_scores_CA[4,4],
                  model_stack_rf_CA$rfFold4 > sensSpec_scores_CA[4,5],
                  model_stack_rf_CA$rfFold5 > sensSpec_scores_CA[4,6])

svm_sum_CA <- sum(model_stack_svm_CA$svmFold1 > sensSpec_scores_CA[5,2], 
                  model_stack_svm_CA$svmFold2 > sensSpec_scores_CA[5,3],
                  model_stack_svm_CA$svmFold3 > sensSpec_scores_CA[5,4],
                  model_stack_svm_CA$svmFold4 > sensSpec_scores_CA[5,5],
                  model_stack_svm_CA$svmFold5 > sensSpec_scores_CA[5,6])



all_folds_CA <- sum( ann_sum_CA, gbm_sum_CA, maxent_sum_CA,rf_sum_CA, svm_sum_CA)
all_folds_13_CA <- all_folds_CA >= 13
all_folds_25_CA <- all_folds_CA == 25


ensemble_CA_df <- data.frame( rasterToPoints( all_folds_CA ) )
colnames(ensemble_CA_df) <-c("long", "lat", "Votes")


majority_CA_df <- data.frame( rasterToPoints( all_folds_13_CA ) )
colnames(majority_CA_df)  <-c("long", "lat", "Votes")

ud_CA_df <- data.frame( rasterToPoints( all_folds_25_CA ) )
colnames(ud_CA_df) <-c("long", "lat", "Votes")


# CA PLOTS

all_votes_CA <- 
  ggplot(data =ensemble_CA_df , aes(y=lat, x=long)) + 
  geom_raster(aes(fill = Votes )) +
  coord_cartesian(xlim = c( left, right ), ylim =c( bottom , top )) +
  ggtitle("All model election - California points") +
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA)



majority_votes_CA <- 
  ggplot(data = majority_CA_df , aes(y=lat, x=long)) + 
  geom_raster( aes(group=Votes, fill = Votes) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("Majority vote - California points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)




# Majority Native 
ud_votes_CA  <- 
  ggplot(data = ud_CA_df , aes(y=lat, x=long)) + 
  geom_raster( aes(fill = Votes ) ) +
  coord_cartesian( xlim = c( left , right ), ylim =c( bottom , top ) ) +
  ggtitle("Unanimous decision - California  points") +
  theme( panel.background = element_rect( fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid")) +
  scale_fill_gradientn(name = "Suitability", colours = rev(terrain.colors(2)), na.value = "blue") + 
  geom_polygon(aes(x = long, y = lat, group=id), data = states, colour="black", fill=NA) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = fort_nat, colour="red", fill=NA)




ggarrange(all_votes_native , all_votes_CA ,
          majority_votes_native , majority_votes_CA ,
          ud_votes_native , ud_votes_CA ,
          ncol = 2, nrow = 3)




