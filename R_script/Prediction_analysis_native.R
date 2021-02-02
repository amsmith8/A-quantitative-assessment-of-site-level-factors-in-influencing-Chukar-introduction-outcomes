# Compute consion mtrix statistics by comparing rasters

library("raster")
library("RColorBrewer")
library("dismo")
library("maptools")
library("raster")
library("rgdal")
library("rgeos")
library("sp")

# Import necesary data 



# Step 2 : Prepare data and functions
####### ######## ######## ####### ######## ########
ann_tiffs <- list.files(path = "./Predictions/model_ann_folds", full.names= TRUE, pattern = ".tif")
gbm_tiffs <- list.files(path = "./Predictions/model_gbm_folds", full.names= TRUE, pattern = ".tif")
maxent_tiffs <- list.files(path = "./Predictions/model_maxent_folds", full.names= TRUE, pattern = ".tif")
rf_tiffs <- list.files(path = "./Predictions/model_rf_folds", full.names= TRUE, pattern = ".tif")
svm_tiffs <- list.files(path = "./Predictions/model_svm_folds", full.names= TRUE, pattern = ".tif")


source("./R_script/Functions.R")
source( "R_script/eBird_data_cleaning.R" )

#compile tiffs to stacks  
model_stack_svm <- listToStack(svm_tiffs)
model_stack_gbm <- listToStack(gbm_tiffs)
model_stack_maxent <- listToStack(maxent_tiffs)
model_stack_rf <- listToStack(rf_tiffs)
model_stack_ann <- listToStack(ann_tiffs)

#Chukar range polygon file 
#Ac_poly <- rgdal::readOGR("/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp") # via Bird Life
#crs(Ac_poly) <- crs(model_stack_rf$rfFold1)

### Seperate the polygon into native and naturalized regions
#Native <- subset(Ac_poly, Ac_poly$OBJECTID == 36 )  # historical  native range for A. chukar. Similar to Christensen 1970
#Naturalized <- subset(Ac_poly, Ac_poly$OBJECTID != 36 ) # recognized regions of naturalized populations


naturalized <- circles(us_pts, d = d , dissolve=TRUE, lonlat=TRUE) #49km is the average distance recorded in Bohl 1957
naturalized  <- polygons(naturalized )


# limit to contiguous US states
states <- rgdal::readOGR("/Users/austinsmith/Downloads/ne_110m_admin_1_states_provinces/ne_110m_admin_1_states_provinces.shp") 
crs(states) <- crs(model_stack_rf$rfFold1)

contiguous_us <- states[states$name != "Alaska",]  # remove Alaska
contiguous_us  <- contiguous_us [contiguous_us $name != "Hawaii",] # remove Hawaii
#plot(contiguous_us)


# Plot colors  
#palette <-colorRampPalette( c( "navy" , "goldenrod1" , "red" ) )
#palette2 <- colorRampPalette( c( "skyblue2" , "firebrick" ) )


# Step 3 : Accuracy - function,s images, plots
####### ######## ######## ####### ######## ########

# MESS Model if wanting to compare novel locations - not needed for analysis 01/17/20
# us_mess <- raster( "Predictions/mess_us.tif"   ) 
# us_mess <- crop(us_mess, naturalized )
# us_mess <- mask(us_mess, naturalized)
# us_mess <- us_mess >0
# plot(us_mess)

#Ac_raster <-mosaic(us_mess , absent_space, fun = sum) # used if wanting to replace other model 
#plot(Ac_raster)

# Create raster of chukar range model 
absent_space <- model_stack_rf$rfFold1 # can be any raster - just need it to convert background space to 0
absent_space [absent_space  < 1] <- 0
#plot(absent_space)

# create raster with chukar range = 1, absent = 0
Ac_raster <-rasterize(naturalized , absent_space, field=1)
#plot(Ac_raster)

true_value_chukar <- merge( Ac_raster, absent_space )
#plot(true_value_chukar)

contiguous_us_chukar <- crop(true_value_chukar, contiguous_us )
#plot(contiguous_us_chukar, main = "Chukar range")
#plot(contiguous_us, add = T)



# CALCULATIONS 

# import threshoolds
auc_scores <- readRDS("./RDS_objects/Native/auc_scores.rds")
sensSpec_scores <- readRDS("./RDS_objects/Native/sensSpec_scores.rds")

AUC_mean <- apply( auc_scores[,-1] , 1, mean , na.rm = TRUE )
AUC_sd <- apply( auc_scores[,-1] , 1 , sd , na.rm = TRUE )

auc_summary <-  round ( data.frame( AUC_mean, AUC_sd ), 4 )
row.names(auc_summary) <- c( "ANN" , "GBM" , "MaxEnt" , "RF" , "SVM" )
auc_summary

# ANN

ann_cm_results <- rbind(
  compute_raster_cm (model_stack_ann$annFold1 , sensSpec_scores[ 1 , 2 ] , contiguous_us_chukar ), 
  compute_raster_cm (model_stack_ann$annFold2 , sensSpec_scores[ 1 , 3 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_ann$annFold3 , sensSpec_scores[ 1 , 4 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_ann$annFold4 , sensSpec_scores[ 1 , 5 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_ann$annFold5 , sensSpec_scores[ 1 , 6 ] , contiguous_us_chukar )
)

ann_cm_results <-data.frame( ann_cm_results)
row.names(ann_cm_results) <- c( "ANN_Fold_1", "ANN_Fold_2" , "ANN_Fold_3" , "ANN_Fold_4" , "ANN_fold_5" )
ann_cm_results$Model <- c( rep("ANN", 5 ) )  


ann_summary <- c(mean(ann_cm_results[,1]), sd(ann_cm_results[,1]),
                 mean(ann_cm_results[,2]), sd(ann_cm_results[,2]),
                 mean(ann_cm_results[,3]), sd(ann_cm_results[,3])
                 )


# GBM
gbm_cm_results <- rbind(
  compute_raster_cm (model_stack_gbm$gbmFold1 , sensSpec_scores[ 2 , 2 ] , contiguous_us_chukar ), 
  compute_raster_cm (model_stack_gbm$gbmFold2 , sensSpec_scores[ 2 , 3 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_gbm$gbmFold3 , sensSpec_scores[ 2 , 4 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_gbm$gbmFold4 , sensSpec_scores[ 2 , 5 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_gbm$gbmFold5 , sensSpec_scores[ 2 , 6 ] , contiguous_us_chukar )
)
gbm_cm_results <-data.frame( gbm_cm_results)
row.names(gbm_cm_results) <- c( "GBM_Fold_1", "GBM_Fold_2" , "GBM_Fold_3" , "GBM_Fold_4" , "GBM_fold_5" )
gbm_cm_results$Model <- c( rep("GBM", 5 ) )

gbm_summary <- c(mean(gbm_cm_results[,1]), sd(gbm_cm_results[,1]),
                 mean(gbm_cm_results[,2]), sd(gbm_cm_results[,2]),
                 mean(gbm_cm_results[,3]), sd(gbm_cm_results[,3])
)


# MaxEnt
maxent_cm_results <- rbind(
  compute_raster_cm (model_stack_maxent$maxentFold1 , sensSpec_scores[ 3 , 2 ] , contiguous_us_chukar ), 
  compute_raster_cm (model_stack_maxent$maxentFold2 , sensSpec_scores[ 3 , 3 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_maxent$maxentFold3 , sensSpec_scores[ 3 , 4 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_maxent$maxentFold4 , sensSpec_scores[ 3 , 5 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_maxent$maxentFold5 , sensSpec_scores[ 3 , 6 ] , contiguous_us_chukar )
)
maxent_cm_results <-data.frame( maxent_cm_results)
row.names(maxent_cm_results) <- c( "MaxEnt_Fold_1", "MaxEnt_Fold_2" , "MaxEnt_Fold_3" , "MaxEnt_Fold_4" , "MaxEnt_fold_5" )
maxent_cm_results$Model <- c( rep("MaxEnt", 5 ) )

maxent_summary <- c(mean(maxent_cm_results[,1]), sd(maxent_cm_results[,1]),
                 mean(maxent_cm_results[,2]), sd(maxent_cm_results[,2]),
                 mean(maxent_cm_results[,3]), sd(maxent_cm_results[,3])
)


# RF
rf_cm_results <- rbind(
  compute_raster_cm (model_stack_rf$rfFold1 , sensSpec_scores[ 4 , 2 ] , contiguous_us_chukar ), 
  compute_raster_cm (model_stack_rf$rfFold2 , sensSpec_scores[ 4 , 3 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_rf$rfFold3 , sensSpec_scores[ 4 , 4 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_rf$rfFold4 , sensSpec_scores[ 4 , 5 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_rf$rfFold5 , sensSpec_scores[ 4 , 6 ] , contiguous_us_chukar )
)

rf_cm_results <-data.frame( rf_cm_results)
row.names(rf_cm_results) <- c( "RF_Fold_1", "RF_Fold_2" , "RF_Fold_3" , "RF_Fold_4" , "RF_fold_5" )
rf_cm_results$Model <- c( rep("RF", 5 ) )

rf_summary <- c(mean(rf_cm_results[,1]), sd(rf_cm_results[,1]),
                 mean(rf_cm_results[,2]), sd(rf_cm_results[,2]),
                 mean(rf_cm_results[,3]), sd(rf_cm_results[,3])
)

# SVM 
svm_cm_results <- rbind(
  compute_raster_cm (model_stack_svm$svmFold1 , sensSpec_scores[ 5 , 2 ] , contiguous_us_chukar ), 
  compute_raster_cm (model_stack_svm$svmFold2 , sensSpec_scores[ 5 , 3 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_svm$svmFold3 , sensSpec_scores[ 5 , 4 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_svm$svmFold4 , sensSpec_scores[ 5 , 5 ] , contiguous_us_chukar ),
  compute_raster_cm (model_stack_svm$svmFold5 , sensSpec_scores[ 5 , 6 ] , contiguous_us_chukar )
)

svm_cm_results <-data.frame( svm_cm_results)
row.names(svm_cm_results) <- c( "SVM_Fold_1", "SVM_Fold_2" , "SVM_Fold_3" , "SVM_Fold_4" , "SVM_Fold_5" )
svm_cm_results$Model <- c( rep("SVM", 5 ) )

svm_summary <- c(mean(svm_cm_results[,1]), sd(svm_cm_results[,1]),
                 mean(svm_cm_results[,2]), sd(svm_cm_results[,2]),
                 mean(svm_cm_results[,3]), sd(svm_cm_results[,3])
)

prediction_summary <- data.frame(round(rbind(ann_summary,gbm_summary,maxent_summary,rf_summary,svm_summary),4))
row.names(prediction_summary) <- c( "ANN" , "GBM" , "MaxEnt" , "RF" , "SVM" )
colnames(prediction_summary) <- c( "Acc_mean" , "Acc_sd" , "Sens_mean" , "Sens_sd" , "Spec_mean" , "Spec_sd" )
prediction_summary 
###

# Statistic summary

statistics_summary <- cbind( auc_summary, prediction_summary)


### 

all_cm_results <- do.call("rbind", list( ann_cm_results,
                                  gbm_cm_results,
                                  maxent_cm_results,
                                  rf_cm_results,
                                  svm_cm_results 
                                  )
)

# Reconfigure for plotting
all_cm_gglist <- reshape2::melt( all_cm_results , id.var='Model' )
all_cm_gglist$Stage <- "Predicition"

# Reconfigure the auc matrix to match the spatial statistics
auc_melt <- reshape2::melt( auc_scores , id.var='model' )
colnames(auc_melt)[1] <- "Model"
auc_melt[,2] <- "AUC"
auc_melt$Stage <- "Evaluation"

# All data 
final_table <- rbind( auc_melt , all_cm_gglist )

library(ggpubr)
plot1 <- ggbarplot(final_table, x = "variable", y = "value", ,position = position_dodge(0.8),
                   title = "Model statistics - native points",
                   xlab = "",
                   ylab = "Score",
                   fill = "Model",
                   palette  = brewer.pal(n = 5, name = "Blues"),
                   legend = "bottom",
                   facet.by = "Stage",
                   add = c("mean_sd"),
                   rremove("x.text")
)
plot1 +   theme(plot.title = element_text(hjust = 0.5) ) + facet_grid(~Stage, space = "free_x", scales = "free_x")


saveRDS( all_cm_results , "./RDS_objects/Native/all_cm_results.rds" )
saveRDS( statistics_summary , "./RDS_objects/Native/statistics_summary.rds" )



# ENSEMBLES 

# Avergae ann
ann_mean <- mean(model_stack_ann)
ann_mean_sensSpec <- rowMeans(sensSpec_scores[1,-1])

# ANN - Voting 
ann_sum <- sum(model_stack_ann$annFold1 > sensSpec_scores[1,2], 
               model_stack_ann$annFold2 > sensSpec_scores[1,3],
               model_stack_ann$annFold3 > sensSpec_scores[1,4],
               model_stack_ann$annFold4 > sensSpec_scores[1,5],
               model_stack_ann$annFold5 > sensSpec_scores[1,6])

# CM Results  
ann_ensembles <- data.frame( rbind( compute_raster_cm (ann_mean , ann_mean_sensSpec  , contiguous_us_chukar ) ,
                                    cm_raster( crop( ann_sum >= 3 , contiguous_us_chukar) , contiguous_us_chukar ),
                                    cm_raster( crop( ann_sum == 5 , contiguous_us_chukar) , contiguous_us_chukar )
                                    )
                             )
row.names( ann_ensembles ) <- c("mean", "n >= 3", "n = 5" )
ann_ensembles$Model <- c( rep("ANN", 3 ) )
ann_ensembles$Method<- c("Mean", "PV", "UD" )
#

# Avergae gbm
gbm_mean <- mean(model_stack_gbm)
gbm_mean_sensSpec <- rowMeans(sensSpec_scores[2,-1])

# gbm - Voting 
gbm_sum <- sum(model_stack_gbm$gbmFold1 > sensSpec_scores[2,2], 
               model_stack_gbm$gbmFold2 > sensSpec_scores[2,3],
               model_stack_gbm$gbmFold3 > sensSpec_scores[2,4],
               model_stack_gbm$gbmFold4 > sensSpec_scores[2,5],
               model_stack_gbm$gbmFold5 > sensSpec_scores[2,6])

# CM Results 
gbm_ensembles <- data.frame( rbind( compute_raster_cm (gbm_mean , gbm_mean_sensSpec  , contiguous_us_chukar ) ,
                                    cm_raster( crop( gbm_sum >= 3 , contiguous_us_chukar) , contiguous_us_chukar ),
                                    cm_raster( crop( gbm_sum == 5 , contiguous_us_chukar) , contiguous_us_chukar )
                                    )
                             )
row.names( gbm_ensembles ) <- c( "mean", "n >= 3", "n = 5" )
gbm_ensembles$Model <- c( rep("GBM", 3 ) )
gbm_ensembles$Method<- c("Mean", "PV", "UD" )

# Avergae maxent
maxent_mean <- mean(model_stack_maxent)
maxent_mean_sensSpec <- rowMeans(sensSpec_scores[3,-1])

# maxent - Voting 
maxent_sum <- sum(model_stack_maxent$maxentFold1 > sensSpec_scores[3,2], 
               model_stack_maxent$maxentFold2 > sensSpec_scores[3,3],
               model_stack_maxent$maxentFold3 > sensSpec_scores[3,4],
               model_stack_maxent$maxentFold4 > sensSpec_scores[3,5],
               model_stack_maxent$maxentFold5 > sensSpec_scores[3,6])

# CM Results
maxent_ensembles <- data.frame( rbind( compute_raster_cm (maxent_mean , maxent_mean_sensSpec  , contiguous_us_chukar ) ,
                                       cm_raster( crop( maxent_sum >= 3 , contiguous_us_chukar) , contiguous_us_chukar ),
                                       cm_raster( crop( maxent_sum == 5 , contiguous_us_chukar) , contiguous_us_chukar )
                                       )
                                )
row.names( maxent_ensembles ) <- c( "mean", "n >= 3", "n = 5" )
maxent_ensembles$Model <- c( rep("MaxEnt", 3 ) )
maxent_ensembles$Method<- c("Mean", "PV", "UD" )

# Avergae rf
rf_mean <- mean(model_stack_rf)
rf_mean_sensSpec <- rowMeans(sensSpec_scores[4,-1])

# rf - Voting 
rf_sum <- sum(model_stack_rf$rfFold1 > sensSpec_scores[4,2], 
                  model_stack_rf$rfFold2 > sensSpec_scores[4,3],
                  model_stack_rf$rfFold3 > sensSpec_scores[4,4],
                  model_stack_rf$rfFold4 > sensSpec_scores[4,5],
                  model_stack_rf$rfFold5 > sensSpec_scores[4,6])

# CM Results
rf_ensembles <- data.frame( rbind( compute_raster_cm (rf_mean , rf_mean_sensSpec  , contiguous_us_chukar ),
                                   cm_raster( crop( rf_sum >= 3 , contiguous_us_chukar) , contiguous_us_chukar ),
                                   cm_raster( crop( rf_sum == 5 , contiguous_us_chukar) , contiguous_us_chukar )
                                   )
                  )
row.names( rf_ensembles ) <- c( "mean", "n >= 3", "n = 5" )
rf_ensembles$Model <- c( rep("RF", 3 ) )
rf_ensembles$Method<- c("Mean", "PV", "UD" )


# Avergae svm
svm_mean <- mean(model_stack_svm)
svm_mean_sensSpec <- rowMeans(sensSpec_scores[5,-1])
svm_mean_cm <- compute_raster_cm (svm_mean , svm_mean_sensSpec  , contiguous_us_chukar )

# svm - Voting 
svm_sum <- sum(model_stack_svm$svmFold1 > sensSpec_scores[5,2], 
              model_stack_svm$svmFold2 > sensSpec_scores[5,3],
              model_stack_svm$svmFold3 > sensSpec_scores[5,4],
              model_stack_svm$svmFold4 > sensSpec_scores[5,5],
              model_stack_svm$svmFold5 > sensSpec_scores[5,6])

# CM Results
svm_ensembles <- data.frame( rbind( cm_raster( crop( svm_sum  >= 1, contiguous_us_chukar) , contiguous_us_chukar ),
                                    cm_raster( crop( svm_sum >= 3 , contiguous_us_chukar) , contiguous_us_chukar ),
                                    cm_raster( crop( svm_sum == 5 , contiguous_us_chukar) , contiguous_us_chukar )
                                    )
                             )
row.names( svm_ensembles ) <- c( "mean", "n >= 3", "n = 5" )
svm_ensembles$Model <- c( rep("SVM", 3 ) )
svm_ensembles$Method<- c("Mean", "PV", "UD" )

alg_ensembles <- rbind( ann_ensembles , gbm_ensembles , maxent_ensembles , rf_ensembles , svm_ensembles )

all_ensembles_gglist <- reshape2::melt( alg_ensembles , id.var=c('Method',"Model" ))



plot2 <- ggbarplot(all_ensembles_gglist , x = "variable", y = "value", ,position = position_dodge(0.8),
                   title = "Ensembles",
                   xlab = "",
                   ylab = "Score",
                   fill = "Model",
                   palette  = brewer.pal(n = 5, name = "Blues"),
                   legend = "bottom",
                   facet.by = "Method",
                   add = c("mean_sd"),
                   rremove("x.text"),
                   x.text.angle = 45,
)
plot2 +   theme(plot.title = element_text(hjust = 0.5) ) 



#  TOTAL ENSEMBLE 


# svm - Voting 
complete_sum <- sum(ann_sum , gbm_sum , maxent_sum , rf_sum , svm_sum ) 

# CM -
complete_uv_cm <- rbind( cm_raster( crop( complete_sum >= 13 , contiguous_us_chukar) , contiguous_us_chukar ),
                          cm_raster( crop( complete_sum == 25 , contiguous_us_chukar) , contiguous_us_chukar )
)
row.names(complete_uv_cm) <- c( "n >= 13", "n = 25" )

plot( crop( complete_sum  >= 13 , contiguous_us_chukar ) )
plot(states, add = T)

saveRDS( alg_ensembles, "./RDS_objects/Native/alg_ensembles.rds" )
saveRDS( all_ensembles_gglist , "./RDS_objects/Native/all_ensembles_gglist.rds" )
saveRDS( complete_uv_cm  , "./RDS_objects/Native/complete_uv_cm.rds" )
