## Chukar modeling - site level factors 
## Author: Austin M. Smith


# SET-UP ENVIRONMENT


#rm(list=ls(all=TRUE))

source( "R_script/Global-covariate-prep.R" )
Model_CRS <- crs( super_stack )


library( "maptools" )
library( "rgdal" )
library( "rgeos" )
library( "sp" )
library( "tictoc" )



# remove unnecessary objects 
rm(bioclim_list,bioclim_stack,topo_list,topo_stack)


# DATA PREPROCESS



#Chukar range polygon file 
Ac_poly <- readShapePoly( "/Users/austinsmith/Documents/SDM_spatial_data/Bird_life_galliformes_fgip/Alectoris_chukar/Alectoris_chukar.shp" ) # via Bird Life
crs( Ac_poly ) <- Model_CRS

### Seperate the polygon into native and naturalized regions
native <- subset( Ac_poly , Ac_poly$OBJECTID == 36 )  # historical  native range for A. chukar. Similar to Christensen 1970
naturalized <- subset( Ac_poly, Ac_poly$OBJECTID != 36 ) # recognized regions of naturalized populations


library( "rnaturalearth" )
world_land <- ne_countries( returnclass = c( "sp", "sf" ) )

# Remove bioclimatic extremes
antarctica <- ne_countries( continent = 'antarctica' )
greenland <- ne_countries( country = 'greenland' )

world_land <- world_land - antarctica - greenland
crs( world_land ) <- Model_CRS


#### Calculations to determine how many points
#sum( area( Ac_poly ) ) / sum( area( world_land ) ) # determine ratio of background.pts : Native.pts



# COMPUTE COVARIATES



set.seed( 5421 )

# Present points
chukar_present <- spsample( native, 1000 , type = 'random' )
chukar_present_coords <- data.frame( chukar_present@coords ) # data frame of lon lat coords 
names(chukar_present_coords) <- c( "lon", "lat" )

k <- 5
k_means <- kmeans( chukar_present_coords , k)
occ_grp <- k_means$cluster

chukar_present_covariates <- extract( super_stack, chukar_present_coords ) # extract values from raster stack 
chukar_present <- cbind( chukar_present_coords, chukar_present_covariates ) # combine coords and covars into one df 

# Background points
background_pts <- spsample( world_land , 11000, type = 'random' )
background_coords <- data.frame( background_pts@coords )
names(background_coords) <- c("lon", "lat")

background_covariates <- extract( super_stack , background_coords )
background_sample <- cbind( background_coords , background_covariates )

#Combine data 
pts_id <-  c(rep(1, nrow(chukar_present)), 
             rep(0, nrow(background_sample))) 

sdm_data <- data.frame( rbind( chukar_present , background_sample ) ) 
sdm_data <- data.frame( cbind( sdm_data , pts_id ) )
sdm_data <- na.omit(sdm_data) # remove N/A's



# The points used for model building
saveRDS(chukar_present, "./RDS_objects/chukar_present_pts.rds")
saveRDS(background_sample, "./RDS_objects/background_sample.rds"  )
saveRDS(sdm_data, "./RDS_objects/chukar_sdm_data.rds"  )
saveRDS(k, "./RDS_objects/k.rds"  )
saveRDS(k_means, "./RDS_objects/k_means_clusters.rds")
saveRDS(occ_grp,"./RDS_objects/occ_grp.rds")





# CREATE DATA STORAGE



# Create data.frame to hold AUC scores 
auc_scores <- data.frame( matrix( NA , nrow = 5 , ncol= 6 ) )
auc_scores[,1] <- c( "ANN","GBM", "MaxEnt", "RF", "SVM")
colnames(auc_scores)<- c("model","Fold_1","Fold_2","Fold_3","Fold_4","Fold_5")

# Create data.frame to hold thresholds
sensSpec_scores <- data.frame( matrix( NA , nrow = 5 , ncol = 6 ) )
sensSpec_scores[,1] <- c( "ANN","GBM", "MaxEnt", "RF", "SVM")
colnames(sensSpec_scores) <- c("model","Fold_1","Fold_2","Fold_3","Fold_4","Fold_5")

# Model stacks
model_stack_ann <- stack()
model_stack_gbm <- stack()
model_stack_maxent <- stack()
model_stack_rf <- stack()
model_stack_svm <- stack()


# RUN ALGORITHMS



# Problem with files on computer.  this uploads rJava for the session on current computer 
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_241.jdk/Contents/Home/jre/lib/server/libjvm.dylib')

#Sys.setenv(NOAWT=TRUE)
# Maxent -dependency 
library("rJava") 
library("dismo")
library("nnet")
library("gbm")
library("kernlab")
library("neuralnet")
library("randomForest")


# Model loop
for (i in 1:k) {
  
  train <- sdm_data[ occ_grp != i, ] # all folds excluding the i-th 
  test <- sdm_data[ occ_grp == i, ] # only the i-th
  
  
  # ANN
  ######## ######## #######
  tic("ann")
  ann <- neuralnet(pts_id ~ ., data=train[3:26] , stepmax = 1e+07 ) # RPROP+ algorithm
  #evaluate
  eval_ann <-  evaluate(p=test[test$pts_id == 1,], test[test$pts_id == 0,], ann)
  auc_scores[1,1+i] <- eval_ann@auc
  auc_scores[1,1+i]
  sensSpec_scores[1,1+i] <- threshold(eval_ann, stat='spec_sens')
  pred_ann <- predict(super_stack, ann)
  model_stack_ann <- stack(model_stack_ann, pred_ann) # add rasters
  toc()
  
  
  # GBM
  ######## ######## #######
  tic("gbm")
  gbm <- gbm( pts_id ~ ., data = train[3:26] , n.trees = 100 )
  #evaluate
  eval_gbm <-  evaluate( p = test[test$pts_id == 1,], test[test$pts_id == 0,], gbm, n.trees = 100 )
  auc_scores[2,1+i] <- eval_gbm@auc
  sensSpec_scores[2,1+i] <- threshold(eval_gbm, stat ='spec_sens')
  pred_gbm <- predict( super_stack, gbm , n.trees = 100 )
  model_stack_gbm <- stack( model_stack_gbm, pred_gbm ) # add rasters
  toc()


  # MaxEnt
  ######## ######## #######
  tic("maxent")
  train_occ <- subset(train, pts_id == 1)
  train_bg <- subset(train, pts_id == 0)
  maxent <- maxent( x = super_stack , p = train_occ[1:2], a = train_bg[1:2] ) # background,  regression
  test_occ <- subset(test, pts_id == 1)
  test_bg <- subset(test, pts_id == 0)
  #evaluate
  eval_maxent <-  evaluate( p = test_occ[1:2], a <-test_bg[1:2], maxent, x = super_stack )
  auc_scores[3,1+i] <- eval_maxent@auc
  sensSpec_scores[3,1+i] <- threshold(eval_maxent, stat='spec_sens')
  pred_maxent<- predict( super_stack , maxent )
  model_stack_maxent <- stack( model_stack_maxent , pred_maxent ) # add rasters
  toc()


  # RF
  ######## ######## #######
  tic("rf")
  rf <- randomForest( pts_id ~ ., data = train[3:26] )
  #evaluate
  eval_rf <-  evaluate( p = test[test$pts_id == 1,], test[test$pts_id == 0,], rf)
  auc_scores[4,1+i] <- eval_rf@auc
  sensSpec_scores[4,1+i] <- threshold( eval_rf , stat = 'spec_sens' )
  pred_rf <- predict( super_stack , rf )
  model_stack_rf <- stack( model_stack_rf , pred_rf ) # add rasters
  toc()

  # SVM
  ######## ######## #######
  tic("svm")
  svm <- ksvm(pts_id ~ ., data=train[3:26] )
  #evaluate
  eval_svm <-  evaluate(p=test[test$pts_id == 1,], test[test$pts_id == 0,], svm)
  auc_scores[5,1+i] <- eval_svm@auc
  sensSpec_scores[5,1+i] <- threshold(eval_svm, stat='spec_sens')
  pred_svm <- predict(super_stack, svm)
  model_stack_svm <- stack(model_stack_svm, pred_svm) # add rasters
  toc()
  
 gc() 
}

###-----------------------------------------------------------------------------------------------------------------------


###################################################
### code chunk number 7: save-output
###################################################
saveRDS( auc_scores , "./RDS_objects/auc_scores.rds" )
saveRDS( sensSpec_scores , "./RDS_objects/sensSpec_scores.rds" )


###-----------------------------------------------------------------------------------------------------------------------

# Important objects

# # The points used for model building
# saveRDS(chukar_present, "./RDS_objects/chukar_present_pts.rds")
# saveRDS(background_sample, "./RDS_objects/background_sample.rds"  )
# saveRDS(sdm_data, "./RDS_objects/chukar_sdm_data.rds"  )
# saveRDS(k_means, "./RDS_objects/k_means_clusters.rds")
# saveRDS(occ_grp,"./RDS_objects/occ_grp.rds")


names( model_stack_ann ) <- c("annFold1", "annFold2", "annFold3", "annFold4", "annFold5")
names( model_stack_gbm ) <- c("gbmFold1", "gbmFold2", "gbmFold3", "gbmFold4", "gbmFold5")
names (model_stack_maxent ) <- c("maxentFold1", "maxentFold2", "maxentFold3", "maxentFold4", "maxentFold5")
names( model_stack_rf ) <- c("rfFold1", "rfFold2", "rfFold3", "rfFold4", "rfFold5")
names( model_stack_svm ) <- c("svmFold1", "svmFold2", "svmFold3", "svmFold4", "svmFold5")


dir.create( "Predictions")
dir.create("Predictions/model_ann_folds")
dir.create("Predictions/model_gbm_folds")
dir.create("Predictions/model_maxent_folds")
dir.create("Predictions/model_rf_folds")
dir.create("Predictions/model_svm_folds")

writeRaster(model_stack_ann, filename=file.path("Predictions/model_ann_folds",names(model_stack_ann)), bylayer=TRUE,format="GTiff")
writeRaster(model_stack_gbm, filename=file.path("Predictions/model_gbm_folds",names(model_stack_gbm)), bylayer=TRUE,format="GTiff")
writeRaster(model_stack_maxent, filename=file.path("Predictions/model_maxent_folds",names(model_stack_maxent)), bylayer=TRUE,format="GTiff")
writeRaster(model_stack_rf, filename=file.path("Predictions/model_rf_folds",names(model_stack_rf)), bylayer=TRUE,format="GTiff")
writeRaster(model_stack_svm, filename=file.path("Predictions/model_svm_folds",names(model_stack_svm)), bylayer=TRUE,format="GTiff")

