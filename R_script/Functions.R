require("raster")



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




# Function 1
#--------------------------------------------------
cm_raster <- function(true, pred){
  
  ct <- crosstab(true, pred, long = TRUE)
  
  TN <- ct$Freq[1]  # True negative (TN)
  FN <- ct$Freq[2]  # False negative (FN)
  FP <- ct$Freq[3]  # False positive (FP)
  TP <- ct$Freq[4]  # True positve (TP)
  
  acc = (TN + TP)/(TN + FN + FP + TP ) # Accuracy
  tpr = TP/(FP + TP) # True positve rate - Sensitvity***
  tnr = TN/(TN + FN) # True negative rate - Specificity***
  #fpr = FN/(TN + FN) #  False positve rate
  #fnr = FP/(FP + TP) # False negative rate

  Stats <- data.frame(round( c(acc, tpr, tnr ), 4) )
  #Stats <- data.frame(round( c(acc, tpr, tnr, fpr, fnr), 3) )
  colnames(Stats) <- names(pred)
  rownames(Stats) <- c( "Accuracy" , "Sensitivity" , "Specificity" )
  #rownames(Stats) <- c( "Accuracy" , "Sensitivity" , "Specificity" , "False Positive Rate" , "False Negative Rate" )
  t(Stats)
}





# Function 3:

compute_raster_cm <- function(raster, thold, true ){
  
  # true is an already cropped version of the raster used to validate model (raster input).
  
  raster [raster > thold ] <- 1
  raster [raster < thold ] <- 0
  
  sub_raster <- crop(raster, true) # only consider region that matches true 
  
  cm_raster( true, sub_raster )
  
}




# cm_raster <- function(true, pred){
#   
#   ct <- crosstab(true, pred, long = TRUE)
#   
#   TN <- ct$Freq[1]  # True negative (TN)
#   FN <- ct$Freq[2]  # False negative (FN)
#   FP <- ct$Freq[3]  # False positive (FP)
#   TP <- ct$Freq[4]  # True positve (TP)
#   
#   acc = (TN + TP)/(TN + FN + FP + TP ) # Accuracy
#   tpr = TP/(FP + TP) # True positve rate - Sensitvity*** 
#   tnr = TN/(TN + FN) # True negative rate - Specificity***
#   #fpr = FN/(TN + FN) #  False positve rate
#   #fnr = FP/(FP + TP) # False negative rate
#   
#   Stats <- data.frame(round( c(acc, tpr, tnr ), 3) )
#   #Stats <- data.frame(round( c(acc, tpr, tnr, fpr, fnr), 3) )
#   colnames(Stats) <- names(pred)
#   rownames(Stats) <- c( "Accuracy" , "Sensitivity" , "Specificity" )
#   #rownames(Stats) <- c( "Accuracy" , "Sensitivity" , "Specificity" , "False Positive Rate" , "False Negative Rate" )
#   t(Stats)
# }

