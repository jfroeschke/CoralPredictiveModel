library(raster)
predictors2 <- predictors


setwd("X:/Data_John/coral/CoralPredictiveModel/asc")
for(i in 1:length(names(predictors2))){
  NAME <- paste0(names(predictors2[[i]]), ".asc")
  writeRaster(predictors2[[i]], filename=NAME,
              overwrite=TRUE)
  print(i)
}
