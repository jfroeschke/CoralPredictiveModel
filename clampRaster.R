library(raster)

bcf <- raster("X:/Data_John/coral/CoralPredictiveModel/blackcoralFinal/Blackcoral.asc")

plot(bcf)
blackcoralNoSmall <- clamp(bcf, lower=.1, upper=1)


rasterFileNameSmall <- paste0("rastersmall/", GROUPS[i], ".tif")
writeRaster(tmpNoSmall, 
            filename=rasterFileNameSmall, overwrite=TRUE)