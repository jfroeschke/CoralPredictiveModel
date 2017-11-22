library(raster)

bcf <- raster("X:/Data_John/coral/CoralPredictiveModel/blackcoralFinal/Blackcoral.asc")

plot(bcf)
blackcoralNoSmall <- clamp(bcf, lower=.1, upper=1,useValues=FALSE)
blackcoralNoSmall <- projectRaster(blackcoralNoSmall, crs="+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
##reproject rasters and export
##pr

projection(blackcoralNoSmall) <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

writeRaster(blackcoralNoSmall, 
            filename="X:/Data_John/coral/CoralPredictiveModel/RastersNoSmall/blackcoralNoSmall.tif", overwrite=TRUE)

ocf <- raster("X:/Data_John/coral/CoralPredictiveModel/octocoralFinal/Octocoral.asc")

plot(ocf)
octocoralNoSmall <- clamp(ocf, lower=.1, upper=1,useValues=FALSE)


projection(octocoralNoSmall) <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

writeRaster(octocoralNoSmall, 
            filename="X:/Data_John/coral/CoralPredictiveModel/RastersNoSmall/octocoralNoSmall.tif", overwrite=TRUE)

scf <- raster("X:/Data_John/coral/CoralPredictiveModel/stonycoralFinal/Stonycoral.asc")

plot(scf)
stonycoralNoSmall <- clamp(scf, lower=.1, upper=1, useValues=FALSE)


projection(stonycoralNoSmall) <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

writeRaster(stonycoralNoSmall, 
            filename="X:/Data_John/coral/CoralPredictiveModel/RastersNoSmall/stonycoralNoSmall.tif", overwrite=TRUE)

###NO clamping for comparison
bcf2 <- bcf
projection(bcf2) <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

writeRaster(bcf2, 
            filename="X:/Data_John/coral/CoralPredictiveModel/RastersNoSmall/blackcoralwithSmall.tif", overwrite=TRUE)
