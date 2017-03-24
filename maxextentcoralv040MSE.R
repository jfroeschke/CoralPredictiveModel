### Apply maxent models to taxa using all 27 input rasters

## Read in rasters from here: X:\Data_John\coral\maxent\dwc\Predictors\alberspredictors
## Stack into raster brick
## Read in coral data with environmental variables values extracted to dataframe
## Fit model
## Plot variable importance
## Make Prediction Map
## Plot ROC 
## Repeat for n species

#Define group of interest
groupName <- "Stony coral"



library(raster)
library(rgdal)
library(ggplot2)
library(rJava)
library(dismo)
library(sp)
library(reshape2)
library(dplyr)


###### Read in data and stack as a raster brick
#### See section 2.1 - 2.5 of maxextentcoralv010.R for information on the preparation of these rasters
#library(raster)
setwd("X:/Data_John/coral/maxent/dwc/Predictors/alberspredictors")
datafiles <- Sys.glob("*.tif")
predictors <- stack(datafiles)


## Read in coral data with environmental variables values extracted to dataframe
## Read in Coral and Extracted Environmental data at each coral location (all species)
#### See section 3.1 - 3.5 of maxextentcoralv010.R for information on the preparation of this shapefile
#library(rgdal)
CoralNOAAsp <- readOGR("X:/Data_John/coral/maxent/gulfcoral/CoralNOAAsp.shp", layer="CoralNOAAsp")

#### See section 4.1 - 4.2 of maxextentcoralv010.R for information on the preparation of this shapefile
#EnvOut <- read.csv("X:/Data_John/coral/maxent/gulfcoral/predictors.csv")


##  create taxa specfic data sets 
coraldf <- CoralNOAAsp@data
coraldf2 <- cbind(coordinates(CoralNOAAsp), coraldf)

# library(rJava)
# library(dismo)
# library(sp)
new.proj <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# GROUPS <- unique(CoralNOAAsp@data$Name)
# for(i in 1:length(GROUPS){


stonyCoral <- subset(coraldf2, Name=="Stony coral")
stonyCoraldf <- stonyCoral[,1:2]
colnames(stonyCoraldf) <- c("x","y")
stonyCoralsp <- stonyCoraldf
coordinates(stonyCoralsp) <- ~ x + y
proj4string(stonyCoralsp) <- new.proj

## Section 5.3: witholding a 20% sample for testing

stonyCoralsp$fold <- kfold(stonyCoralsp, k=5)
stonyCoraltest <- subset(stonyCoralsp, stonyCoralsp$fold==1)
stonyCoraltrain <- subset(stonyCoralsp, stonyCoralsp$fold!=1)


## Section 5.4: fit a maxent model
me <- maxent(predictors, coordinates(stonyCoraltrain))

## Plot variable importance
## Section 5.5: plot variable importance
Results <- data.frame(me@results)
##grep 'contribution' 
Results$Names <- rownames(Results)
VarImpRows <-  grep("\\contribution\\>",Results[,2]) 
VarImp <- Results[VarImpRows,]
#library(reshape2)
tmp <-  colsplit(VarImp$Names, pattern="\\.", c("Var", "Del"))
VarImpClean <- data.frame(cbind(tmp$Var, VarImp$me.results))
colnames(VarImpClean) <- c("Predictor", "Relative.Influence")
VarImpClean$Relative.Influence <- as.numeric(as.character(VarImpClean$Relative.Influence))

#library(dplyr)
VarImp2 <- arrange(VarImpClean, desc(as.numeric(as.character(VarImpClean[,2]))))

VarImp2$order <- length(VarImp2$Relative.Influence):1
VarImp2$Predictor <- reorder(VarImp2$Predictor, VarImp2$order)


p <- ggplot(data = VarImp2, aes(x = Predictor, y = Relative.Influence))
p2 <- p + geom_bar(stat = "identity", colour = "black", fill = "#0072B2") + 
  labs(x = "", y = "Relative influence (%)") + 
  theme(panel.background = element_rect(fill = "gray", 
                                        colour = "black")) + coord_flip()
p3 <- p2 + theme(axis.text = element_text(colour = "black", 
                                          size = 15))
p4 <- p3 + scale_y_continuous(expand = c(0, 0.25), limits = c(0, 
        (max(VarImp2$Relative.Influence) + 0.1 * max(VarImp2$Relative.Influence))))
p5 <- p4 + theme(panel.border = element_rect(colour = "black", 
                                             fill = NA, size = 2))
p6 <- p5 + theme(axis.title.x = element_text(colour = "black", 
                                             size = 15))

ggsave(p6, filename="X:/Data_John/coral/CoralPredictiveModel/stonyCoral.png",
       dpi=600, height=8,width=8, units=c("in"))

################# End VarImp Plot ########################


# background data
bg <- randomPoints(predictors, 1000)

# extract values
pvtest <- data.frame(extract(predictors, coordinates(stonyCoraltest) ))
avtest <- data.frame(extract(predictors, bg))
e2 <- evaluate(me, p=pvtest, a=avtest)

testp <- predict(me, pvtest)
head(testp)
testa <- predict(me, avtest)
e3 <- evaluate(p=testp, a=testa)
e3
threshold(e3)
plot(e3, 'ROC')


######################### Spatially Explicit Predictions ###########

# predict to entire dataset
r <- predict(me, predictors)
# plot(r)
# plot(antipathesModsp, add=TRUE)
writeRaster(r, filename="X:/Data_John/coral/CoralPredictiveModel/raster/stonyCoral.tif", overwrite=TRUE)
writeOGR(stonyCoralsp , "X:/Data_John/coral/CoralPredictiveModel/shp/stonyCoral.shp","AntipathesModsp", driver="ESRI Shapefile", overwrite=TRUE)

library(devtools)
source_gist("https://gist.github.com/jfroeschke/f8b3ef5b3992d59014bb363e149b7861")

rasterSelect <- function(x, maxDepth=-3704, minDepth=0) {
  require(raster)
  ## Define two functions
  fun <- function(x) { x[x >=minDepth] <- NA; return(x)} 
  funmax <- function(x) { x[x <= maxDepth] <- NA; return(x)} 
  ## Subset
  xfun <- calc(x, fun)
  xfunmax <- calc(xfun, funmax)
  ##Return raster object
  return(xfunmax)
}
stonyCoralNoSmall <- rasterSelect(r, .02, 1)
writeRaster(stonyCoralNoSmall, 
      filename="X:/Data_John/coral/CoralPredictiveModel/raster/stonyCoralNoSmall.tif", overwrite=TRUE)

