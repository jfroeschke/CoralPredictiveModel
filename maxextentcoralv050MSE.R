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
setwd("X:/Data_John/coral/CoralPredictiveModel")

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
##remove variables based on list in predictorCorrelation.R (lines 95-122)
datafiles2 <- datafiles[c(1,4:7, 10:19, 21,23,25)]
#predictors <- stack(datafiles)
predictors <- stack(datafiles2)

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
setwd("X:/Data_John/coral/CoralPredictiveModel")

###get nrows by group
OctocoralNROW <- nrow(subset(CoralNOAAsp@data, Name=="Octocoral")) #6371
StonycoralNROW <- nrow(subset(CoralNOAAsp@data, Name=="Stony coral")) #1571
BlackcoralNROW <- nrow(subset(CoralNOAAsp@data, Name=="Black coral")) #2223

GROUPS <- unique(CoralNOAAsp@data$Name)

for(i in 1:1){
#for(i in 2:length(GROUPS)){
print(i)
tmp <- subset(coraldf2, Name==GROUPS[i])

tmpdf <- tmp[,1:2]
colnames(tmpdf) <- c("x","y")
tmpsp <- tmpdf
coordinates(tmpsp) <- ~ x + y
proj4string(tmpsp) <- new.proj

## Section 5.3: witholding a 20% sample for testing
## conser 70% from phillip et al. ?
tmpsp$fold <- kfold(tmpsp, k=5)
tmptest <- subset(tmpsp, tmpsp$fold==1)
tmptrain <- subset(tmpsp, tmpsp$fold!=1)


## Section 5.4: fit a maxent model
me <- maxent(predictors, coordinates(tmptrain),
             nbg=1000)
# plot showing importance of each variable
#plot(me)



#response(me, var=8)
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
ggName <- paste0("png/", GROUPS[i], ".png")
ggsave(p6, filename=ggName,
       dpi=600, height=8,width=8, units=c("in"))

RDataName <- paste0("RData/", GROUPS[i], ".RData")
save(me, tmpsp, p6,file=RDataName)
################# End VarImp Plot ########################


# background data
bg <- randomPoints(predictors, 10000)#change according to gulliage

# extract values
pvtest <- data.frame(extract(predictors, coordinates(tmptest) ))
avtest <- data.frame(extract(predictors, bg))
e2 <- evaluate(me, p=pvtest, a=avtest)

testp <- predict(me, pvtest)
head(testp)
testa <- predict(me, avtest)
e3 <- evaluate(p=testp, a=testa)
e3
threshold(e3)
rocNAME <- paste0("roc/", GROUPS[i],"_ROC", ".png") 
png(filename=rocNAME, width=8, height = 8, units="in", res=600)
plot(e3, 'ROC')
dev.off()

## extract data for different plot if desired
# e3@TPR
# e3@FPR
# png(filename="rocNAME")
# plot(e3@FPR, e3@TPR, type="l")
# dev.off()
######################### Spatially Explicit Predictions ###########

# predict to entire dataset
r <- predict(me, predictors)
# plot(r)
# plot(antipathesModsp, add=TRUE)
rasterFileName <- paste0("raster/", GROUPS[i], ".tif")
writeRaster(r, filename=rasterFileName, overwrite=TRUE)
shpFileName <- paste0("shp/", GROUPS[i], ".shp")
shpFileobj <- paste0(GROUPS[i], "sp")
writeOGR(tmpsp , shpFileName,shpFileobj, driver="ESRI Shapefile", overwrite=TRUE)

library(devtools)
#source_gist("https://gist.github.com/jfroeschke/f8b3ef5b3992d59014bb363e149b7861")

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


tmpNoSmall <- rasterSelect(r, .1, 1)
rasterFileNameSmall <- paste0("rastersmall/", GROUPS[i], ".tif")
writeRaster(tmpNoSmall, 
      filename=rasterFileNameSmall, overwrite=TRUE)
print(i)
} ## end loop


###read

