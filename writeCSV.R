##write occurrence data as csv
## Read in coral data with environmental variables values extracted to dataframe
## Read in Coral and Extracted Environmental data at each coral location (all species)
#### See section 3.1 - 3.5 of maxextentcoralv010.R for information on the preparation of this shapefile
library(rgdal)
CoralNOAAsp <- readOGR("X:/Data_John/coral/maxent/gulfcoral/CoralNOAAsp.shp", layer="CoralNOAAsp")

##  create taxa specfic data sets 
coraldf <- CoralNOAAsp@data
coraldf2 <- cbind(coordinates(CoralNOAAsp), coraldf)

###get nrows by group
Octocoral <- subset(coraldf2, Name=="Octocoral") #6371
Stonycoral <- subset(coraldf2, Name=="Stony coral") #1571
Blackcoral <- subset(coraldf2, Name=="Black coral") #2223

Octocoralxy <- data.frame(
                taxa=rep("Octocoral", nrow(Octocoral)),
                x=Octocoral[,1],
                y=Octocoral[,2])
Blackcoralxy <- data.frame(
  taxa=rep("Blackcoral", nrow(Blackcoral)),
  x=Blackcoral[,1],
  y=Blackcoral[,2])

Stonycoralxy <- data.frame(
  taxa=rep("Stonycoral", nrow(Stonycoral)),
  x=Stonycoral[,1],
  y=Stonycoral[,2])

write.csv(Blackcoralxy, 
          "X:/Data_John/coral/CoralPredictiveModel/csv/BlackCoral.csv",
          row.names=FALSE)
write.csv(Octocoralxy, 
          "X:/Data_John/coral/CoralPredictiveModel/csv/Octocoral.csv",
          row.names=FALSE)
write.csv(Stonycoralxy, 
          "X:/Data_John/coral/CoralPredictiveModel/csv/Stonycoral.csv",
          row.names=FALSE)

##This will export file for use in the maxent program

# presencePoints <- data.frame(coordinates(tmptrain))
# Species <- data.frame(taxa=rep("BlackCoral", 1257))
# BlackCoral <- cbind(Species, presencePoints)
# write.csv(BlackCoral, 
#           "X:/Data_John/coral/CoralPredictiveModel/csv/BlackCoral.csv",
#           row.names=FALSE)
# 
# ##write occurrence data as csv
# presencePoints <- data.frame(coordinates(tmptrain))
# Species <- data.frame(taxa=rep("BlackCoral", 1257))
# BlackCoral <- cbind(Species, presencePoints)
# write.csv(BlackCoral, 
#           "X:/Data_John/coral/CoralPredictiveModel/csv/BlackCoral.csv",
#           row.names=FALSE)