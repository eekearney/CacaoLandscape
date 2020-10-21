### Merge data files to make data labels
##Need locality, date, and collector 
rm(list=ls())
setwd("~/")

##Libraries
library(splitstackshape)
library(reshape)

##Data 
load("~/Dropbox/Cacao/Analyses16/Data/Insects_Cnts.RData") 
load("~/Dropbox/Cacao/Analyses16/Data/EditedCollEvents.RData")

##Summing collected insects to get basic count
insects$Total <- rowSums(insects[,5:8], na.rm = TRUE) ##summing
##everything collected insect columns, then adding in buffer
insects$NewTotal <- round((insects$Total), 0) + 2

##Parsing down insect file to necessary columns 
collnum <-  insects[1:584,c(1:3,15)]

## Parsing down collev file to necessary
events <-  collev[,c("Coll_Event", "Chakra", "Date", "TOD")]

#Merging the two 
orglabel <- merge(collnum, events, by.x = "CollectingEvent", by.y = "Coll_Event")

##Reorg
orglabel <-  orglabel[,c(1,6,5,3,7,4)]

colnames(orglabel) <-  c("CollEvent", "Date", "Site", "Tree",
                         "TOD", "NumLabels")

##Expanding rows by number in numlabels
labels <-  expandRows(orglabel, "NumLabels")

#order the labels by chakra and then collecting event number
ordlabels <- labels[order(labels$Site, labels$CollEvent),]

##Adding in time of day for collection
for (i in 1:length(unique(ordlabels$TOD))) {

  subset <-  ordlabels[ordlabels$TOD == unique(ordlabels$TOD)[i],]

  CurrentTOD <- unique(ordlabels$TOD)[i]

  if (CurrentTOD == "EM") {
    subset$TimeOfDay <- rep("Dawn", dim(subset)[1])
  } else if (CurrentTOD == "MD") {
    subset$TimeOfDay <- rep("Noon", dim(subset)[1])
  } else if (CurrentTOD == "LA") {
    subset$TimeOfDay <- rep("Dusk", dim(subset)[1])
  } else {
    subset$TimeOfDay <- rep("NA", dim(subset)[1])
  }
  
if (i ==1) {
    labels2 <- subset
  } else {
    labels2 <-  rbind(labels2, subset)
  }

}

##Adding in Fields, Town Names and lat and longitudes 
for (i in 1:length(unique(labels2$Site))) {

  subset <-  labels2[labels2$Site == unique(labels2$Site)[i],]

  CurrentSite <- unique(labels2$Site)[i]

  if (CurrentSite == "RB1") {
    subset$Field <- rep("Field 1", dim(subset)[1])
    subset$Town <- rep("Rio Blanco", dim(subset)[1])
    subset$LatLong <- rep("S 1.006651, W 77.564365", dim(subset)[1])
  } else if (CurrentSite == "RB2"){
    subset$Field <- rep("Field 2", dim(subset)[1])
    subset$Town <- rep("Rio Blanco", dim(subset)[1])
    subset$LatLong <- rep("S 1.006651, W 77.564365", dim(subset)[1])
  } else if (CurrentSite == "RB3"){
    subset$Field <- rep("Field 3", dim(subset)[1])
    subset$Town <- rep("Rio Blanco", dim(subset)[1])
    subset$LatLong <- rep("S 1.006651, W 77.564365", dim(subset)[1])
  } else if (CurrentSite == "RB4"){
    subset$Field <- rep("Field 4", dim(subset)[1])
    subset$Town <- rep("Rio Blanco", dim(subset)[1])
    subset$LatLong <- rep("S 1.006651, W 77.564365", dim(subset)[1])
  } else if (CurrentSite == "SJ1"){
    subset$Field <- rep("Field 1", dim(subset)[1])
    subset$Town <- rep("San Jose", dim(subset)[1])
    subset$LatLong <- rep("S 0.926717, W 77.786488", dim(subset)[1])
  } else if (CurrentSite == "SJ2"){
    subset$Field <- rep("Field 2", dim(subset)[1])
    subset$Town <- rep("San Jose", dim(subset)[1])
    subset$LatLong <- rep("S 0.926717, W 77.786488", dim(subset)[1])
  } else if (CurrentSite == "CC1"){
    subset$Field <- rep("Field 1", dim(subset)[1])
    subset$Town <- rep("Campo Cocha", dim(subset)[1])
    subset$LatLong <- rep("S 1.089848, W 77.563197", dim(subset)[1])
  } else if (CurrentSite == "CC2"){
    subset$Field <- rep("Field 2", dim(subset)[1])
    subset$Town <- rep("Campo Cocha", dim(subset)[1])
    subset$LatLong <- rep("S 1.089848, W 77.563197", dim(subset)[1])
  } else if (CurrentSite == "CC3"){
    subset$Field <- rep("Field 3", dim(subset)[1])
    subset$Town <- rep("Campo Cocha", dim(subset)[1])
    subset$LatLong <- rep("S 1.089848, W 77.563197", dim(subset)[1])
  } else if (CurrentSite == "CC4"){
    subset$Field <- rep("Field 4", dim(subset)[1])
    subset$Town <- rep("Campo Cocha", dim(subset)[1])
    subset$LatLong <- rep("S 1.089848, W 77.563197", dim(subset)[1])
  } else if (CurrentSite == "CC5"){
    subset$Field <- rep("Field 5", dim(subset)[1])
    subset$Town <- rep("Campo Cocha", dim(subset)[1])
    subset$LatLong <- rep("S 1.110528, W 77.552647", dim(subset)[1])
  } else if (CurrentSite == "CCall"){
    subset$Field <- rep("All Fields", dim(subset)[1])
    subset$Town <- rep("Campo Cocha", dim(subset)[1])
    subset$LatLong <- rep("S 1.089848, W 77.563197", dim(subset)[1])
  } else if (CurrentSite == "SJall"){
    subset$Field <- rep("All Fields", dim(subset)[1])
    subset$Town <- rep("San Jose", dim(subset)[1])
    subset$LatLong <- rep("S 0.926717, W 77.786488", dim(subset)[1])
  }
  
  if (i ==1) {
    labels3 <- subset
  } else {
    labels3 <-  rbind(labels3, subset)
  }
  
} 

##Adding in placeholders 
labels3$Tree_Lab <- rep("Tree", dim(labels3)[1])
labels3$State_Lab <- rep("Napo", dim(labels3)[1])
labels3$Country_Lab <- rep("Ecuador", dim(labels3)[1])
labels3$Coll_Lab <- rep("E. Kearney", dim(labels3)[1])

newlabels <- labels3

##Reformat dates
newlabels$Date <- as.Date(newlabels$Date, format = "%m/%d/%y")
                                        #code as dates

newlabels$Date_Lab <- format(newlabels$Date,'%d %b %Y')

## ##Pasting into one column
## newlabels$all <- paste("=","\"",newlabels$Tree_Lab,newlabels$Tree,
##                        newlabels$Field, "\"", "& CHAR(13) &", "\"", newlabels$Site,
##                        newlabels$State_Lab, newlabels$Country_Lab,
##                        "\"", "& CHAR(13) &", "\"",
##                        newlabels$Date_Lab, newlabels$Coll_Lab, "\"", "%")

## newlabels$CollEventmod <- paste(newlabels$CollEvent, "%")


##Saving in editable format by excel
write.csv(newlabels, file = "~/Dropbox/Cacao/Analyses16/Data/Labels.csv")
