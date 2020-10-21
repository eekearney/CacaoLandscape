##Figuring out box content file 
rm(list=ls())
setwd("~/")

##Data
load("~/Box Sync/Analysis_Projects/Cacao/Analyses/Data/Insects_Cnts.RData")
load("~/Box Sync/Analysis_Projects/Cacao/Analyses/Data/CollEvents.RData")

##Parsing down insect file to necessary columns
collnum <-  insects[,c(1:3)]

## Parsing down collev file to necessary
events <-  collev[,1:3]

##Merging
collev_trees <- merge(collnum, events, by.x = "Coll_Event", by.y = "Coll_Event")


##Aggregating to get # of trees per collection
collcnts <-  aggregate(collev_trees$Tree, list(collev_trees$Coll_Event,
                                  collev_trees$Chakra.x), FUN="length")

##Saving
write.table(collcnts, file =
     "~/Box Sync/Analysis_Projects/Cacao/Analyses/Data/Box_Cnts.csv")

