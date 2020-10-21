##making Trees Data
rm(list=ls())
setwd("~/") 

## Packages 

##Load 
load("~/MEGA/Analyses16/Data/Trees.RData")


##renaming columns 
colnames(trees) <- c("Chakra", "Tree", "Treatment", "Type", "Trunk_Cir", 
	"Can_Ht", "Water", "PerShade", "PerCover")


#Save 
save(trees, file = "~/MEGA/Analyses16/Data/EditedTrees.RData")