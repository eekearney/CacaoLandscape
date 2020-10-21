### Merge data files to make marking data for analysis 
rm(list=ls())
setwd("~/")

## Libraries
library(lme4)
library(lmerTest)
library(chron)
library(MuMIn)
library(car)
library(boot)

## Load in spreadsheets
load("~/Box Sync/Analysis_Projects/Cacao/Analyses15/Data/Specimens.RData")
load("~/Box Sync/Analysis_Projects/Cacao/Analyses15/Data/Trees.RData")
load("~/Box Sync/Analysis_Projects/Cacao/Analyses15/Data/EditedCollEvents.RData")
load("~/Box Sync/Analysis_Projects/Cacao/Analyses15/Data/Landscape.RData")
load("~/Box Sync/Analysis_Projects/Cacao/Analyses15/Data/Insects_Cnts.RData")

##Get Rough numbers
##take out genus and species rows
spec <- specimens[,c(1:8,11)]

spec.comp <- spec[!is.na(spec$Order),]

##Percentages of orders
perOrder <- aggregate(spec.comp$Essig_ID, by = list(spec.comp$Order), length)

colnames(perOrder) <- c("Order", "SpecN")

perOrder$Percent <- perOrder$SpecN/dim(spec.comp)[1]
 
##consolidating
topOrder <-  data.frame(matrix(nrow = 9, ncol =
                               2))

colnames(topOrder) <- c("Order", "Percent")

topOrder$Order <- perOrder$Order[c(9,13,16,10,14,7,5,8,15)]

topOrder$Percent <- perOrder$Percent[c(9,13,16,10,14,7,5,8,15)]

topOrder$Percent[1] <- topOrder$Percent[1] + perOrder$Percent[6]

topOrder$Percent[7] <- topOrder$Percent[7] + perOrder$Percent[3] + perOrder$Percent[4]

topOrder$Percent[4] <- topOrder$Percent[4] + perOrder$Percent[11] + perOrder$Percent[12]

topOrder$Percent[3] <- topOrder$Percent[3] + perOrder$Percent[17]

##Graph

orders <- ggplot(data=topOrder, aes(x=Order, y=Percent, fill=Order)) +
  geom_bar(stat="identity")  + theme_bw() +
    theme(panel.grid = element_blank(), panel.background =
          element_rect(size = 1))

orders2 <- orders + scale_x_discrete(limits=
  topOrder$Order)  + theme(axis.text.x=element_text(angle=45, hjust =
  1))   + theme(legend.position="none") + theme(panel.border = element_blank())


orders3 <- orders2  + theme(axis.text.x = element_text(size = 12), axis.text.y =
    element_text(size = 12), 
    axis.title.y = element_text(size = 14))


pdf(file =
    "~/Box Sync//Analysis_Projects/Cacao/Analyses15/Results/OrdersPercentPlot.pdf",
    width = 11, height = 4.5)

orders3

dev.off()

## ~~~~~~~~~~~~~~~~~~~~ ## making morphospecies ## ~~~~~~~~~~~~~~~~~~~~ ## 
spec.comp$MorphoSpp <- rep(NA, 1442)

##not sure if this is a good use of time....

## ~~~~~~~~~~~~~~~~~~~~ ## END making morphospecies ## ~~~~~~~~~~~~~~~~~~~~ ## 


###making family level abundance data
colnames(insects)[1] <- "Coll_Event" 

##make columns to aggregate and merge by 
spec.comp$SampID <- paste(spec.comp$Coll_Event, spec.comp$Chakra, spec.comp$Tree,
                            sep = "_")

insects$SampID <- paste(insects$Coll_Event, insects$Chakra, insects$Tree,
                            sep = "_")

##taking out trees not sampled
r  <- with(insects,
           which(Notes=="RAIN"| Notes=="not enough time"|
           Notes=="ran out of time"| Notes=="NO FLOWERS",
                 arr.ind=TRUE))

insects.samp <- insects[-r, ]

##taking only needed columns
insects.samp2 <- insects.samp[,c("SampID","Coll_Event", "Chakra",
                                  "Tree", "CATs", "SBs", "OHs", "LFs",
                                  "LGs")]
insects.samp2$Diptera <- rep(NA, 880)  
insects.samp2$Hymenoptera <- rep(NA, 880)  
insects.samp2$Thysanoptera <- rep(NA, 880)  
insects.samp2$Hemiptera <- rep(NA, 880)  

##making long dataframe
insects.long <- melt(insects.samp2, id=c("SampID","Coll_Event",
                                      "Chakra", "Tree"))

##making NAs Zeros
insects.long[is.na(insects.long)] <- 0

colnames(insects.long)[5:6] <- c("Order", "Abund")

##aggregate
spec.sum <- aggregate(spec.comp$Essig_ID, list(spec.comp$SampID,
                                               spec.comp$Order),
                      FUN="length")

colnames(spec.sum) <- c("SampID","Order", "Abund") 

##making uniquely identifying column 
spec.sum$OrderID <- paste(spec.sum$SampID, spec.sum$Order, sep= "_")

insects.long$OrderID <- paste(insects.long$SampID, insects.long$Order, sep= "_")

##merging by OrderID
spec.tot <- merge(spec.sum, insects.long, by = "OrderID")

##Summing Abundances
spec.tot$Abundance <- spec.tot$Abund.y + spec.tot$Abund.x

##getting needed columns
spec.abund <- spec.tot[,c(1:3,6:8,11)]

colnames(spec.abund)[2:3] <-  c("SampID", "Order")


##Adding in type
spec.land <- merge(land, spec.abund, by = "Chakra")

##Adding in Tree data
colnames(trees)[c(2,6,7,11,12)] <- c("Tree", "PerShade", "PerCover",
                            "TreeFlNum", "NeighFlNum")

trees2 <- trees[, c("Chakra", "Tree", "Can_Ht", "Water (Y/N)",
                   "PerShade", "PerCover", "TreeFlNum", "NeighFlNum", "Dist")]

trees2$TreeID <- paste(trees2$Chakra, trees2$Tree, sep = "_")

spec.abund$TreeID <- paste(spec.abund$Chakra, spec.abund$Tree,
                            sep = "_")

spec.tree <- merge(trees2, spec.land, by = "TreeID")

##Adding in collev data
collev <- collev[,c("Coll_Event", "Chakra", "DOY", "Time_Start",
                    "TOD", "AvgTemp", "AvgWeather")]

spec.collev <- merge(collev, spec.tree, by = "Coll_Event")


##Clean
spec.all <- spec.collev[,c("Coll_Event", "Chakra.x", "Tree.x",
                             "Order", "Abundance", "Type", "Can_Ht",
                             "Water (Y/N)", "Dist",
                             "PerShade", "PerCover", "DOY",
                             "Time_Start", "TOD", "AvgTemp",
                             "AvgWeather", "TreeFlNum", "NeighFlNum",
                           "OrderID", "SampID", "TreeID")]

colnames(spec.all)[8] <- "WaterPres"

##Site
firstletter <- sapply(strsplit(as.character(spec.all$Chakra), ""), "[", 1)
secondletter <- sapply(strsplit(as.character(spec.all$Chakra), ""), "[", 2)
spec.all$Site <-  paste(firstletter, secondletter, sep = "")

##scaling
spec.all[,c(7,9,10,11,12,15,17,18)] <- apply(spec.all[,c(7,9,10,11,12,15,17,18)], 2, scale)

##as factors
spec.all[,c(1,2,4,6,8,14,16,21,22)] <-
  lapply(spec.all[,c(1,2,4,6,8,14,16,21,22)], as.factor) 

##glmm??
glmm(Abundance ~ Type + Dist + PerShade + PerCover + DOY + TOD +
     WaterPres + 
      + (1|Site) + (1|Chakra.x) + (1|TreeID)
