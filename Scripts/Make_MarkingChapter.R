### Merge data files to make marking data for analysis 
rm(list=ls())
setwd("~/")

## Libraries 

##PCAs
library(psych)
library(FactoMineR)
library(nFactors)

## Load in spreadsheets
load("~/MEGA/Analyses16/Data/Marking.RData")
load("~/MEGA/Analyses16/Data/Edited_Trees.RData")
load("~/MEGA/Analyses16/Data/Edited_CollEvents.RData")
load("~/MEGA/Analyses16/Data/FieldData.RData") 

marking <- marking[,1:10]

########################################################################################################
########################################################################################################
####################################### ~~~~ Prepping ~~~~ #############################################
########################################################################################################
########################################################################################################

##Collecting Events - taking only marking flower entries
Mcollev <- collev[collev$Purpose == "MF",]

########################### Julian dates for MARKING ###########################
class(Mcollev$Date)
Dates <- as.Date(Mcollev$Date, format = "%m/%d/%Y")
head(Dates)
Mcollev$DOYMarked <- as.numeric(strftime(Dates, format = "%j"))

######################### Taking only polliantion data from marking dataframe ###############################
markingHO <-  marking[marking$Trt == "O" | marking$Trt == "H",]

########################### Julian dates for CHECKING ###########################
class(markingHO$Date)
Dates <- as.Date(markingHO$Date, format = "%m/%d/%Y")
head(Dates)
markingHO$DOYChecked <- as.numeric(strftime(Dates, format = "%j"))

##Checking Mcollev & MarkingHO 
head(Mcollev)
head(markingHO)

########################################################################################################
########################################################################################################
####################################### ~~~~ Merging ~~~~ ##############################################
########################################################################################################
########################################################################################################

############################### Field ###################################
##merge landscape types with hand pollination data
marking <- merge(markingHO, field, by = "Chakra")

########################## Collecting Info ##############################
##merge collection events with hand polliantion data 
marking2 <- merge(marking, Mcollev, by.x = "CollectingEvent" , by.y =
                    "Coll_Event")

############################### Trees ###################################
##make unique matching column for trees and marking2 
trees$Chakra_Tree <- paste(trees$Chakra, trees$Tree, sep = "_")
marking2$Chakra_Tree <- paste(marking2$Chakra.x, marking2$Tree, sep = "_")

##merge marking2 and trees
marking3 <-  merge(marking2, trees, by = "Chakra_Tree")

##take needed columns
marking4 <- marking3[,c("CollectingEvent", "Chakra.x", "Type.x", 
                        "slope", "northness", "eastness", "Tree.x",
                        "Treatment", "DOYChecked", "DOYMarked", 
                        "Time_Start", "AvgTemp", "AvgWeather", 
                        "DaysFirst", "DaysRecent","NumTreat",
                        "Trunk_Cir","Can_Ht", "Water", 
                        "PerShade", "PerCover", "Type.y",
                        "Branch", "Dist", "Ht", "Diam",
                        "Trt", "Fruit (y/n)")] ##add in DaysFirst, Recent and NumTreat

colnames(marking4) <- c("Coll_Event", "Chakra", "Type", 
                        "slope", "northness", "eastness", "Tree",
                        "Treatment", "DOYChecked", "DOYMarked", 
                        "Time_Start", "AvgTemp", "AvgWeather",
                        "DaysFirst", "DaysRecent", "NumTreat", 
                        "Trunk_Cir", "Can_Ht", "Water", 
                        "PerShade", "PerCover", "Tree_Type",  
                        "Branch", "Distance", "Height", "Diameter",
                        "Pollination", "Fruit")


########################################################################################################
########################################################################################################
####################################### ~~~~ Editing ~~~~ ##############################################
########################################################################################################
########################################################################################################

##make site and chakra number columns
firstletter <- sapply(strsplit(as.character(marking4$Chakra), ""), "[", 1)
secondletter <- sapply(strsplit(as.character(marking4$Chakra), ""), "[", 2)
marking4$Site <-  paste(firstletter, secondletter, sep = "")
marking4$Field <- sapply(strsplit(as.character(marking4$Chakra), ""), "[", 3)

##make separate treatment columns
marking4$Leaves <- sapply(strsplit(as.character(marking4$Treatment), ""), "[", 1)
marking4$Banana <- sapply(strsplit(as.character(marking4$Treatment), ""), "[", 2)

##Difference between DOYMarked and DOYChecked
##Need to scale of marking and day of check, 
#before scale figure the difference between the two
marking4$DiffDOY <- marking4$DOYChecked - marking4$DOYMarked

## for those flowers on trunks "-" replaced with 0
marking4$Distance[marking4$Distance == "-"] <- 0

####################################### ~~~~ Marking5 ~~~~ ##############################################
##taking out the couldn't be found rows
marking5 <- marking4[!marking4$Fruit == "Couldn't be found",]

##making columns numeric or factors 
marking5$Coll_Event <-  as.factor(as.character(marking5$Coll_Event))
marking5$Chakra <-  as.factor(marking5$Chakra)
marking5$Site <-  as.factor(marking5$Site)
marking5$Field <-  as.factor(as.character(marking5$Field))
marking5$Tree <-  as.factor(as.character(marking5$Tree))
marking5$Banana <- as.factor(marking5$Banana) 
marking5$Leaves <- as.factor(marking5$Leaves) 
marking5$Treatment <- as.factor(marking5$Treatment) 
marking5$Pollination <- as.factor(marking5$Pollination) 
marking5$Water <- as.factor(marking5$Water) 

marking5$AvgTemp <-  as.numeric(marking5$AvgTemp)
marking5$AvgWeather <-  as.numeric(marking5$AvgWeather)

marking5$DaysFirst <- as.numeric(marking5$DaysFirst)
marking5$DaysRecent <- as.numeric(as.character(marking5$DaysRecent))

marking5$Trunk_Cir <-  as.numeric(marking5$Trunk_Cir)
marking5$Can_Ht<-  as.numeric(marking5$Can_Ht)
marking5$PerShade <-  as.numeric(marking5$PerShade)
marking5$PerCover <-  as.numeric(marking5$PerCover)

marking5$Height <-  as.numeric(marking5$Height)
marking5$Distance <-  as.numeric(marking5$Distance)


################################## ~~~~ Marking6 - End Edits ~~~~ #######################################
marking6 <- marking5

save(marking6, file =
      "~/MEGA/Analyses16/Data/Marking_Halfway_20190121.RData")

########################################################################################################
########################################################################################################
#################################### ~~~~ Correlations ~~~~ ############################################
########################################################################################################
########################################################################################################

#load("~/MEGA/Analyses16/Data/Marking_Halfway_20181012.RData")

####Test for correlation (here instead of at the beginning of the analysis part) ########
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~ Distance + Height + Diameter,
      data=marking6, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20) ## need to modify these to the right data/columns
##Distance and Diameter are correlated at 0.65

pairs(~ Trunk_Cir + Can_Ht + PerCover + PerShade, 
      data=marking6, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20) 
##Trunk_Cir and Can_Ht are barely correlated (0.52)

pairs(~ AvgTemp + DOYMarked + Time_Start + AvgWeather + DiffDOY +
      DaysFirst + DaysRecent + NumTreat, 
      data=marking6, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
##DOYMarked is 0.81 correlated with NumTreat, & with DaysFirst (0.70)
##NumTreat is correlated with DaysFirst
##Both make sense... - should do a PCA

pairs(~ slope + Type + northness + eastness,
      data=marking6, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
##slope  & eastness very correlated (0.81) 

########################################################################################################
########################################################################################################
######################################## ~~~~ PCAs ~~~~ ################################################
########################################################################################################
########################################################################################################

##PCAFlower
flower.var <- marking6[,c("Distance", "Diameter")]
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(flower.var, cor=TRUE)
pcas <- fit$scores # the principal components
marking6$PCAFlower<- pcas[,1] ##82.0% of variation
##plots 
result <- PCA(flower.var) #82% 

##PCATreat
Treat.var <- marking6[,c("DOYMarked", "DaysFirst", "NumTreat")]
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(Treat.var, cor=TRUE)
pcas <- fit$scores # the principal components
marking6$PCATreat<- pcas[,1] 
##plots
result <- PCA(Treat.var) ## 87% of the variation in first axis 

##PCAField
Field.var <- marking6[,c("slope", "eastness")]
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(Field.var, cor=TRUE)
pcas <- fit$scores # the principal components
marking6$PCAField<- pcas[,1] ## 90% of variation explained
##plots 
result <- PCA(Field.var) #90% 

########################################################################################################
########################################################################################################
#################################### ~~~~ Final Edits ~~~~ #############################################
########################################################################################################
########################################################################################################

##making tree factors
marking6$TreeF <- with(marking6, factor(Chakra:Tree))

##Making Fruit column binomial 
marking6$Fruit <- (marking6$Fruit == "Y")*1

##taking only columns needed

markingFinal <- marking6[, c("Fruit", "Leaves", "Banana", "Treatment", "Pollination", 
                                    "northness", "PCAField", 
                                    "PCATreat", "DiffDOY",
                                    "Time_Start", "AvgWeather","AvgTemp", 
                                    "DaysRecent", 
                                    "Water", "Trunk_Cir", "Can_Ht", 
                                    "PerCover", "PerShade", 
                                    "PCAFlower",
                                    "Site", "Chakra", "Field", "TreeF")] 

####Checking for NAs that need to be taken out 
sapply(markingFinal, function(x) sum(is.na(x)))
##No NAs

#################################### ~~~~ Saving Dataframe ~~~~ ########################################
save(markingFinal, file =
       "~/MEGA/Analyses16/Data/MarkingFlowerLvl_20190121.RData")



##################################### Not Edited _ Oct 8, 2018 #########################################
########################################################################################################
########################################################################################################
#################################### ~~~~ Making by Tree ~~~~ ##########################################
########################################################################################################
########################################################################################################

###aggregate pollination data from markingScaledAT
markingSuccess <- aggregate(markingScaledAT$Fruit, by = list(markingScaledAT$Coll_Event, 
 markingScaledAT$TreeF, markingScaledAT$Pollination, markingScaledAT$Treatment, markingScaledAT$SDOYMarked), sum)

markingTotal <- aggregate(markingScaledAT$Fruit, by = list(markingScaledAT$Coll_Event, 
 markingScaledAT$TreeF, markingScaledAT$Pollination, markingScaledAT$Treatment, markingScaledAT$SDOYMarked), length)

colnames(markingSuccess) <- c("Coll_Event", "TreeF", "Pollination", "Treatment", "Date", "Sum")
colnames(markingTotal) <- c("Coll_Event", "TreeF", "Pollination", "Treatment", "Date", "Length")

markingProp <- merge(markingSuccess, markingTotal, by = c("Coll_Event", "TreeF", 
  "Pollination", "Treatment"), all = TRUE)

markingProp$proportion <- markingProp$Sum/markingProp$Length
#now we have all the correct numbers...just need to get them into the data frame in the right way

#################################### ~~~~ Putting Together ~~~~ ########################################
### now get columns for hand.prop, open.prop, and hand.flowers, open.flowers 
markingOpen <- markingProp[markingProp$Pollination == "O", c("Coll_Event", "TreeF", "Pollination", "Treatment", 
  "Sum", "Length", "proportion")]
head(markingOpen)

colnames(markingOpen)[5:7] <- c("SumOpen", "LengthOpen", "ProOpen")

markingHand <- markingProp[markingProp$Pollination == "H", c("Coll_Event", "TreeF", "Pollination", "Treatment", 
  "Sum", "Length", "proportion")]

colnames(markingHand) <- c("Coll_Event", "TreeF", "Pollination", "Treatment",
  "SumHand", "LengthHand", "ProHand")

##merge hand and open to make dataframe for proportions
markingReady <- merge(markingOpen, markingHand, by = c("Coll_Event", 
  "TreeF"), all = TRUE)

#################################### ~~~~ Merge w/ Rest ~~~~ ###########################################
markingScaledAT$UniqueID <- paste(markingScaledAT$Coll_Event, markingScaledAT$TreeF, sep = ":")

collapsed <- markingScaledAT[!duplicated(markingScaledAT$UniqueID), ]

markingTree <- merge(markingReady, collapsed, by = c("Coll_Event", 
  "TreeF"), all = TRUE)

markingTree2 <- markingTree[, c("Coll_Event", "Site", "Field", "Chakra", "Tree", "TreeF", 
  "Leaves", "Banana", "Treatment", 
  "SumOpen", "LengthOpen", "ProOpen", "SumHand", "LengthHand", "ProHand",
  "Type", "slope", "northness", "eastness", 
  "SDOYMarked", "SDOYChecked", "SDiffDOY", 
  "SDaysFirst", "SDaysRecent", "NumTreat",
  "AvgTemp", "AvgWeather", "Time_Start", 
  "Trunk_Cir", "Can_Ht", "Water", "PerCover", "PerShade", "Tree_Type")]

##remove tree without tree_type 
markingTree3 <- markingTree2[!markingTree2$Tree_Type == "-",]

#################################### ~~~~ Correlations ~~~~ ############################################

pairs(~ Trunk_Cir + Can_Ht + PerCover + PerShade, 
      data=markingTree3, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20) 
##nothing really correlated

pairs(~ AvgTemp + SDOYMarked + Time_Start + AvgWeather + SDiffDOY +
      SDaysFirst + SDaysRecent + NumTreat, 
      data=markingTree3, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
##SDOYMarked is 0.73 correlated with DaysFirst which makes sense...
##SDOYMarked & DaysFirst are both highly (0.84, 0.93) with NumTreat

pairs(~ slope + Type + northness + eastness,
      data=markingTree3, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
##slope & eastness are highly correlated (0.81)


#################################### ~~~~ Saving Dataframe ~~~~ ########################################
markingTreeLvl <- markingTree3

save(markingTreeLvl, file =
       "~/MEGA/Analyses16/Data/MarkingTreeLvl_Ready.RData")
 

