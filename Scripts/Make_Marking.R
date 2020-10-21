### Merge data files to make marking data for analysis 
rm(list=ls())
setwd("~/")

## Libraries

## Load in spreadsheets
load("~/MEGA/Analyses16/Data/Marking.RData")
load("~/MEGA/Analyses16/Data/EditedTrees.RData")
load("~/MEGA/Analyses16/Data/CollEvents.RData")
load("~/MEGA/Analyses16/Data/FieldData.RData")

##paring down marking
marking <- marking[,1:10]

##Editing Tree data
trees$Trunk_Cir <- scale(trees$Trunk_Cir)
trees$Can_Ht <- scale(trees$Can_Ht)
trees$PerCover <- scale(trees$PerCover)
trees$PerShade <- scale(trees$PerShade)

##Collecting Events
Mcollev <- collev[collev$Purpose == "Marking",]

Mcollev$Temp_Start <-  as.numeric(Mcollev$Temp_Start)
Mcollev$Temp_End <-  as.numeric(Mcollev$Temp_End)

Mcollev$TempAvg <- scale(((Mcollev$Temp_Start+Mcollev$Temp_End)/2))
Mcollev$Time_Start <- scale(Mcollev$Time_Start)

##Take only open and hand pollinated entries
markingHO <-  marking[marking$Trt == "O" | marking$Trt == "H",]

##merge landscape types with hand pollination data
marking1 <- merge(markingHO, field, by = "Chakra")

##merge collection events with hand polliantion data 
marking2 <- merge(marking1, collev, by.x = "CollectingEvent" , by.y =
              "Coll_Event")

##make unique matching column for trees and marking2 
trees$Chakra_Tree <- paste(trees$Chakra, trees$Tree, sep = "_")
marking2$Chakra_Tree <- paste(marking2$Chakra.x, marking2$Tree, sep = "_")

##merge marking2 and trees
marking3 <-  merge(marking2, trees, by = "Chakra_Tree")

##take needed columns
marking4 <- marking3[,c("CollectingEvent", "Chakra.x", "Type.x", "Tree.x",
                        "Treatment", "Branch", "Dist", "Ht", "Diam",
                        "Trt", "Date.x", "Fruit (y/n)", "Date.y",
                        "Time_Start", "Temp_Start", "Temp_End",
                        "Weather_Start", "Weather_End", "Trunk_Cir",
                        "Can_Ht", "Water", "PerShade", "PerCover",
                        "Type.y")] ##add in DaysFirst, Recent and NumTreat

colnames(marking4) <- c("Coll_Event", "Chakra", "Type", "Tree",
                        "Treatment", "Branch", "Distance", "Height", "Diameter",
                        "Pollination", "DateChecked", "Fruit", "DateMarked",
                        "Time_Start", "Temp_Start", "Temp_End",
                        "Weather_Start", "Weather_End", "Trunk_Cir",
                        "Can_Ht", "Water", "PerShade", "PerCover",
                        "Tree_Type")

##make site and chakra number columns
firstletter <- sapply(strsplit(as.character(marking4$Chakra), ""), "[", 1)
secondletter <- sapply(strsplit(as.character(marking4$Chakra), ""), "[", 2)
marking4$Site <-  paste(firstletter, secondletter, sep = "")

marking4$Leaves <- sapply(strsplit(as.character(marking4$Treatment), ""), "[", 1)
marking4$Banana <- sapply(strsplit(as.character(marking4$Treatment), ""), "[", 2)

##taking out the couldn't be found rows
marking5 <- marking4[!marking4$Fruit == "Couldn't be found",]

##making columns numeric or factors 
marking5$Coll_Event <-  as.factor(as.character(marking5$Coll_Event))
marking5$Chakra <-  as.factor(marking5$Chakra)
marking5$Tree <-  as.factor(as.character(marking5$Tree))
marking5$Branch <-  as.factor(marking5$Branch)
marking5$Site <-  as.factor(marking5$Site)
marking5$Field <-  as.factor(marking5$Field)
marking5$Type <- as.factor(marking5$Type) 

marking5$Height <-  as.numeric(marking5$Height)
marking5$Distance <-  as.numeric(marking5$Distance)
marking5$Trunk_Cir <-  as.numeric(marking5$Trunk_Cir)
marking5$Can_Ht<-  as.numeric(marking5$Can_Ht)
marking5$PerShade <-  as.numeric(marking5$PerShade)
marking5$PerCover <-  as.numeric(marking5$PerCover)
marking5$Temp_Start <-  as.numeric(marking5$Temp_Start)
marking5$Temp_End <-  as.numeric(marking5$Temp_End)

marking6 <- marking5

####Test for correlation (here instead of at the beginning of the analysis part) ########
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, use = "complete.obs"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~ Distance + Height + Diameter,
      data=markingScaledNAOmit, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20) ## need to modify these to the right data/columns
      ##Distance and Diameter are ...

pairs(~ Trunk_Cir + Can_Ht + PerCover + PerShade, 
      data=markingScaledNAOmit, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20) 
      ##Trunk_Cir and Can_Ht are ...

pairs(~ AvgTemp + SDOYmarked + Time_Start + AvgWeather + DaysChecked, 
      data=markingScaledNAOmit, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
      ##Temp_Avg and DOY are ...

pairs(~ Slope + Type,
      data=markingScaledNAOmit, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
      ## not really correlated

##making julian dates
class(marking6$DateMarked)
Dates <- as.Date(marking6$DateMarked, format = "%m/%d/%Y")
head(Dates)
marking6$DOY <- as.numeric(strftime(Dates, format = "%j"))

##making tree factors
marking6$TreeF <- with(marking6, factor(Chakra:Tree))

##Making Fruit column binomial 
marking6$Fruit <- (marking6$Fruit == "Y")*1

##Scaling variables
markingScaled <- marking6

markingScaled$Distance <- scale(marking6$Distance)
markingScaled$Diameter <- scale(marking6$Diameter)
markingScaled$Height <- scale(marking6$Height)
markingScaled$Time_Start <- scale(marking6$Time_Start)
markingScaled$TempAvg <- scale(((marking6$Temp_Start+marking6$Temp_End)/2))

##taking only columns needed
markingScaled2 <- markingScaled[, c("Fruit", "Leaves", "Banana", "Pollination", 
                  "Type", "Distance", "Diameter", "Height", 
                  "DOY", "Time_Start", "Water",
                  "Trunk_Cir", "Can_Ht", "Tree_Type",
                  "PerCover", "PerShade", "TempAvg", "Site", "Chakra",
                  "TreeF")] ## need to add in again! 



## ##No AvgTemp
## markingScaledAT <- markingScaled[!is.na(markingScaled$TempAvg),]

## ##No NAs in Dist
## markingScaledDist <- markingScaled[!is.na(markingScaled$Distance),]

## ###Taking Distance our (only diameter) makes everything significant...


##Taking NAs out 
markingScaledNAOmit <- na.omit(markingScaled2)

##Saving Dataframe
save(markingScaledNAOmit, file =
     "~/Box Sync/Analysis_Projects/Cacao/Analyses16/Data/Marking_Ready.RData")

##non-binomial #s 

prop <- function(x) {
      p <- sum((x == "1")*1)/length(x)
      p
}

props <- aggregate(markingScaled$Fruit, 
      by = list(markingScaled$TreeF, markingScaled$DOY, markingScaled$Treatment, 
            markingScaled$Pollination, markingScaled$Trunk_Cir, 
            markingScaled$Can_Ht, markingScaled$Type, markingScaled$Time_Start, 
            markingScaled$TempAvg, markingScaled$Leaves, markingScaled$Banana, 
             markingScaled$Tree_Type, markingScaled$PerShade, markingScaled$Water,
            markingScaled$PerCover), 
      FUN = prop)

colnames(props) <- c("TreeF", "DOY", "Treatment","Pollination", 
      "Trunk_Cir", "Can_Ht", "Type", "Time_Start", "TempAvg", 
      "Leaves", "Banana", "Tree_Type", "PerShade", "Water",  
      "PerCover", "Prop")

props2 <- props[props$Pollination == "H", ]
props2$Open <- props$Prop[props$Pollination == "O"]

props2$Diff <- props2$Prop-props2$Open

props2$Rarefied <- (props2$Prop-props2$Open)/props2$Prop


props2$Chakra <- sapply(strsplit(as.character(props2$TreeF), ":"), "[", 1)

props2$Site <- paste(sapply(strsplit(as.character(props2$TreeF), ""), "[", 1), 
      sapply(strsplit(as.character(props2$TreeF), ""), "[", 2), sep = "")

props2$Types <- sapply(props2$Type, function(x) if (x == "1") {"2.Ag"} else if (x == "2") {"1.Frag"} else {"3.For"})
      ##no difference between frag and forest (as expected - only one ag field so not a usable result)

pollmodel <- lmer(Diff ~ Leaves*Banana +
                  DOY + Can_Ht + Time_Start + 
                  PerCover + TempAvg +
                  (1|Type) + (1|TreeF), 
                  data =  props2)

                  control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e9)), na.action = na.omit)






