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
load("~/Dropbox/Analyses16/Data/Marking.RData")
load("~/Dropbox/Analyses16/Data/Edited_Trees.RData")
load("~/Dropbox/Analyses16/Data/Edited_CollEvents.RData")
load("~/Dropbox/Analyses16/Data/FieldData.RData") 

########################################################################################################
########################################################################################################
####################################### ~~~~ Prepping ~~~~ #############################################
########################################################################################################
########################################################################################################

####################################### Scaling Slope Data #############################################
field$slope <- scale(field$slope)
field$northness <- scale(field$northness)
field$eastness <- scale(field$eastness)

####################################### Scaling Tree Data #############################################
trees$Trunk_Cir <- scale(trees$Trunk_Cir)
trees$Can_Ht <- scale(trees$Can_Ht)
trees$PerCover <- scale(trees$PerCover)
trees$PerShade <- scale(trees$PerShade)

##Collecting Events - taking only marking flower entries
Mcollev <- collev[collev$Purpose == "MF",]

############################## Scaling Time, Temp, Weather Data ########################################
Mcollev$Time_Start <- scale(Mcollev$Time_Start)
Mcollev$AvgTemp <- scale(Mcollev$AvgTemp)
Mcollev$AvgWeather <- scale(Mcollev$AvgWeather)

####################################### Scaling Trickier Data #############################################

########################### Scaling julian dates for MARKING ###########################
class(Mcollev$Date)
Dates <- as.Date(Mcollev$Date, format = "%m/%d/%Y")
head(Dates)
Mcollev$DOYMarked <- as.numeric(strftime(Dates, format = "%j"))
ScaledDOY <- scale(unique(Mcollev$DOYMarked))
ScaledDOY
OrderedDOY <- unique(Mcollev$DOYMarked)[order(unique(Mcollev$DOYMarked), decreasing = FALSE)]
OrderedDOY
DOY <- as.data.frame(cbind(ScaledDOY, OrderedDOY))
colnames(DOY) <- c("ScaledDOY", "OrderedDOY")

##ordering scaled values correctly
DOY2 <- DOY[match(Mcollev$DOYMarked,DOY$OrderedDOY),] 

##putting scaled values back in
Mcollev$SDOYMarked <- DOY2$ScaledDOY

################## Scaling DaysFrist using Marked juliand date as peg ##################
ScaledDaysFirst <- scale(unique(Mcollev$DaysFirst))
ScaledDaysFirst
OrderedDaysFirst <- unique(Mcollev$DaysFirst)
OrderedDaysFirst
DaysFirst <- as.data.frame(cbind(ScaledDaysFirst, OrderedDaysFirst))
colnames(DaysFirst) <- c("ScaledDaysFirst", "OrderedDaysFirst")

##ordering scaled values correctly
DaysFirst2 <- DaysFirst[match(Mcollev$DaysFirst,DaysFirst$OrderedDaysFirst),] 

##putting scaled values back in
Mcollev$SDaysFirst <- DaysFirst2$ScaledDaysFirst

################## Scaling DaysRecent using Marked Julian date w/ recent as a peg ##################
Mcollev$DaysRecentPeg <- paste(Mcollev$DOYMarked, Mcollev$DaysRecent, sep = "_")
ScaledDaysRecent <- scale(Mcollev$DaysRecent[!duplicated(Mcollev$DaysRecentPeg)])
OrderedDaysRecent <- Mcollev$DaysRecentPeg[!duplicated(Mcollev$DaysRecentPeg)]
DaysRecent <- as.data.frame(cbind(ScaledDaysRecent, OrderedDaysRecent))
colnames(DaysRecent) <- c("ScaledDaysRecent", "OrderedDaysRecent")

##ordering scaled values correctly
DaysRecent2 <- DaysRecent[match(Mcollev$DaysRecentPeg, DaysRecent$OrderedDaysRecent),] 

##putting scaled values back in
Mcollev$SDaysRecent <- DaysRecent2$ScaledDaysRecent

######################### Taking only polliantion data from marking dataframe ###############################
markingHO <-  marking[marking$Trt == "O" | marking$Trt == "H",]

########################### Scaling julian dates for CHECKING ###########################
class(markingHO$Date)
Dates <- as.Date(markingHO$Date, format = "%m/%d/%Y")
head(Dates)
markingHO$DOYChecked <- as.numeric(strftime(Dates, format = "%j"))
head(markingHO$DOYChecked)
ScaledDOY <- scale(unique(markingHO$DOYChecked))
ScaledDOY
OrderedDOY <- unique(markingHO$DOYChecked)[order(unique(markingHO$DOYChecked), decreasing = FALSE)]
OrderedDOY
DOY <- as.data.frame(cbind(ScaledDOY, OrderedDOY))
colnames(DOY) <- c("ScaledDOY", "OrderedDOY")

##ordering scaled values correctly
DOY2 <- DOY[match(markingHO$DOYChecked,DOY$OrderedDOY),] 

##putting scaled values back in
markingHO$SDOYChecked <- DOY2$ScaledDOY

##Checking Mcollev & MarkingHO w/ scaling
head(Mcollev)
head(markingHO)

########################################################################################################
########################################################################################################
####################################### ~~~~ Merging ~~~~ ##############################################
########################################################################################################
########################################################################################################

##merge landscape types with hand pollination data
marking <- merge(markingHO, field, by = "Chakra")

##merge collection events with hand polliantion data 
marking2 <- merge(marking, Mcollev, by.x = "CollectingEvent" , by.y =
                    "Coll_Event")

##make unique matching column for trees and marking2 
trees$Chakra_Tree <- paste(trees$Chakra, trees$Tree, sep = "_")
marking2$Chakra_Tree <- paste(marking2$Chakra.x, marking2$Tree, sep = "_")

##merge marking2 and trees
marking3 <-  merge(marking2, trees, by = "Chakra_Tree")

##take needed columns
marking4 <- marking3[,c("CollectingEvent", "Chakra.x", "Type.x", 
                        "slope", "northness", "eastness", "Tree.x",
                        "Treatment", "Branch", "Dist", "Ht", "Diam",
                        "Trt", "DOYChecked", "SDOYChecked", "Fruit (y/n)", 
                        "DOYMarked", "SDOYMarked",
                        "Time_Start", "AvgTemp", "AvgWeather",
                        "Trunk_Cir",
                        "Can_Ht", "Water", "PerShade", "PerCover",
                        "Type.y", "DaysFirst", "DaysRecent",
                        "SDaysFirst", "SDaysRecent", "NumTreat")] ##add in DaysFirst, Recent and NumTreat

colnames(marking4) <- c("Coll_Event", "Chakra", "Type", 
                        "slope", "northness", "eastness", "Tree",
                        "Treatment", "Branch", "Distance", "Height", "Diameter",
                        "Pollination", "DOYChecked", "SDOYChecked", "Fruit", 
                        "DOYMarked", "SDOYMarked",
                        "Time_Start", "AvgTemp", "AvgWeather",
                        "Trunk_Cir",
                        "Can_Ht", "Water", "PerShade", "PerCover",
                        "Tree_Type", "DaysFirst", "DaysRecent", 
                        "SDaysFirst", "SDaysRecent", "NumTreat")


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

marking5$SDaysFirst <- as.numeric(marking5$SDaysFirst)
marking5$SDaysRecent <- as.numeric(as.character(marking5$SDaysRecent))

marking5$Trunk_Cir <-  as.numeric(marking5$Trunk_Cir)
marking5$Can_Ht<-  as.numeric(marking5$Can_Ht)
marking5$PerShade <-  as.numeric(marking5$PerShade)
marking5$PerCover <-  as.numeric(marking5$PerCover)

marking5$Height <-  as.numeric(marking5$Height)
marking5$Distance <-  as.numeric(marking5$Distance)


################################## ~~~~ Marking6 - End Edits ~~~~ #######################################
marking6 <- marking5

########################################################################################################
########################################################################################################
#################################### ~~~~ Correlations ~~~~ ############################################
########################################################################################################
########################################################################################################

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
##Trunk_Cir and Can_Ht are not really correlated

pairs(~ AvgTemp + DOYMarked + Time_Start + AvgWeather + DiffDOY +
      DaysFirst + DaysRecent + NumTreat, 
      data=marking6, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
##DOYMarked is 0.70 correlated with DaysFirst which makes sense...

pairs(~ slope + Type + northness + eastness,
      data=marking6, lower.panel=panel.smooth,
      upper.panel=panel.cor, pch=20)
##slope  & eastness very correlated (0.81) 

########################################################################################################
########################################################################################################
######################################## ~~~~ PCAs ~~~~ ################################################
########################################################################################################
########################################################################################################

##PCAFlower - Don't need
# flower.var <- marking6[,c("Distance", "Diameter")]
# PCA.Flower <- prcomp(flower.var)
# sd_flower <- PCA.Flower$sdev
# var_flower <- (sd_flower)^2
# prop_varF <- var_flower/sum(var_flower)
# marking6$PCAFlower<- PCA.Flower$x[,1]


#PCA Tree - Don't need
# Tree.var <- marking6[,c("Can_Ht","Trunk_Cir")]
# PCA.Tree <- prcomp(Tree.var)
# sd_Tree <- PCA.Tree$sdev
# var_Tree <- (sd_Tree)^2
# prop_varT <- var_Tree/sum(var_Tree)
# marking6$PCATree<- PCA.Tree$x[,1]


##PCA Weather - Don't need
##(PRComp function can't run with NAs, 
##Need to take NA out of Avg Temp and Avg Weather) ID (collevent/treenumber/Flower)
# is.na(marking6)
# na.omit(marking6)

# Weather.var <- marking7[,c("AvgTemp","AvgWeather")]
# PCA.Weather <- prcomp(Weather.var)
# sd_Weather <- PCA.Weather$sdev
# var_Weather <- (sd_Weather)^2
# prop_varW <- var_Weather/sum(var_Weather)
# marking6$PCAWeather<-PCA.Weather$x[,1]

########################################################################################################
########################################################################################################
#################################### ~~~~ Final Edits ~~~~ #############################################
########################################################################################################
########################################################################################################

##making tree factors
marking6$TreeF <- with(marking6, factor(Chakra:Tree))

##Making Fruit column binomial 
marking6$Fruit <- (marking6$Fruit == "Y")*1

##trying to scale DiffDOY correctly
marking6$UniqueDiffDOY <- paste(marking6$Coll_Event, marking6$DiffDOY, sep = "_")
ScaledDiffDOY<- scale(as.numeric(sapply(strsplit(as.character(unique(marking6$UniqueDiffDOY)),"_"), "[", 2)))
ScaledDiffDOY
OrderedDiffDOY <- unique(marking6$UniqueDiffDOY)
OrderedDiffDOY
DiffDOY <- as.data.frame(cbind(ScaledDiffDOY, OrderedDiffDOY))
colnames(DiffDOY) <- c("ScaledDiffDOY", "OrderedUniqueDiffDOY")
DiffDOY$OrderedDiffDOY <- sapply(strsplit(as.character(unique(marking6$UniqueDiffDOY)),"_"), "[", 2)

##ordering scaled values correctly
DiffDOY2 <- DiffDOY[match(marking6$DiffDOY, DiffDOY$OrderedDiffDOY),] 

##putting scaled values back in
marking6$SDiffDOY <- as.numeric(as.character(DiffDOY2$ScaledDiffDOY))

##Scaling Flower level variables
marking6$Distance <- scale(marking6$Distance)
marking6$Diameter <- scale(marking6$Diameter)
marking6$Height <- scale(marking6$Height)

##Scaling variables
markingScaled <- marking6

##taking only columns needed

markingScaled2 <- markingScaled[, c("Fruit", "Leaves", "Banana", "Treatment", "Pollination", 
                                    "Type", "slope", "northness", "eastness", 
                                    "Distance", "Diameter", "Height",
                                    "SDOYChecked", "SDOYMarked", "SDiffDOY",
                                    "Time_Start", "AvgWeather","AvgTemp", 
                                    "Water", "Trunk_Cir", "Can_Ht", "Tree_Type",
                                    "PerCover", "PerShade", 
                                     "SDaysFirst", "SDaysRecent", "NumTreat", 
                                     "Site", "Chakra", "Field", "TreeF", "Tree", "Coll_Event")] 

####Checking for NAs that need to be taken out 
sapply(markingScaled2, function(x) sum(is.na(x)))

##Some are treated as factors! 

## 72 NAs in AvgTemp Columns (= 72 data points)
markingScaledAT <- markingScaled2[!is.na(markingScaled2$AvgTemp),]

#################################### ~~~~ Saving Dataframe ~~~~ ########################################
save(markingScaledAT, file =
       "~/Dropbox/Analyses16/Data/MarkingFlowerLvl_Ready.RData")


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
       "~/Dropbox/Analyses16/Data/MarkingTreeLvl_Ready.RData")
 

########################################################################################################
########################################################################################################
######################################### ~~~~ Unused ~~~~ #############################################
########################################################################################################
########################################################################################################

##non-binomial #s 

prop <- function(x) {
  p <- sum((x == "1")*1)/length(x)
  p
}

props <- aggregate(markingScaled2$Fruit, 
                    by= list(markingScaled2$TreeF, 
                            markingScaled2$DOYChecked, markingScaled2$DOYMarked,
                            markingScaled2$Pollination, markingScaled2$Trunk_Cir, 
                            markingScaled2$Can_Ht, markingScaled2$Type, markingScaled2$Time_Start, 
                            markingScaled2$AvgTemp, markingScaled2$Leaves, markingScaled2$Banana,
                            markingScaled2$Tree_Type, markingScaled2$PerShade, markingScaled2$Water,
                            markingScaled2$PerCover, markingScaled2$DiffDOY,
                            markingScaled2$DaysFirst,markingScaled2$DaysRecent,
                            markingScaled2$PCATree, markingScaled2$PCAWeather),
                    FUN = prop)
colnames(markingScaled2)

colnames(props) <- c("TreeF", 
                     "DOYChecked","DOYMarked",
                     "Pollination", "Trunk_Cir", 
                     "Can_Ht", "Type", "Time_Start", 
                     "AvgTemp","Leaves", "Banana",
                     "Tree_Type", "PerShade", "Water",
                     "PerCover", "DiffDOY","DaysFirst","DaysRecent","PCATree", "PCAWeather", "Prop")

Props.hand <- props[props$Pollination == "H", ]
Props.open <- props[props$Pollination == "O", ]
Testcol.hand <- paste(Props.hand$TreeF, Props.hand$DOYChecked)
Testcol.open <- paste(Props.open$TreeF, Props.open$DOYChecked)
Testcol.open %in% Testcol.hand
Testcol.hand %in% Testcol.open
Props.open[137,]
Props.open <- Props.open[c(1:136,138:164),]
head(Props.open$TreeF)
head(Props.hand$TreeF)

props2<- Props.hand
colnames(props2)[21]<-"p.hand"

props2$p.open <- Props.open$Prop


props2$Diff <- props2$p.hand-props2$p.open

props2$Rarefied <- (props2$p.hand-props2$p.open)/props2$p.hand


props2$Chakra <- sapply(strsplit(as.character(props2$TreeF), ":"), "[", 1)

props2$Site <- paste(sapply(strsplit(as.character(props2$TreeF), ""), "[", 1), 
                     sapply(strsplit(as.character(props2$TreeF), ""), "[", 2), sep = "")

props2$Types <- sapply(props2$Type, function(x) if (x == "1") {"2.Ag"} else if (x == "2") {"1.Frag"} else {"3.For"})

##no difference between frag and forest (as expected - only one ag field so not a usable result)
  

pollmodel <- lmer(Diff ~ Leaves*Banana +
                    DOYMarked+ DiffDOY+ DaysFirst+ DaysRecent +
                    PCATree + Tree_Type+
                    PCAWeather +
                    PerCover + PerShade + 
                    Water + Time_Start +
                    (1|Type) + (1|TreeF) + (1|Site) + (1|Chakra),
                  data =  props2)


warnings()
qqnorm(resid(pollmodel))
qqline(resid(pollmodel))

summary(pollmodel)
search
tt <- getME(pollmodel,"theta")
ll <- getME(pollmodel,"lower")
min(tt[ll==0])
##Parsing out variance. Chakra and Type show 0 variance and 0 Std.Dev. 

pollmodel <- lmer(Diff ~ Leaves*Banana*DaysFirst+
                    DOYMarked+ DiffDOY+ DaysRecent +
                    PCATree + Tree_Type+
                    PCAWeather +
                    PerCover + PerShade + 
                    Water + Time_Start +
                 (1|TreeF) + (1|Site) +  (1|Type),
                  data =  props2)
tt <- getME(pollmodel,"theta")
ll <- getME(pollmodel,"lower")
min(tt[ll==0])
hist(props2$Diff)
par(mfrow=c(1,1))
elibrary(DHARMa)
hist(props2$Diff)
simulationOutput <- simulateResiduals(fittedModel = pollmodel, n = 10000) 

pollmodel <- lmer(Diff ~ Pollination*Banana*Leaves +
                    DOYMarked + DiffDOY+ DaysRecent + DaysFirst +
                    PCATree + Tree_Type+
                    PCAWeather +
                    PerCover + PerShade + 
                    Water + Time_Start +
                    (1|TreeF) + (1|Site) + (1|Type),
                  data =  props2)
##Plot it 
plotSimulatedResiduals(simulationOutput = simulationOutput)


