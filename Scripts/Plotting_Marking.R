##Plot results from model averaging
rm(list=ls())
setwd("~/")

##Libraries
library(lme4)
library(lmerTest)
##se estimates
library(merTools)

##tidy tools
library(tidyr)
library(dplyr)

##graphing
library(arm) ##for invlogit
library(plyr)
library(grid)
library(ggplot2) ##for plots
library(boot)
library(reshape2) 
library(RColorBrewer)

##Colors
myColors <- brewer.pal(6, 'Dark2')
ColsTreat <- c(myColors[2], myColors[2], myColors[1], myColors[1],
              myColors[6], myColors[6], myColors[5], myColors[5])

ColsPoll <- c(myColors[3],myColors[4])

##Functions
source("~/Box Sync//Analysis_Projects/FuncRe_RespDiv/Analyses/Functions/plotdata.R")
source("~/Box Sync//Analysis_Projects/FuncRe_RespDiv/Analyses/Functions/Multiplot.R")
source("~/Box Sync//Analysis_Projects/FuncRe_RespDiv/Analyses/Functions/predictIntervals.R")

fmt <- function(){
    function(x) format(x,nsmall = 3, scientific = FALSE)
  }

proportion.fun <- function(x) {
  sum(x)/length(x)
}

TreatConversion <- function(x){
  if (x == "LB"){
    y <- 4
  } else if (x == "LN") {
    y <- 3
  } else if (x == "NB"){
    y <- 2
  } else {
    y <- 1
  }
  y
}

##Load data
load("~/MEGA/Analyses16/Data/Marking_Halfway_20190121.RData") 
marking6$Fruit <- (marking6$Fruit == "Y")*1

##Simple Model
load("~/MEGA/Analyses16/Data/Marking_SimpleModel_190121.RData")

##Looking at model output 
summary(global)


###################################################################
######################### Sup Table: Sampling #####################
###################################################################

pretab.sam <- aggregate(marking6$Chakra, 
  by = list(marking6$Chakra, marking6$DOYMarked), length)
colnames(pretab.sam) <- c("Chakra", "DOY", "Num")

tab.sam <- aggregate(pretab.sam$Chakra, 
  by = list(pretab.sam$Chakra, pretab.sam$DOY), length)
colnames(tab.sam) <- c("Chakra", "DOY", "Num")

as.Week <- function(x){
  week <- floor(x/7)
  week
}

tab.sam$Week <- unlist(lapply(tab.sam$DOY, FUN = as.Week))
tab.sam$Week <- tab.sam$Week-39

##Make a short data frame
short.data <- tapply(tab.sam$Num, list(ROW = tab.sam$Chakra, 
        COLUMN = tab.sam$Week), sum)


##for LaTex table 
  #     &  1 & 2  & 3  & 4  & 5  & 6  & 7 \\  
  # CC1 &  1 & 0 &  1 &  1 &  1 & 0 & 0 \\
  # CC2 &  1 & 0 &  1 &  1 &  1 & 0 & 0 \\
  # RB1 & 0 & 0 &  1 &  1 & 0 &  1 &  1 \\
  # RB2 & 0 & 0 & 0 &  1 &  1 &  1 &  1 \\
  # SJ1 &  1 & 0 &  1 & 0 &  1 &  1 & 0 \\
  # SJ2 &  1 & 0 &  1 & 0 &  1 &  1 & 0 \\


####################################################################
####################################################################
######################### Treatment Plot ###########################
####################################################################
####################################################################
###chances of success across all types 
predictions.TreatPoll <- expand.grid(Pollination = c("H", "O"), Treatment = as.factor(c("ANN", "LB", "NB", "LN")), 
  PCAField = 0, northness = 0, DaysRecent = 0, PCATreat = 0, 
  AvgTemp = 0, AvgWeather = 0, Time_Start = 0,
  Trunk_Cir = 0, PerShade = 0, PerCover = 0,  Can_Ht = 0, Water = c("N", "Y"), 
  PCAFlower = 0, Fruit = 0)

pred.se <- predict.int(mod = global, dd = predictions.TreatPoll, y = "Fruit", family = "binomial")
pred.se$Treatment <- as.character(pred.se$Treatment)
##works! 9.12
##re.form fits random intercept at 0 (fully conditional)
## 01.21.19: getting an "unused arguments" warning --> Does not produce SEs any more, 
# recomends bootMer but this doesn't work with "glmerMod" (or "S4") classes 
## 01.25.19: using Lauren's code & not worrying about the theory behind it...not best practices 

# ##trying a few things 
# test <- bootMer(global, predict, nsim = 10)
# test< .Last.value

# test2 <- predictInterval(global, newdata = predictions.TreatPoll, n.sims = 10)

# predictions.TreatPoll$Fruit <- pred.se$fit
# ## predictions.TreatPoll$SE <-  pred.se$se ##need to figure this out now...

# predictions.TreatPoll2 <- predictions.TreatPoll[predictions.TreatPoll$Water == "N",]

##weights of each type 
marking6 %>% group_by(Pollination, Treatment, Water) %>%
       dplyr::summarise(count = n()) -> real

real1 <- as.data.frame(real)

real1 %>% group_by(Pollination) %>% 
    dplyr::summarise(sum = sum(count)) %>% 
    left_join(real) %>% 
    mutate(weight = count/sum) -> t

t$Treatment <- as.character(t$Treatment)
t$Treatment[t$Treatment == "NN"] <- "ANN"

t %>% right_join(pred.se, by = c("Pollination", "Treatment", "Water")) -> real2 ##merge together

real3 <- real2[, c("Pollination", "Treatment", "Water", "Fruit", "phi", "plo", "count", "sum", "weight")]

real3$weight[is.na(real3$weight)] <- 0

###getting weighted scores 
real3 %>% mutate(mean = Fruit*weight, upper = phi*weight, lower = plo*weight) -> real4

real4 %>% group_by(Pollination) %>% 
    dplyr::summarize(mn = sum(mean), up = sum(upper), low = sum(lower)) -> mod.poll

##multiplying 


##making final
model.prop <- data.frame(Pollination = c("H", "O"), TreatN = c(0.5, 0), Success = c(s.hand, s.open))
model.prop$ConfInt <- c(e.hand*1.96, e.open*1.96)


##raw data  
raw.prop <- aggregate(marking6$Fruit, 
  by = list(marking6$Chakra, marking6$Tree, marking6$DOYMarked, marking6$Pollination), 
  proportion.fun) ##Treef doesn't exist in marking6 
colnames(raw.prop) <- c("Chakra", "Tree", "DOY", "Pollination", "Success")

raw.avgprop <- aggregate(raw.prop$Success, 
  by = list(raw.prop$Chakra, raw.prop$DOY, raw.prop$Pollination), 
  mean)
colnames(raw.avgprop) <- c("Chakra", "DOY", "Pollination", "Success")

##Plot Data sets
raw.avgprop$TreatN <- (raw.avgprop$Pollination == "H")*0.5

##Plot
par(mfrow = c(1,1), mar = c(4,5,3,2))
plot(Success ~ jitter(TreatN, factor = 5), data = raw.avgprop[raw.avgprop$Pollination == "O",], 
  xlim = c(-0.2, .7), ylim =c(0,0.8), xaxt = "n", 
  xlab = "", ylab = "Chance of Fruit Set", 
  pch = 20, col = alpha("black", 0.25))
mtext(text = c("Open", "Hand"),  at = c(0,.5), side = 1, line = 0.25)
mtext(text = c("Pollination"),  at = c(0.25), side = 1, line = 1.5)
points(Success ~ jitter(TreatN, factor = 10), data = raw.avgprop[raw.avgprop$Pollination == "H",], 
  pch = 17, col = alpha("black", 0.25))
points(Success ~ TreatN, data = model.prop[model.prop$Pollination == "O",], pch = 21)
points(Success ~ TreatN, data = model.prop[model.prop$Pollination == "H",], pch = 2)
arrows(model.prop$TreatN, model.prop$Success-model.prop$ConfInt, 
  model.prop$TreatN, model.prop$Success+model.prop$ConfInt,
   length=0.025, angle=90, code=3, lwd = 1.5)

#Save
dev.print(pdf, "~/Dropbox/Analyses16/Results/20181023_Pollination.pdf", width = 6, height = 5)
####################################################################
####################################################################
####################### Dot & Whisker Plot #########################
####################################################################
####################################################################
##parameters with # of models and relative importance 

##frmat to make dot_whisker plot
sum.global <- summary(global)
pretibble <- as.data.frame(sum.global$coef)

pretibble$term <- rownames(sum.global$coef)

colnames(pretibble)[1:2] <- c("estimate", "std.error")

pretibble$Name <- c("Intercept", "Pollination - Open", 
    "Leaves & Pseudostems (L&P)", "Leaves Only", "Pseudostems Only", 
    "PCA - DOY & Treatment", "DaysRecent", "PCA - Branch",      
    "PCA - Field", "Northness", "Mean Temperature", "Trunk Circumference", 
    "% Cover", "% Shade", "Water",    
    "Mean Weather", "Start Time", "Canopy Height",
    "Pollination * L&P",
    "Pollination * Leaves Only", "Pollination * Pseudostems Only")

pretibble$ConfInt <- pretibble$std.error*1.96

include_list <- c("(Intercept)", "PollinationO", 
    "TreatmentLB", "TreatmentLN", "TreatmentNB", 
    "PollinationO:TreatmentLB", "PollinationO:TreatmentLN", "PollinationO:TreatmentNB", 
    "PCAFlower", "PerCover", "WaterY", 
    "northness", "AvgWeather", 
    "PCAField", 
    "PCATreat", "DaysRecent", 
    "AvgTemp",  "Time_Start", 
    "Trunk_Cir", "Can_Ht", "PerShade")

pretibble <- pretibble[include_list,]

pretibble$num <- seq(from = dim(pretibble)[1], to  = 1, by = -1)

##normal plot
par(mar = c(5, 13, 3, 2))
plot(num ~ estimate, data = pretibble[-1,], 
    xlim = c(-6, 3.5), type = "n", 
    yaxt = "n", ylab = "", xlab = "Parameter Estimate")
points(num ~ estimate, data = pretibble[c(2,6,9:11),], pch = 16, cex = 1.2)
points(num ~ estimate, data = pretibble[c(12:13),], pch = 21,  
  cex = 1.2, bg = alpha("black", 0.3))
points(num ~ estimate, data = pretibble[c(3:5,7,8,13:21),], 
    pch = 1, cex = 1.2)
arrows(pretibble$estimate-pretibble$ConfInt, pretibble$num,
  pretibble$estimate+pretibble$ConfInt, pretibble$num, 
  length = 0, angle=90, code=3, lwd = 1.5)
abline(v = 0, lty = "dashed")
mtext(text = pretibble$Name[-1],  at = pretibble$num[-1], side = 2, line = 0.25, las = 2)


##save
dev.print(pdf, "~/MEGA/Analyses16/Results/20190303_Parameters.pdf", width = 8, height = 5)

####################################################################
####################################################################
####################### Type*Treatment Plot ########################
####################################################################
####################################################################

#Prediction from model for predetermined values 
predictions.TreatPoll <- expand.grid(Pollination = c("H", "O"), Treatment = as.factor(c("ANN", "LB", "NB", "LN")), 
  PCAField = 0, northness = 0, DaysRecent = 0, PCATreat = 0, 
  AvgTemp = 0, AvgWeather = 0, Time_Start = 0,
  Trunk_Cir = 0, PerShade = 0, PerCover = 0,  Can_Ht = 0, Water = c("N", "Y"), 
  PCAFlower = 0, Fruit = 0)

pred.se <- predict(global, newdata = predictions.TreatPoll, 
  type = "response", backtransform = TRUE, re.form = NA, se.fit = TRUE)
##works! 10.12
##re.form fits random intercept at 0 (fully conditional)

predictions.TreatPoll$Fruit <- pred.se$fit
predictions.TreatPoll$SE <-  pred.se$se

###Graphing
predictions.TreatPoll2 <- predictions.TreatPoll[predictions.TreatPoll$Water == "N",]
predictions.TreatPoll2$ConfInt <- predictions.TreatPoll2$SE*1.96
predictions.TreatPoll2$Low <- predictions.TreatPoll2$Fruit - predictions.TreatPoll2$ConfInt
predictions.TreatPoll2$High <- predictions.TreatPoll2$Fruit + predictions.TreatPoll2$ConfInt
predictions.TreatPoll2$TreatN <- unlist(lapply(predictions.TreatPoll2$Treatment, FUN = TreatConversion))

##Raw Data
raw.prop <- aggregate(marking6$Fruit, 
  by = list(marking6$Chakra, marking6$Tree, marking6$DOYMarked, 
            marking6$Treatment, marking6$Pollination), 
  proportion.fun)
colnames(raw.prop) <- c("Chakra", "TreeF", "DOY", "Treatment", "Pollination", "Fruit")

raw.avgprop <- aggregate(raw.prop$Fruit, 
  by = list(raw.prop$Chakra, raw.prop$DOY, raw.prop$Treatment, raw.prop$Pollination), 
  mean)
colnames(raw.avgprop) <- c("Chakra", "DOY", "Treatment", "Pollination", "Fruit")
raw.avgprop$TreatN <- unlist(lapply(raw.avgprop$Treatment, FUN = TreatConversion))
raw.avgprop$TreatN[raw.avgprop$Pollination == "O"] <- raw.avgprop$TreatN[raw.avgprop$Pollination == "O"] + 
                                                            rep(seq(from = -0.1, to = -0.35, length.out = 24), 4) 
raw.avgprop$TreatN[raw.avgprop$Pollination == "H"] <- raw.avgprop$TreatN[raw.avgprop$Pollination == "H"] + 
                                                            rep(seq(from = 0.1, to = 0.35, length.out = 24), 4) 

##labels
treats <- c("No Additions", "Banana", "Leaves", "Banana & Leaves")

##Plot 
par(mfrow = c(1,1), mar = c(4,5,3,2))
plot(Fruit ~ TreatN, data = raw.avgprop[raw.avgprop$Pollination == "O",], 
  xlab = "", ylab = "Chance of Fruit Set", pch = 16, cex = 0.9, 
  col = alpha("black", 0.25), ylim = c(0,1), xlim = c(0.5,4.5), xaxt = "n")
points(Fruit ~ TreatN, data = raw.avgprop[raw.avgprop$Pollination == "H",], 
  pch = 17, cex = 0.9, col = alpha("black", 0.25))
points(Fruit ~ TreatN, data = predictions.TreatPoll2[predictions.TreatPoll2$Pollination == "O",], 
  pch = 21, cex = 1.3)
points(Fruit ~ TreatN, data = predictions.TreatPoll2[predictions.TreatPoll2$Pollination == "H",], 
  pch = 24)
arrows(predictions.TreatPoll2$TreatN, predictions.TreatPoll2$Low, predictions.TreatPoll2$TreatN, 
    predictions.TreatPoll2$High, length=0.02, angle=90, code=3)
legend(x = 0.45, y = 1, legend = c("Hand ", "Open"), cex = 0.7, pch = c(17,19))
mtext(text = treats,  at = c(1:4), side = 1, line = 0.25)
mtext(text = "Management Treatment",  at = c(2.475), side = 1, line = 1.75)

##save
dev.print(pdf, "~/Dropbox/Analyses16/Results/20181023_Treat.pdf", 
  width = 7, height = 5)







