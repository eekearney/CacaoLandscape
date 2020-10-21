##Analyzing marking (fruit set) data - Cacao'15
rm(list=ls())
setwd("~/")

## Libraries
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("chron")
# install.packages("MuMIn")
# install.packages("car")
# install.packages("boot")
# install.packages("DHARMa")
# install.packages("snow")
# install.packages("doParallel")
# install.packages("arm")
# install.packages("AICcmodavg")
# install.packages("dotwhisker")
# install.packages("broom")
# install.packages("dplyr")

#general
library(chron)

##analyses
library(lme4)
library(lmerTest)
library(DHARMa)

#parallel
library(MuMIn)
library(car)
library(boot)
library(snow)
library(doParallel)
library(arm)
library(AICcmodavg)

##plot the coefs & SEs from avg'ed models 
library(dotwhisker)
library(broom)
library(dplyr)

##Load
load("~/MEGA/Analyses16/Data/MarkingFlowerLvl_20190121.RData") 

## NN, LN, NB, LB = Treatment
## or Leaves*Banana 

##reordering so that NN is the intercept
markingFinal$Treatment <- as.character(markingFinal$Treatment)
markingFinal$Treatment[markingFinal$Treatment == "NN"] <- rep("ANN", 
      length(markingFinal$Treatment[markingFinal$Treatment == "NN"]))
markingFinal$Treatment <- as.factor(markingFinal$Treatment)

##Scaling 
markingScaled <- markingFinal
markingScaled[,c("northness", "PCAField", "PCATreat", "DiffDOY",
                  "AvgWeather","AvgTemp", 
                  "DaysRecent", "Trunk_Cir", "Can_Ht", 
                  "PerCover", "PerShade", "PCAFlower")] <- scale(markingScaled[,c("northness", 
                                                          "PCAField", "PCATreat", "DiffDOY",
                                                          "AvgWeather","AvgTemp", 
                                                          "DaysRecent", "Trunk_Cir", "Can_Ht", 
                                                          "PerCover", "PerShade", "PCAFlower")])
markingScaled$Time_Start <- scale(as.numeric(markingScaled$Time_Start))

markingScaled$Plot <- paste(markingScaled$Chakra, markingScaled$Treatment, sep = "_")

##glmm time!
global <- glmer(Fruit ~ Pollination*Treatment + PCATreat + DaysRecent + 
                  PCAFlower + PCAField + northness + AvgTemp + 
                  Trunk_Cir + 
                  PerCover + PerShade + Water + 
                  AvgWeather + Time_Start + Can_Ht +
                  (1|TreeF) + (1|Chakra) + (1|Site), 
                  data =  markingScaled, family = binomial, 
                  control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e9))) ## converges! 
##ISSUE:singular fit
getME(global,"theta") ##Site & Chakra are very small 
##taking (1|Site) out because its intercept is estimated to be 0 

global2 <- glmer(Fruit ~ Pollination*Treatment + PCATreat + DaysRecent + 
                  PCAFlower + PCAField + northness + AvgTemp + 
                  Trunk_Cir + 
                  PerCover + PerShade + Water + 
                  AvgWeather + Time_Start + Can_Ht +
                  (1|TreeF) + (1|Chakra), 
                  data =  markingScaled, family = binomial, 
                  control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e9))) ## converges! 
##ISSUE:singular fit
getME(global2,"theta") ##Site & Chakra are very small 
##taking (1|Chakra)  out because its intercept is estimated to be 0 

global3 <- glmer(Fruit ~ Pollination*Treatment + PCATreat + DaysRecent + 
                  PCAFlower + PCAField + northness + AvgTemp + 
                  Trunk_Cir + 
                  PerCover + PerShade + Water + 
                  AvgWeather + Time_Start + Can_Ht + 
                  (1|TreeF), 
                  data =  markingScaled, family = binomial, 
                  control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e9))) ## converges! 
##No issues 
getME(global3,"theta") ##Site & Chakra are very small 

AICc(global)
AICc(global2)
AICc(global3) ##best model based on AICc values 

##look at pollmodel
r.squaredGLMM(global3) ##fixed effects explain 33% of the variation, 41% with random effects 

global <- global3 

##Save 
save(global, file = 
    "~/MEGA/Analyses16/Data/Marking_SimpleModel_190121.RData")


# ##Model Averaging Approach - not supported in this case 
# ## ~~~~~~~~~~~~~~~~~~ ##Uses Parallel##  ~~~~~~~~~~~~~~~~~~~~~~ ##

# ##Libraries
# ##install.packages(c("snow", "doParallel", "lme4", "lmerTest",
# ##"chron", "MuMIn", "car", "boot"))

# ##Making cluster and cores
# cl <- makeCluster(10)
# registerDoParallel(cl, cores = 6)

# ##Adding needed packages to cluster
# clusterEvalQ(cl, {library(lme4)
#                   library(lmerTest)
#                   library(chron)
#                   library(MuMIn)
#                   library(car)
#                   library(boot)
#                 }
#              )

# ##Model for averaging = global from above

# ##Exporting model to cluster
# clusterExport(cl, "markingScaled")

# ##Dredge fails unless this is set
# options(na.action = na.fail)

# # Start the clock!
# ptm <- proc.time()

# modelsOut = pdredge(global, cluster = cl)

# # Stop the clock
# proc.time() - ptm

# save(modelsOut, file =
#      "~/MEGA/Analyses16/Data/Marking_ModelAvgOutput_181012B.RData") 

# ## ~~~~~~~~~~~~~~~~~~ ##End Parallel##  ~~~~~~~~~~~~~~~~~~~~~~ ##

# ##Load model selection
# load("~/MEGA/Analyses16/Data/Marking_ModelAvgOutput_181012B.RData") 

# # ##Looking at model output - don't need to look at full model 
# # avg <-  model.avg(modelsOut) ##takes some time - 1280 models

# # sumavg <- summary(avg) ##will take a while 

# # sumavg$coefmat.full ##table of coefficients and values for full model 
# # sumavg$coefmat.subset ##table of coefficients and values for full model 

# ##Look at best fitting models only - top 
# models.delta2 <- get.models(modelsOut, subset = delta < 2) #Warning takes a while 
#     # I recommend saving these separately

# save(models.delta2, file = 
#   "~/MEGA/Analyses16/Data/Marking_Delta2Models_2018101.RData")

# ##load
# load("~/MEGA/Analyses16/Data/Marking_Delta2Models_20181012.RData")

# ##how many? 
# length(models.delta2) # 13 in total - 2 convergence issues

# ##get avg model
# avg.delta2 <- model.avg(models.delta2, revised.var = TRUE, fit = TRUE) 
# ##revised.var = True gives st errors from Burnham and Anderson (2004, equation 4) 
# ## Burnham, K. P. and Anderson, D. R. (2004) Multimodel inference - understanding AIC and BIC in model selection. Sociological Methods & Research 33(2): 261-304.

# sum.avg.delta2 <- summary(avg.delta2)
# sum.avg.delta2$coefmat.full


# ##Look at pretty good fitting models only - ~top 
# models.delta5 <- get.models(modelsOut, subset = delta < 5) #Warning takes a while 
#     #I recommend saving these separately

# save(models.delta5, file = 
#   "~/MEGA/Analyses16/Data/Marking_Delta5Models_20181012.RData")

# ##how many? 
# length(model.delta5)



# avg.delta5 <- model.avg(modelsOut, subset = delta < 5, revised.var =
#                         TRUE)

# sum.avg.delta5 <- summary(avg.delta5)
# sum.avg.delta5$coefmat.full

