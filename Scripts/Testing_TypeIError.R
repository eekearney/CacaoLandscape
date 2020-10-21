##testing the Type I error rate in model

###set up
rm(list=ls())
setwd("~/")

##Libraries

##glmms
library(lme4)
library(lmerTest)
 

##Load
load("~/MEGA/Analyses16/Data/MarkingFlowerLvl_20190121.RData") 

##Simple Model
load("~/MEGA/Analyses16/Data/Marking_SimpleModel_190121.RData")

##Looking at model output 
summary(global)

########################### Scaling Data ##############################
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


########################################################################
########################################################################
####################### Swapping Classifications #######################
########################################################################
########################################################################


################################################################
      ################### Functions ###################
################################################################

#################### Swapping Pollination Treatment ####################
## Making random treatments - Switching up hand/open pollination
rand.Treatment <- function(reps, data, column, othercolumn){
  ## reps = number of data frames to be generated 
  ## data = dataframe 
  ## column = number of the column to be randomized
  ## othercolumn = if interaction, a term to be used to label the other column

  test.dats <- vector(length = reps, mode = "list")
  
  for (p in 1:reps){
      dd <- data
      set.seed(54321*p+p)
      q <- dd[,column]
      r <- sample(q, length(q), replace = FALSE)
      dd$PollRand <- r
      dd$MgmtRand <- dd[,othercolumn]
    test.dats[[p]] <- dd
  }
  test.dats
}

#################### Swapping Management Strategies ####################
rand.Type <- function(reps, data, column, list, othercolumn){
  ## reps = number of data frames to be generated 
  ## data = dataframe 
  ## column = number of the column to be randomized
  ## list = number of the column of the trees (unique)
  ## othercolumn = if interaction, a term to be used to label the other column

  test.dats <- vector(length = reps, mode = "list")
  
  for (p in 1:reps){
      dd <- data
      set.seed(54321*p+p)
      q <- as.character(dd[,column])
      trees <- unique(dd[,list])
      r <- sample(unique(q), length(trees), replace = TRUE)
      dd$MgmtRand <- rep(NA, dim(dd)[1])
      for (i in 1:length(trees)){
        dd$MgmtRand[dd[,list] == trees[i]] <- rep(r[i], length(dd$MgmtRand[dd[,list] == trees[i]]))
      }
      dd$MgmtRand <- as.factor(dd$MgmtRand)
      dd$PollRand <- dd[,othercolumn]
    test.dats[[p]] <- dd
  }
  test.dats
}

#################### Swapping Two Fixed Effects - Int ####################
rand.Int <- function(reps, data, singlecolumn, factorcolumn, list){
  ## reps = number of data frames to be generated 
  ## data = dataframe 
  ## singlecolumn = number of column where one line = randomization level
  ## factorcolumn = number of column where randomization should be performed at a higher level/grouping
  ## list = number of the column of the grouping factor (unique) - for factor column
  test.dats <- vector(length = reps, mode = "list")
  
  for (p in 1:reps){
      dd <- data
      #single randomization
      s <- dd[,singlecolumn]
      set.seed(54321*p+p)
      t <- sample(s, length(s), replace = FALSE)
      dd$PollRand <- t
      ##randomization @ factor level
      q <- as.character(dd[,factorcolumn])
      factor <- unique(dd[,list])
      set.seed(54321*p+p)
      r <- sample(unique(q), length(factor), replace = TRUE)
      dd$MgmtRand <- rep(NA, dim(dd)[1])
      for (i in 1:length(factor)){
        dd$MgmtRand[dd[,list] == factor[i]] <- rep(r[i], length(dd$MgmtRand[dd[,list] == factor[i]]))
      }
      dd$MgmtRand <- as.factor(dd$MgmtRand)
      #dd$OtherVar <- dd[,othercolumn] ## add "othercolumn" as a number to the call
    test.dats[[p]] <- dd
  }
  test.dats
}

################################################################
      ################### Testing ###################
################################################################

treat.rand <- rand.Treatment(10, markingScaled, column = 5, othercolumn = 4)
mgmt.rand <- rand.Type(10, markingScaled, list = 23, column = 4, othercolumn = 5)
int.rand <- rand.Int(10,  data = markingScaled, list = 23, singlecolumn = 5, factorcolumn = 4)

##using function to make random data sets 
randtreat.dats <- rand.Treatment(1000, markingScaled, column = 5, othercolumn = 4)

randtype.dats <- rand.Type(1000, markingScaled, list = 23, column = 4, othercolumn = 5)

randint.dats <- rand.Int(1000,  data = markingScaled, list = 23, singlecolumn = 5, factorcolumn = 4)

##saving randomized data sets
save(randtreat.dats, file = "~/MEGA/Analyses16/Data/Marking_Rand_20190129.RData")

save(randtype.dats, file = "~/MEGA/Analyses16/Data/Marking_RandMgmt_20190129.RData")

save(randint.dats, file = "~/MEGA/Analyses16/Data/Marking_RandInt_20190129.RData")

#######################################################################################################
#######################################################################################################
############################### Running in parallel --- ON MacPro!!!!!! ###############################
#######################################################################################################
#######################################################################################################


##libraries 
# install.packages("doMC")

##parallel 
library(MuMIn)
library(car)
library(boot)
library(snow)
library(doParallel)
library(arm)
library(AICcmodavg)
library(plyr)

##Making cluster and cores
cl <- makeCluster(5)
registerDoParallel(cl, cores = 5)

##Adding needed packages to cluster
clusterEvalQ(cl, {library(lme4)
                  library(lmerTest)
                  library(chron)
                  library(MuMIn)
                  library(car)
                  library(boot)
                  library(arm)
                  library(AICcmodavg)
                }
             )


##Dredge fails unless this is set
options(na.action = na.fail)

####################################################################
######################## === Analyzing === #########################
####################################################################

########################### TREATMENT #############################
#loading data back in
load("~/MEGA/Analyses16/Data/Marking_Rand_20190129.RData")

##Exporting data to cluster
clusterExport(cl, "randtreat.dats")

##Function - treat
fun.glmmRand <- function(dats, column, c.names) {
  #dats - dataframe for analysis
  #column - number of effects
  #c.names - names of effects
  
  coeffs <- vector(mode = "numeric", length = (2*column))
  names(coeffs) <- c.names

  ##Running linear models
  ##glmm time!
  glmm.test <- glmer(Fruit ~ PollRand*MgmtRand + PCATreat + DaysRecent + 
                  PCAFlower + PCAField + northness + AvgTemp + 
                  Trunk_Cir + 
                  PerCover + PerShade + Water + 
                  AvgWeather + Time_Start + Can_Ht +
                  (1|TreeF), 
                  data =  dats, family = binomial, 
                  control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e9))) ## converges! 

  # #Previous Version
  # glmm.test <- glmer(Fruit ~ TreatRand*Type + PCATemp + TreeType + AvgWeather +
  #                  (1|TreeF), data =  dats, family = binomial, 
  #                  control=glmerControl(optimizer="bobyqa",
  #                              optCtrl=list(maxfun=1e9)), na.action = na.omit)

  ##Putting coefs and pvs in dataframes
  for (i in 1:column) {
  coeffs[i] <- coef(summary(glmm.test))[i,1]
  coeffs[i+column] <- coef(summary(glmm.test))[i,4]
  }
  coeffs
}


##POLLINATION
column.names <- c("Coef:hand-intercept", "Coef:open", "Pval:hand-intercept",  "Pval:open")
test <- laply(.data = randtreat.dats[1:10], .fun = fun.glmmRand, 
  column = 2, c.names = column.names, .parallel = TRUE)

##Testing Time
ptm <- proc.time()
tester <- laply(.data = randtreat.dats[1:10], column = 2, 
  c.names = column.names, 
  .fun = fun.glmmRand, .parallel = TRUE)
proc.time() - ptm

ptm <- proc.time()
rand.treatcoeffs<- laply(.data = randtreat.dats, column = 2, 
  c.names = column.names, 
  .fun = fun.glmmRand, .parallel = TRUE)
proc.time() - ptm

save(rand.treatcoeffs, 
  file = "~/MEGA/Analyses16/Data/RandPoll_Coeffs_Results_20190129.RData")

############################### MGMT ###############################
#data
load("~/MEGA/Analyses16/Data/Marking_RandMgmt_20190129.RData")

##Exporting data to cluster
clusterExport(cl, "randtype.dats")

##Function - treat
fun.glmmRand <- function(dats, column, c.names) {
  
  coeffs <- vector(mode = "numeric", length = (2*column))
  names(coeffs) <- c.names

  ##Running linear models
  glmm.test <- glmer(Fruit ~ MgmtRand*PollRand + PCATreat + DaysRecent + 
                  PCAFlower + PCAField + northness + AvgTemp + 
                  Trunk_Cir + 
                  PerCover + PerShade + Water + 
                  AvgWeather + Time_Start + Can_Ht +
                  (1|TreeF), 
                  data =  dats, family = binomial, 
                  control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e9))) 
  ##Putting coefs and pvs in dataframes
  for (i in 1:column) {
  coeffs[i] <- coef(summary(glmm.test))[i,1]
  coeffs[i+column] <- coef(summary(glmm.test))[i,4]
  }
  coeffs
}


##testing type
ptm <- proc.time()
tester <- laply(.data = randtype.dats[1:10], column = 4, c.names = 
  c("Coef:NN-intercept", "Coef:LB", "Coef:LN", "Coef:NB", 
    "Pval:NN-intercept", "Pval:LB", "Pval:LN", "Pval:NB"), 
  .fun = fun.glmmRand, .parallel = TRUE)
proc.time() - ptm

##real type treatment 
ptm <- proc.time()
rand.mgmtcoeffs <- laply(.data = randtype.dats, column = 4, c.names = 
 c("Coef:NN-intercept", "Coef:LB", "Coef:LN", "Coef:NB", 
    "Pval:NN-intercept", "Pval:LB", "Pval:LN", "Pval:NB"), 
   .fun = fun.glmmRand, .parallel = TRUE)
proc.time() - ptm

##saving
save(rand.mgmtcoeffs, file = "~/MEGA/Analyses16/Data/RandMgmt_Coeffs_Results_20190129.RData")


######################## INTERACTION ########################

load("~/MEGA/Analyses16/Data/Marking_RandInt_20190129.RData")

##Exporting data to cluster
clusterExport(cl, "randint.dats")

fun.IntRand <- function(dats, effects, c.names) {

  coeffs <- vector(mode = "numeric", length = (2*length(effects)))
  names(coeffs) <- c.names

  ##Running linear models
  glmm.test <- glmer(Fruit ~ PollRand*MgmtRand + PCATreat + DaysRecent + 
                  PCAFlower + PCAField + northness + AvgTemp + 
                  Trunk_Cir + 
                  PerCover + PerShade + Water + 
                  AvgWeather + Time_Start + Can_Ht +
                  (1|TreeF), 
                  data =  dats, family = binomial, 
                  control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e9))) 
  ##Putting coefs and pvs in dataframes
  for (i in 1:length(effects)){
    z <- effects[i]
  coeffs[i] <- coef(summary(glmm.test))[z,1]
  coeffs[i+length(effects)] <- coef(summary(glmm.test))[z,4]
  }
  coeffs
}
 
# test <- fun.IntRand(randint.dats[[1]], effects = c(1:5,19:21), c.names = 
#   c("Coef:Intercept", "Coef:Open", "Coef:LB", "Coef:LN", "Coef:NB", 
#     "Coef:OpenLB", "Coef:OpenLN", "Coef:OpenNB",
#     "Pval:Intercept", "Pval:Open", "Pval:LB", "Pval:LN", "Pval:NB", 
#     "Pval:OpenLB", "Pval:OpenLN", "Pval:OpenNB")) 


##testing type
ptm <- proc.time()
tester <- laply(.data = randint.dats[1:10], effects = c(1:5,19:21), c.names = 
  c("Coef:Intercept", "Coef:Open", "Coef:LB", "Coef:LN", "Coef:NB", 
    "Coef:OpenLB", "Coef:OpenLN", "Coef:OpenNB",
    "Pval:Intercept", "Pval:Open", "Pval:LB", "Pval:LN", "Pval:NB", 
    "Pval:OpenLB", "Pval:OpenLN", "Pval:OpenNB"), 
  .fun = fun.IntRand, .parallel = TRUE)
proc.time() - ptm

##real type treatment 
ptm <- proc.time()
rand.intcoeffs <- laply(.data = randint.dats, effects = c(1:5,19:21), c.names = 
 c("Coef:Intercept", "Coef:Open", "Coef:LB", "Coef:LN", "Coef:NB", 
    "Coef:OpenLB", "Coef:OpenLN", "Coef:OpenNB",
    "Pval:Intercept", "Pval:Open", "Pval:LB", "Pval:LN", "Pval:NB", 
    "Pval:OpenLB", "Pval:OpenLN", "Pval:OpenNB"), 
    .fun = fun.IntRand, .parallel = TRUE)
proc.time() - ptm


save(rand.intcoeffs, file = "~/MEGA/Analyses16/Data/RandInt_Coeffs_Results_20190129.RData")


#### Looking at the numbers 

type.one <- function(x){
  p <- sum((x<0.05)*1)/1000
  p
}

##Treatment
load("~/MEGA/Analyses16/Data/RandPoll_Coeffs_Results_20190129.RData")

sum((rand.treatcoeffs[,4] < 0.05)*1)/1000 ##0.054

##Type 
load("~/MEGA/Analyses16/Data/RandMgmt_Coeffs_Results_20190129.RData")

apply(rand.mgmtcoeffs[,c(6:8)], 2, type.one)

# Pval:LB Pval:LN Pval:NB 
#   0.069   0.076   0.079 


##Interaction
load("~/MEGA/Analyses16/Data/RandInt_Coeffs_Results_20190129.RData")

apply(rand.intcoeffs[,c(10:16)], 2, type.one)

# Pval:Open    Pval:LB    Pval:LN   Pval:NB  Pval:OpenLB  Pval:OpenLN  Pval:OpenNB 
#   0.041       0.055      0.050      0.060       0.057       0.047      0.050 

      
