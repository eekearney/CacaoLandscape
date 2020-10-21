##Analyze Econ with MC
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

##piping  
library(dplyr)
library(plyr)

##MonteCarlo
library(MonteCarlo)

## Load in spreadsheets
load("~/MEGA/Analyses16/Data/Norm_Poll_Est.RData")
load("~/MEGA/Analyses16/Data/Hand_Poll_Est.RData")
load("~/MEGA/Analyses16/Data/LB_Poll_Est.RData")
load("~/MEGA/Analyses16/Data/FruitSurv_Est.RData")
load("~/MEGA/Analyses16/Data/Prices_190221.RData")
load("~/MEGA/Analyses16/Data/Labor_Market_Est.RData")

##functions
source("~/MEGA/Analyses16/Functions/NetRevenue.R")

#################################################################
########################## MC Prep ##############################
#################################################################

##varying parameters
scen_grid <- c("hand", "LB", "norm")
labor_grid <- c("low", "med", "high")
cont_grid <- c("no") ##not having "yes" because it makes things less clear 

param_list=list("scen"=scen_grid, "labor"=labor_grid, "cont"=cont_grid)

##need to make sure these are in workspace:
norm.beta
hand.beta
LB.beta
fruit.beta
head(prices2)
shapes

#################################################################
########################## MC Run ###############################
#################################################################

# ptm <- proc.time()
# tester<-MonteCarlo(func=net.revenue, nrep=100, param_list=param_list) 
# # Stop the clock
# proc.time() - ptm


# ##parallelized 
# ptm <- proc.time()
# tester<-MonteCarlo(func=net.revenue, nrep=100, param_list=param_list, ncpus = 10) 
# proc.time() - ptm

#### hmmm. seems that parallelization is not helping it to be faster 

# ptm <- proc.time()
# tst <- MonteCarlo(func=net.revenue, nrep=15, param_list=param_list)
# proc.time() - ptm

# ptm <- proc.time()
# tst <- MonteCarlo(func=net.revenue, nrep=15, param_list=param_list, ncpus = 15)
# proc.time() - ptm

Rev_result3 <- MonteCarlo(func=net.revenue, nrep=10000, param_list=param_list, ncpus = 15)

#save
save(Rev_result3, 
	file = "~/MEGA/Analyses16/Data/Revenue_10k_190227.RData")

# ##second run see if it is stable 
# Rev_result2<-MonteCarlo(func=net.revenue, nrep=1000, param_list=param_list, ncpus = 5)

# #save
# save(Rev_result2, 
# 	file = "~/MEGA/Analyses16/Data/Revenue_2_1000_190222.RData")


#################################################################
###################### Analyze results ##########################
#################################################################

load("~/MEGA/Analyses16/Data/Revenue_10k_190227.RData")

Rev_result3 %>% MakeFrame() %>% 
	separate(scen, into = c("scen", "poll"), sep = "=") %>%
	separate(labor, into = c("lab", "labor"), sep = "=") %>%  
	select(poll, labor, fruit, net, tot, cost) -> rev1a

###Net
net.lm <- lm(net ~ poll*labor, data =rev1a)

net.aov <- aov(net ~ poll*labor, data =rev1a)
TukeyHSD(net.aov)

###Costs
cost.lm <- lm(cost ~ poll*labor, data =rev1a)


##Fruits 
fruit.lm <- lm(fruit ~ poll, data =rev1a)
summary(fruit.lm)

##Tot Rev
tot.lm <- lm(tot ~ poll, data =rev1a)
summary(tot.lm)

tot.aov <- aov(tot ~ poll, data =rev1a)
TukeyHSD(tot.aov)










