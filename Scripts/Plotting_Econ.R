##Plotting Econ Analysis Results
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
library(tidyr)

##MonteCarlo
library(MonteCarlo)

##Data
# load("~/MEGA/Analyses16/Data/Revenue_Final_190221.RData")

load("~/MEGA/Analyses16/Data/Revenue_1_1000_190222.RData")
load("~/MEGA/Analyses16/Data/Revenue_2_1000_190222.RData")
load("~/MEGA/Analyses16/Data/Revenue_10k_190227.RData")

##Reformat
Rev_result3 %>% MakeFrame() %>% 
	separate(scen, into = c("scen", "poll"), sep = "=") %>%
	separate(labor, into = c("lab", "labor"), sep = "=") %>%  
	{. ->> rev1a} %>% group_by(poll, labor) %>%
	dplyr::summarize(avg.net = mean(net), stdev.net = sd(net)) %>% 
	arrange(factor(as.factor(labor), levels = c("low", "med", "high"))) %>%
	 mutate(plo.net = avg.net-stdev.net, phi.net = avg.net+stdev.net) -> net.sum

##Plot
norm.x <- c(.4, .9, 1.4) 
LB.x <- c(.5, 1, 1.5) 
hand.x <- c(.6, 1.1, 1.6)

##Net Rev
par(mfrow = c(1,1), mar = c(4,5,1,2))
plot(avg.net ~ LB.x, data = net.sum[net.sum$poll == "LB",], 
  pch = 21, cex = 1.3, ylim = c(-250,400), xlim = c(0.25, 1.75), 
  ylab = "Net Revenue (US$)", xlab = "", xaxt = "n") 
mtext(text = c("Low", "Med", "High"),  at = c(0.5, 1, 1.5), side = 1, line = 0.5)
mtext(text = "Labor Market",  at = c(1), side = 1, line = 1.75)
arrows(LB.x, net.sum$plo.net[net.sum$poll == "LB"], LB.x, 
    net.sum$phi.net[net.sum$poll == "LB"], length=0.02, angle=90, code=3)

points(avg.net ~ hand.x, data = net.sum[net.sum$poll == "hand",], 
  pch = 24)
arrows(hand.x, net.sum$plo.net[net.sum$poll == "hand"], hand.x, 
    net.sum$phi.net[net.sum$poll == "hand"], length=0.02, angle=90, code=3)

points(avg.net ~ norm.x, data = net.sum[net.sum$poll == "norm",], 
  pch = 22, cex = 1.3)
arrows(norm.x, net.sum$plo.net[net.sum$poll == "norm"], norm.x, 
    net.sum$phi.net[net.sum$poll == "norm"], length=0.02, angle=90, code=3)

lines(x = c(0, 3.5), y = c(0, 0), lty = "dashed")
legend(x = 0.25, y = -100, legend = c("Current ", "L & P", "Hand"), cex = 0.9, pch = c(22,21,24))


##save
dev.print(pdf, "~/MEGA/Analyses16/Results/20190303_NetRev.pdf", 
  width = 7, height = 5)

##################################################################
####################### Tot & Fruits #############################
##################################################################

Rev_result3 %>% MakeFrame() %>% 
	separate(scen, into = c("scen", "poll"), sep = "=") %>%
	separate(labor, into = c("lab", "labor"), sep = "=") %>%  
	{. ->> fruit1a} %>% group_by(poll) %>%
	dplyr::summarize(avg.tot = mean(tot), stdev.tot = sd(tot), 
		avg.fruit = mean(fruit), stdev.fruit = sd(fruit)) %>%
	mutate(plo.tot = avg.tot-stdev.tot, phi.tot = avg.tot+stdev.tot, 
	plo.fruit = avg.fruit-stdev.fruit, phi.fruit = avg.fruit+stdev.fruit) -> fruit.sum


##Plot
x <- c(1.5, 1, 0.5)	
par(mfrow = c(1,1), mar = c(5,5,1,2))

##Fruits
plot(avg.fruit ~ x, data = fruit.sum, 
  pch = 21, cex = 1.3, ylim = c(1000, 3500), xlim = c(0.25, 1.75), 
  ylab = "Fruits Produced", xlab = "Management", xaxt = "n") 
mtext(text = c("Current ", "L & P", "Hand"),  at = c(0.5, 1, 1.5), side = 1, line = 1)
arrows(x, fruit.sum$plo.fruit, x, 
    fruit.sum$phi.fruit, length=0.02, angle=90, code=3)

##save
dev.print(pdf, "~/MEGA/Analyses16/Results/20190303_Fruits.pdf", 
  width = 7, height = 5)

##Total
plot(avg.tot ~ x, data = fruit.sum, 
  pch = 21, cex = 1.3, ylim = c(0, 500), xlim = c(0.25, 1.75), 
  ylab = "Total Revenue (US$)", xlab = "Management", xaxt = "n") 
mtext(text = c("Current ", "L & P", "Hand"),  at = c(0.5, 1, 1.5), side = 1, line = 1)
arrows(x, fruit.sum$plo.tot, x, 
    fruit.sum$phi.tot, length=0.02, angle=90, code=3)

##save
dev.print(pdf, "~/MEGA/Analyses16/Results/20190303_TotRevenue.pdf", 
  width = 7, height = 5)

##################################################################
############################ Costs ###############################
##################################################################

Rev_result3 %>% MakeFrame() %>% 
	separate(scen, into = c("scen", "poll"), sep = "=") %>%
	separate(labor, into = c("lab", "labor"), sep = "=") %>%  
	{. ->> rev1a} %>% group_by(poll, labor) %>%
	dplyr::summarize(avg.cost = mean(cost), stdev.cost = sd(cost)) %>% 
	arrange(factor(as.factor(labor), levels = c("low", "med", "high"))) %>% 
	mutate(plo.cost = avg.cost-stdev.cost, phi.cost = avg.cost+stdev.cost) -> cost.sum


##Plot
norm.x <- c(.4, .9, 1.4) 
LB.x <- c(.5, 1, 1.5) 
hand.x <- c(.6, 1.1, 1.6)

par(mfrow = c(1,1), mar = c(5,5,1,2))

plot(avg.cost ~ LB.x, data = cost.sum[cost.sum$poll == "LB",], 
  pch = 21, cex = 1.3, ylim = c(-10,500), xlim = c(0.25, 1.75), 
  ylab = "Opportunity Cost (US$)", xlab = "Labor Market", xaxt = "n") 
mtext(text = c("Low", "Med", "High"),  at = c(0.5, 1, 1.5), side = 1, line = 1)
arrows(LB.x, cost.sum$plo.cost[cost.sum$poll == "LB"], LB.x, 
    cost.sum$phi.cost[cost.sum$poll == "LB"], length=0.02, angle=90, code=3)

points(avg.cost ~ hand.x, data = cost.sum[cost.sum$poll == "hand",], 
  pch = 24)
arrows(hand.x, cost.sum$plo.cost[cost.sum$poll == "hand"], hand.x, 
    cost.sum$phi.cost[cost.sum$poll == "hand"], length=0.02, angle=90, code=3)

points(avg.cost ~ norm.x, data = cost.sum[cost.sum$poll == "norm",], 
  pch = 22, cex = 1.3)
arrows(norm.x, cost.sum$plo.cost[cost.sum$poll == "norm"], norm.x, 
    cost.sum$phi.cost[cost.sum$poll == "norm"], length=0.02, angle=90, code=3)

legend(x = 0.25, y = 500, legend = c("Current ", "L & P", "Hand"), cex = 0.9, pch = c(22,21,24))

##save
dev.print(pdf, "~/MEGA/Analyses16/Results/20190303_Costs.pdf", 
  width = 7, height = 5)

##################################################################
##################################################################
############################ Tables ##############################
##################################################################
##################################################################

MakeTable(output=Rev_result3, rows="scen", cols=c("labor"), digits=2, include_meta=FALSE)
