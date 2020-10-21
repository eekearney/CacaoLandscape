##Plotting MC Distributions 
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

##plotting
library(RColorBrewer)
library(DescTools)

## Load in spreadsheets
load("~/MEGA/Analyses16/Data/Marking.RData") 
load("~/MEGA/Analyses16/Data/Marking_SimpleModel_190121.RData")
load("~/MEGA/Analyses16/Data/ModelEst_190220.RData")
load("~/MEGA/Analyses16/Data/FruitSurvival_COnfInt_190220.RData")
prices <- read.csv("~/MEGA/Analyses16/Data/Econ/CacaoPrices.csv", header = TRUE)[,c(1:4)]

#2014 Flower Data
load("~/Box Sync/Analysis_Projects/Cacao/Analyses14/R Data//Poll_Rates.Rdata")

#2015 Data
load("~/Box Sync/Analysis_Projects/Cacao/Analyses15/Data/Marking_AnalysisStart.RData")

########################### Flowering ###########################
#2016 
marking %>% group_by(CollectingEvent, Chakra, Tree) %>% 
	dplyr::summarise(Count = n()) %>% 
	group_by(Count) %>% 
	dplyr::summarise(FCount = n()) %>% 
	mutate(Tot = sum(FCount), Freq = FCount/Tot) -> flowercounts16

#2015
markingScaled2 %>% group_by(SDOYmarked, TreeF) %>% 
	dplyr::summarise(Count = n()) %>% 
	group_by(Count) %>% 
	dplyr::summarise(FCount = n()) %>% 
	mutate(Tot = sum(FCount), Freq = FCount/Tot) -> flowercounts15


#2014 
fl.pol %>% group_by(Date_Marked, Field, Tree) %>% 
	dplyr::summarise(Count = sum(Num_Marked)) %>% 
	group_by(Count) %>% 
	dplyr::summarise(FCount = n()) %>% 
	mutate(Tot = sum(FCount), Freq = 2*(FCount/Tot)) -> flowercounts14

##Combining 
merged1 <- merge(flowercounts15, flowercounts16, by = "Count", all = TRUE)
merged2 <- merge(flowercounts14, merged1, by = "Count", all = TRUE)
merged2$Freq[is.na(merged2$Freq)] <- 0
merged2$Freq.x[is.na(merged2$Freq.x)] <- 0
merged2$Freq.y[is.na(merged2$Freq.y)] <- 0
merged2$Summed <- merged2$Freq + merged2$Freq.x + merged2$Freq.y
merged2$NewFreq <- merged2$Summed/sum(merged2$Summed)

barplot(height = merged2$NewFreq, names = merged2$Count)

merged2$NewFreq[merged2$Count %in% c(2:4)] <- rep(sum(merged2$NewFreq[merged2$Count %in% c(2:4)])/3, 3)
merged2$NewFreq[merged2$Count %in% c(5:7)] <- rep(sum(merged2$NewFreq[merged2$Count %in% c(5:7)])/3, 3)
merged2$NewFreq[merged2$Count %in% c(8:10)] <- rep(sum(merged2$NewFreq[merged2$Count %in% c(8:10)])/3, 3)
merged2$NewFreq[merged2$Count %in% c(11:13)] <- rep(sum(merged2$NewFreq[merged2$Count %in% c(11:13)])/3, 3)
merged2$NewFreq[merged2$Count %in% c(14:16)] <- rep(sum(merged2$NewFreq[merged2$Count %in% c(14:16)])/3, 3)
merged2$NewFreq[merged2$Count %in% c(17:19)] <- rep(sum(merged2$NewFreq[merged2$Count %in% c(17:19)])/3, 3)

barplot(height = merged2$NewFreq, names = merged2$Count)
merged2$NewTimes <- merged2$NewFreq*1000

expanded <- merged2[rep(merged2$Count, merged2$NewTimes), 1]

##more normal bust also very close to 0 
test5 <- NULL

for (i in 1:10000){
	set.seed(-10000-i)
	tester <- rnorm(1, mean = 3, sd = 7)
	if (tester < 0){
	tester <- rexp(1, rate = 0.15)
	}
	test5[i] <- round(tester, 0)
}

# hist(test5, breaks = c(-1:100), freq = FALSE, main = "Flower Number Distribution")

##Needed distributions 
load("~/MEGA/Analyses16/Data/Norm_Poll_Est.RData")
load("~/MEGA/Analyses16/Data/Hand_Poll_Est.RData")
load("~/MEGA/Analyses16/Data/LB_Poll_Est.RData")
load("~/MEGA/Analyses16/Data/FruitSurv_Est.RData")
load("~/MEGA/Analyses16/Data/Prices_190221.RData")
load("~/MEGA/Analyses16/Data/Labor_Market_Est.RData")

##All
##save
png(filename= "~/MEGA/Analyses16/Results/20190417_EcondDist.png", 
    units="in", width=7, height=9, pointsize=12, res=1080)

par(mfrow = c(3,2))
par(mar=c(5,3.5,1,0))
plot(density(test5, adjust=2), xlim = c(1,70), yaxt='n',
	main = "", ylab = "", xlab = "Flower Count")
mtext("Probability \nDensity", at = 0.04, side = 2, line = 0.5, cex = 0.7)
# mtext("A", at = -5, side = 1, line = -11, cex = 1.1)

########################### Fruit Set ###########################
myColors <- brewer.pal(6, 'Dark2')
ColsTreat <- c(myColors[2], myColors[1], myColors[6], myColors[5])

###Business as Usual 
norm.test <- (rbeta(10000, shape1 = norm.beta["alpha"],shape2 = norm.beta["beta"])*
		(norm.beta["upper.lim"]-norm.beta["lower.lim"]))+norm.beta["lower.lim"]
norm.x <- density(norm.test, adjust=2)$x
norm.y <- density(norm.test, adjust=2)$y

par(mar=c(5,1,1,1))
plot(x = 0, y = 0, ylim = c(0,100), type = "n", yaxt='n', 
	xlim = c(0,0.5), main = "", ylab = "", xlab = "Fruit Set Probability")	
# mtext("B", at = -0.03, side = 1, line = -8.5, cex = 1.1)
# lines(loess(norm.y ~ norm.x, span = 0.5), col = ColsTreat[3])
lines(density(norm.test, adjust=2), col = ColsTreat[3])
segments(x0 = c(0,0.051), y0 = -0.01, x1 = c(0.02,0.5), y1 = -0.01, col = ColsTreat[3], lwd = 0.8)

###Leaves & Pseudostems
LB.test <- (rbeta(10000, shape1 = LB.beta["alpha"],shape2 = LB.beta["beta"])*
		(LB.beta["upper.lim"]-LB.beta["lower.lim"]))+LB.beta["lower.lim"]
# LB.x <- density(LB.test, adjust=2)$x
# LB.y <- density(LB.test, adjust=2)$y/sum(density(LB.test, adjust=2)$y)
lines(density(LB.test, adjust = 2), col = ColsTreat[2])
segments(x0 = c(0,0.11), y0 = 0, x1 = c(0.07,0.5), y1 =0, col = ColsTreat[2], lwd = 0.8)

###Hand
hand.test <- (rbeta(10000, shape1 = hand.beta["alpha"],shape2 = hand.beta["beta"])*
		(hand.beta["upper.lim"]-hand.beta["lower.lim"]))+hand.beta["lower.lim"]
# hand.x <- density(hand.test, adjust=2)$x
# hand.y <- density(hand.test, adjust=2)$y/sum(density(hand.test, adjust=2)$y)*0.8
lines(density(hand.test, adjust = 2), col = ColsTreat[1])
segments(x0 = c(0,0.38), y0 = 0, x1 = c(0.28,0.5), y1 =0, col = ColsTreat[1], lwd = 0.8)


legend(x = 0.33, y = 105, legend = c("Current    ", "Leaves & \nPseudostems", "Hand"), 
  cex = 0.9, lwd = 1, col = ColsTreat[c(3,2,1)], bty = "n")


######################## Fruit Survival #########################

surv.prob <- (rbeta(10000, shape1 = fruit.beta["alpha"], shape2 = fruit.beta["alpha"])*
		(fruit.beta["upper.lim"]-fruit.beta["lower.lim"]))+fruit.beta["lower.lim"] 

par(mar=c(5,3.5,1,0))
plot(density(surv.prob, adjust=2), xlim = c(0,1), yaxt='n',
	main = "", ylab = "", xlab = "Fruit Survival to Maturity")
mtext("Probability \nDensity", at = 2, side = 2, line = 0.5, cex = 0.7)
segments(x0 = c(0,0.945), y0 = 0, x1 = c(0.286,1), y1 =0, lwd = 1)

######################## Fruit Weight #########################
bean.wt <- runif(10000, min = 35, max = 40)

par(mar=c(5,1,1,1))
plot(x = 0, y = 0, xlim = c(35,40), ylim = c(0,1), yaxt='n',
	main = "", ylab = "", xlab = "Dried Bean Weight")
segments(x0 = c(35), y0 = 0.6, x1 = c(40), y1 = 0.6, lwd = 1.2)


########################### Price #############################

m <- mean(prices2$kg.price)
s <- sd(prices2$kg.price)
yr.price <- rnorm(10000, mean = m, sd = s)

par(mar=c(5,3.5,1,0))
plot(density(yr.price, adjust=2), xlim = c(0,6), yaxt='n',
	main = "", ylab = "", xlab = "Price (US$/kg)")
mtext("Probability \nDensity", at = 0.3, side = 2, line = 0.5, cex = 0.7)
segments(x0 = c(-0.19), y0 = 0, x1 = c(0), y1 =0, lwd = 3, col = "white")

######################## Labor Market #########################

low.prob <- rbeta(10000, shape1 = shapes$low[1], shape2 = shapes$low[2]) 
med.prob <- rbeta(10000, shape1 = shapes$med[1], shape2 = shapes$med[2]) 
high.prob <- rbeta(10000, shape1 = shapes$high[1], shape2 = shapes$high[2]) 

par(mar=c(5,1,1,1))
plot(density(med.prob, adjust=2), xlim = c(0,1), ylim = c(0,3), yaxt='n', type = "n",
	main = "", ylab = "", xlab = "Probability of Securing Work") 


lines(density(low.prob, adjust = 2), lty = 1)
lines(density(med.prob, adjust = 2), lty = 2)
lines(density(high.prob, adjust = 2), lty = 3)

legend(x = 0.10, y = 3.1, legend = c("Low", "Medium       ", "High"), 
  cex = 0.9, lty = c(1,2,3), bty = "n", ncol = 3)

dev.off()


