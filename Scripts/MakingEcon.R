
### Merge make data to run through MC equation
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

## Load in spreadsheets
load("~/MEGA/Analyses16/Data/Marking.RData") 
load("~/MEGA/Analyses16/Data/Marking_SimpleModel_190121.RData")
load("~/MEGA/Analyses16/Data/ModelEst_190220.RData")
load("~/MEGA/Analyses16/Data/FruitSurvival_COnfInt_190220.RData")
prices <- read.csv("~/MEGA/Analyses16/Data/Econ/CacaoPrices.csv", header = TRUE)[,c(1:4)]

#Functions 
source("~/MEGA/Analyses16/Functions/predict_Var.R")

#################################################################
#################################################################
############################ Prep ###############################
#################################################################
#################################################################

#################################################################
########################### Revenue #############################
#################################################################

########################### Flowering ###########################
marking %>% group_by(CollectingEvent, Chakra, Tree) %>% 
	dplyr::summarise(Count = n()) -> flowercounts

hist(flowercounts$Count, breaks = 20) ##not very normal

flowercounts$Count[flowercounts$Count > 14] <- flowercounts$Count[flowercounts$Count > 14] + 
					runif(length(flowercounts$Count[flowercounts$Count > 14]), min = -3, max = 30)

num <- vector(length = 10000)

for (i in 1:length(num)) { 
	set.seed(i)
	test <- round(mean(sample(c(flowercounts$Count, rep(c(0:2), 5)), size = 2, replace = TRUE)),0)
	if (test > 2){
	num[i] <- round(runif(1, min = test-2, max = test+2),0)	
	} else {
	num[i] <- round(runif(1, min = 0, max = test+2),0)	
	}
}

#hist(num, breaks = 50)

##picking flowers
flowers2 <- data.frame(col1 = rep("nonsense", length(num)), count = num)

flowers2 %>% group_by(count) %>% dplyr::summarize(actnum = n()) -> flowers3 

#plot(x = flowers3$count, y = flowers3$actnum, ylim = c(0,1500), xlim = c(-5,60))

# span <- seq(0, 50, by = 1)
# test <- ((dnorm(span, mean = 10, sd = 2)+dnorm(span, mean = 18, sd = 12))/2)*10000
# lines(x = span, y = test, col = "black", lty =5)

test <- NULL

for (i in 1:3650){
	set.seed(-10000-i)
	if (rbinom(1, size = 1, prob = 0.75) == 1){
	tester <- round(rnorm(1, mean = 9, sd = 3),0)
	} else {
	tester <- round(rnorm(1, mean = 18, sd = 10),0)	
	}
	if (tester < 0) {
	tester <- tester*-1
	}
	test[i] <- tester
}

tester2 <- data.frame(col1 = rep("nonsense", length(test)), count = test)

tester2 %>% group_by(count) %>% dplyr::summarize(actnum = n()) -> tester3 

#points(x = tester3$count, y = tester3$actnum, col = "red")

######################## Pollination ############################
	## polliantion is at tree level 

summary(global)

# estBetaParams <- function(mu, var) {
#   alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
#   beta <- alpha * (1 / mu - 1)
#   return(params = list(alpha = alpha, beta = beta))
# }

#Prediction from model for predetermined values 
predictions.TreatPoll <- expand.grid(Pollination = c("H", "O"), Treatment = as.factor(c("ANN", "LB", "NB", "LN")), 
  PCAField = 0, northness = 0, DaysRecent = 0, PCATreat = 0, 
  AvgTemp = 0, AvgWeather = 0, Time_Start = 0,
  Trunk_Cir = 0, PerShade = 0, PerCover = 0,  Can_Ht = 0, Water = c("N", "Y"), 
  PCAFlower = 0, Fruit = 0)

pred.se <- predict.dist(global, dd = predictions.TreatPoll, 
	y = "Fruit", family = "binomial")[c(1:8),c(1:2,17:23)]

# Sums of squares.
#
delta <- function(fit, actual) sum((fit-actual)^2)
#
# The objective function handles the transformed parameters `theta` and
# uses `f.beta` and `delta` to fit the values and measure their discrepancies.
#
objective <- function(theta, x, lower=0, upper=1, true) {
  ab <- exp(theta) # Parameters are the *logs* of alpha and beta
  p <- pbeta((x-lower)/(upper-lower), ab[1], ab[2])
  x.p <- (function(p) log(p/(1-p)))
  fit <- x.p(p)
  actual <- x.p(true)
  return (delta(fit, actual))
}

beta.param <- function(probs, quants){
	##initialize
	x1 <- (probs["lower.x"]-probs["lower.lim"])/(probs["upper.lim"]-probs["lower.lim"])
	x2 <- (probs["upper.x"]-probs["lower.lim"])/(probs["upper.lim"]-probs["lower.lim"])

	x <- c(x1, x2)

	#solve
	start <- log(c(1e1, 1e1))
	sol <- nlm(objective, start, x=x, true=quants,lower=0, upper=1, 
	typsize=c(1,1), fscale=1e-12, gradtol=1e-12)
	parms <- exp(sol$estimate)
	#check
	print(rbind(Real = x, Est = qbeta(p = quants, shape1= parms[1], shape2 = parms[2])))

	##result
	return(c(alpha=parms[1], beta=parms[2], probs["lower.lim"], probs["upper.lim"]))
}

q <- c(0.025, 0.975)


##distibution parameters
###Normal 
norm.prob <- unlist(c(pred.se[8, c("minus4", "minus2", "plus2", "plus4")]))
names(norm.prob) <- c("lower.lim", "lower.x", "upper.x", "upper.lim")
norm.beta <- beta.param(norm.prob, q)

save(norm.beta,
	file = "~/MEGA/Analyses16/Data/Norm_Poll_Est.RData")

hand.prob <- unlist(c(pred.se[7, c("minus4", "minus2", "plus2", "plus4")]))
names(hand.prob) <- c("lower.lim", "lower.x", "upper.x", "upper.lim")
hand.beta <- beta.param(hand.prob, q)
hand.beta

save(hand.beta,
	file = "~/MEGA/Analyses16/Data/Hand_Poll_Est.RData")


LB.prob <- unlist(c(pred.se[4, c("minus4", "minus2", "plus2", "plus4")]))
names(LB.prob) <- c("lower.lim", "lower.x", "upper.x", "upper.lim")
LB.beta <- beta.param(LB.prob, q)
LB.beta

save(LB.beta,
	file = "~/MEGA/Analyses16/Data/LB_Poll_Est.RData")

####################### Fruit survival ##########################
 	##assuming that survival is for the tree as a whole rather than each distinct fruit 
fruit.abort

fruit.beta <- beta.param(fruit.abort, q)
fruit.beta

##save 
save(fruit.beta,
	file = "~/MEGA/Analyses16/Data/FruitSurv_Est.RData")

################# Conversion from fruit to beans ################
 ##one pod is 35-40 grams of beans 

runif(1, min = 35, max = 40)

########################## Cacao Prices #########################

# need to standardize and then make distribution of prices 
## 1 lb wet -*2.325-> 1 lb dry -*2.20462-> 1kg dry = 5.1243
## 1 lb -*2.20462-> 1kg
## 1 metric tonne --> 1000kg

prices %>% mutate(kg.price = (Price*(Measure == "US$/tonne")*0.001)+(Price*(Measure == "US$/wet lb")*5.1243)+
	(Price*(Measure == "US$/dry lb")*2.20462)+(Price*(Measure == "US$/dry kg")*1)) -> prices2

# need to look at histogram 
hist(prices2$kg.price) ##looks great! 

#making normal curve 
m <- mean(prices2$kg.price)
s <- sd(prices2$kg.price)
span <- seq(0.5, 5, by = 0.25)
test <- dnorm(span, mean = m, sd = s)
plot(x = span, y = test, lab = c(10,10,7))

##final curve 
rnorm(x, mean = m, sd = s)

##save
save(prices2, 
	file = "~/MEGA/Analyses16/Data/Prices_190226.RData")

####Output = Total Revenue 


#################################################################
########################### Costs ###############################
#################################################################

######################## Labor Needed ###########################
	##Dependent on Pollination Scheme 

	## Normal 
		# clear 4 times a year 
		# 2 days per clearing 
	norm.work <- c(2)

	##Leaves Banana
		# clear 3 times a year (2 days/time) 
		# cut vegetation once a month (2 days/time)
		# put banana down every two weeks (1 day/time)
	LB.work <- c(2, 1, 1, 2, 1, 1, 2, 1, 1)	

	## Hand 	
		# 50 trees 
		# clear at beginning (2 days)
		# pollination once a week = 2 days
		# 13 weeks total
		##4 - first, 2 every week after 
	hand.work <- c(4, rep(2, times = 12))

######################## Labor Market ##########################

##low labor market - mean = 0.2857143, var = 0.01728395
rbeta(1, shape1 = 2, shape2 = 5, ncp = 0) 

##med labor market - mean = 0.5, var = 0.019
rbeta(1, shape1 = 6, shape2 = 6, ncp = 0)

##high labor market - mean = 0.7142857, var = 0.01728395
rbeta(1, shape1 = 5, shape2 = 2, ncp = 0) 

shapes <- list(low = c(2, 5), med = c(6,6), high = c(5,2))

##save
save(shapes,  
	file = "~/MEGA/Analyses16/Data/Labor_Market_Est.RData")

######################### Wages ################################ 
## wages = $394/month or $18/day 
		##Output = Total Costs 



