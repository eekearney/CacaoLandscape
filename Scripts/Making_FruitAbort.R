###Fruit Abortion estimation
###
rm(list=ls())
setwd("~/")

###Libraries
library(dplyr) 
library(tidyr)
library(chron)

#survival analysis - https://www.datacamp.com/community/tutorials/survival-analysis-R
library(survival)
library(survminer)

##Data
#fruits
load("~/MEGA/Analyses16/Data/FruitMark.RData")
load("~/MEGA/Analyses16/Data/Fruit_Measures.RData")

##flowers
load("~/MEGA/Analyses16/Data/Marking.RData")

#dates
load("~/MEGA/Analyses16/Data/Edited_CollEvents.RData")

#################################################################
#################################################################
########################## Dataframe ############################
#################################################################
#################################################################

##Need:
	# Match fruiting flowers -> fruits 
	# pull in correct dates 
	# match that to the "fruitmeas" 
	# modify for survival curve analysis 

## 1 ## matching fruiting flowers to fruits
colnames(marking) <- c("Coll_Event", "Chakra", "Tree", "Dist", "Ht", "Diam", 
	"Trt", "Branch", "Date", "Fruit", "Size")

#take only the fruits from the flowers that were marked (only ones we have complete data on)
marking %>% filter(Fruit == "Y") %>%
	mutate(ID_flow = paste(Chakra, Tree, Dist, Ht, sep = "_")) -> fruits
		 
#merge the marking information (i.e. the date of flowering) with the fruit marking data (for the letters)		 
fruitmark %>% mutate(ID_flow = paste(Chakra, Tree, Dist., Height, sep = "_")) %>% 
	mutate(ID_fruit = paste(Collection, Chakra, Tree, Fruit, sep = "_")) %>% {. ->> fruitmark2} %>%
	right_join(fruits, by = "ID_flow") -> fruits2

## 2 ## matching collecting events to dates 
dates <- collev[, c("Coll_Event", "Date")]

#only putting in the dates for the flower marking, not the fruit marking
fruits2 %>% left_join(dates, by = "Coll_Event") -> fruits3

head(fruits3)

## 3 ## match to fruit meas (older measures)

fruitmeas %>% mutate(ID_fruit = paste(Collection, Chakra, Tree, Fruit, sep = "_")) %>% {. ->> fruitmeas2} %>%
		right_join(fruits3, by = "ID_fruit") -> merged

## 4 ## Tidy for analysis 
changeDate <- function(x) {
	as.numeric(strftime(as.Date(x, format = "%m/%d/%Y"), format = "%j"))
	}

##prepping for cutting down of the data frane/short/long transistions
merged %>% mutate(Start = as.numeric(strftime(as.Date(Date.y, format = "%Y-%m-%d"), format = "%j")), 
	Next = paste(changeDate(Date.x), Size.y, sep = "_"), 
	Date1 = paste(changeDate(Date), Size, sep = "_"),
	Date2 = paste(changeDate(Date_1), Size_1, sep = "_"),
	Date3 = paste(changeDate(Date_2), Size_2, sep = "_"), 
	Date4 = paste(changeDate(Date_3), Size_3, sep = "_")) -> ReadyForCut

ReadyForCut %>%  dplyr::select(ID_flow, Trt, Start, Next, Date1, Date2, Date3, Date4) %>% 
	gather("Next", "Date1", "Date2", "Date3", "Date4", key = "Time", value = "Date") %>% 
	separate(Date, into = c("Date", "survival"), sep = "_") %>% 
	filter(!is.na(as.numeric(Date))) %>% filter(!(survival == "NA")) %>% 
	mutate(Days = as.numeric(Date)-Start) %>% 
	mutate(surv.ob = is.na(as.numeric(survival))*1) %>% {. ->> midcut} %>%
	arrange(ID_flow, desc(Days)) %>% 
	filter(!duplicated(ID_flow)) -> cut
head(cut)

midcut %>% filter(Days <= 38) %>% 
	arrange(ID_flow, desc(Days)) %>% 
	filter(!duplicated(ID_flow)) -> TrtCut


#select columns > change structure to long > make seprate date and survival columns > 
		# take out all the extras > get the number of days that a fruit survived

# #test for NAs in all columns
# sapply(cut, function(x) sum(is.na(x)))
	
#################################################################
#################################################################
########################## Analysis #############################
#################################################################
#################################################################

##note: 1 if event (death occured), 0 if censored
## time should be the oldest time point known 
surv_object <- Surv(time = cut$Days, event = cut$surv.ob)
surv_object 

##kaplan-meier curves
fit1 <- survfit(surv_object ~ 1, data = cut)
summary(fit1)

limits <- survfit(surv_object ~ 1, data = cut, conf.int = 0.9999999)
summary(limits) 

##graphing it 
ggsurvplot(fit1, data = cut)

###compiling 
fruit.abort <- c(mean = fit1$surv[20], lower.x = fit1$lower[20], upper.x = fit1$upper[20], 
	lower.lim = limits$lower[20], upper.lim = limits$upper[20])

##save
save(fruit.abort, file = "~/MEGA/Analyses16/Data/FruitSurvival_COnfInt_190220.RData")


####################### BONUS #################################
##looking at Treatment effects 
surv_object <- Surv(time = TrtCut$Days, event = TrtCut$surv.ob)
surv_object 

##kaplan-meier curves
fit1 <- survfit(surv_object ~ Trt, data = TrtCut, conf.int = 0.95)
summary(fit1) 

##graphing it 
ggsurvplot(fit1, data = TrtCut)




