##Modifying Collection Events to:
##Add in day times
rm(list=ls())
setwd("~/") 

##Libraries
library(chron)

##load data
load("~/MEGA/Analyses16/Data/CollEvents.RData")
load("~/MEGA/Analyses16/Data/TreatDates.RData")

##Add in new column for values
##splitting sites and fields
collev$Site <- substring(collev$Chakra, 1, 2)
collev$Field <-  substring(collev$Chakra, 3, 3)

##################################################################
##################### Formatting Times ###########################
##################################################################

# ##Need to convert from seconds to hours
# collev$Time_Start <- 

# ##Separate out the hour by the number rather than do the
# ##calculations based on that number

# collev$Hour_Start <- round(collev$Time_Start, 0)

# collev$Hour_End <- as.numeric(sapply(strsplit(as.character(collev$Time_End),":"), "[", 1))

# ##Classification - Early Morning (EM), Midday (MD), Late Afternoon
# ##(LA) - also Morning (M), 
# ##Afternoon (A), and All Day (AD) 

# collev$TOD <- rep(0, dim(collev)[1]) 

# for (i in 1:dim(collev)[1]){

#   start <- collev$Hour_Start[i]
#   end <- collev$Hour_End[i]
  
#   if (collev$Purpose[i] == "Collection"){
#     if (start <= 8 & end <= 9) {
#     collev$TOD[i] <-  "EM"
#   } else if (start >= 10 & end < 14) {
#     collev$TOD[i] <-  "MD"
#   } else if (start >= 16 & end >= 17) {
#     collev$TOD[i] <-  "LA"
#   }
#   } else {
#     if (end <= 12) {
#     collev$TOD[i] <-  "M"
#   } else if (start >= 12 & end <  18) {
#     collev$TOD[i] <- "A" 
#   } else if (start <= 11 & end >= 14) {
#     collev$TOD[i] <- "AD" 
#   }  
# }
# }

##################################################################
######################### Average Temp ###########################
##################################################################

AvgingTemp <- function(T1, T2){
    T1 <- as.numeric(T1)
    T2 <- as.numeric(T2)
    if (!is.na(T1) & !is.na(T2)){
      final <- mean(c(T1, T2))
    } else if (is.na(T1) & !is.na(T2)) {
      final <- T2
    } else if (!is.na(T1) & is.na(T2)) {
      final <- T1
    } else {
      final <- NA
    }
    final
}

##Avg_Temp
collev$AvgTemp <- mapply(FUN = AvgingTemp, collev$Temp_Start, collev$Temp_End)

##################################################################
###################### Average Weather ###########################
##################################################################

##Weather
collev$AvgWeather <- rep(NA, dim(collev)[1])

standweather <- function(test){
 col1 <- ifelse(test == "clear", 1, 0)  
 col2 <- ifelse(test == "few clouds", 2, 0)
 col3 <- ifelse(test == "semi cloudy", 3, 0)
 col4 <- ifelse(test == "cloudy", 4, 0)
 col5 <- ifelse(test == "light rain", 5, 0)
 col6 <- ifelse(test == "na", NA, 0)

 tot <- col1+ col2 + col3 + col4 +col5 +col6
 tot
}

collev$NumWea_Start <- standweather(collev$Weather_Start)
collev$NumWea_End <- standweather(collev$Weather_End)

AvgingWeather <- function(W1, W2){
    W1 <- as.numeric(W1)
    W2 <- as.numeric(W2)
    if (!is.na(W1) & !is.na(W2)){
      final <- mean(c(W1, W2))
    } else if (is.na(W1) & !is.na(W2)) {
      final <- W2
    } else if (!is.na(W1) & is.na(W2)) {
      final <- W1
    } else {
      final <- NA
    }
    final
}

collev$AvgWeather <- mapply(AvgingWeather, collev$NumWea_Start, collev$NumWea_End)

##Days since first treatment & most recent treatment & total # of treatments - put this in using the treatdates data 
head(treatdates)

treatdates$Date <- as.Date(treatdates$Date, format = "%m-%d-%y")

###### Since first treatment ###########
##Cutting treatdates down to just Round 1 
RdOne <- treatdates[treatdates$Round == "1",]

collev$Date <- as.Date(collev$Date, format = "%m/%d/%y")

##make subtraction function 
SubDates <- function(x){
  Chakra <- x[names(x) == "Chakra"]
  CollDate <- x[names(x) == "Date"]
  CollDate <- as.Date(CollDate, format = "%Y-%m-%d")
  #browser()
  treatdate <- RdOne$Date[RdOne$Chakra == Chakra]
  time.since.treat <- CollDate - treatdate
  num.since.treat <- as.numeric(time.since.treat)
}

##use apply to make new column in dataset 
collev$DaysFirst <- apply(collev, 1, FUN = SubDates)

###### Since most recent treatment ###########
##make subtraction function 
SubDates <- function(x){
  Chakra <- x[names(x) == "Chakra"]
  CollDate <- x[names(x) == "Date"]
  CollDate <- as.Date(CollDate, format = "%Y-%m-%d")
  #browser()
  treatdate <- treatdates$Date[treatdates$Chakra == Chakra]
  time.since.treat <- CollDate - treatdate
  num.since.treat <- as.numeric(time.since.treat)
  logvec <- num.since.treat > 0
  possibledates <- num.since.treat[logvec == TRUE]
  if (length(possibledates) == 0 ){
    recent <- 0
  } else {
    recent <- min(possibledates)
  }
  recent
}

##use apply to make new column in dataset 
collev$DaysRecent <- apply(collev, 1, FUN = SubDates)

###### Num of treatments ###########

CountDates <- function(x){
  Chakra <- x[names(x) == "Chakra"]
  CollDate <- x[names(x) == "Date"]
  CollDate <- as.Date(CollDate, format = "%Y-%m-%d")
  #browser()
  treatdate <- treatdates$Date[treatdates$Chakra == Chakra]
  time.since.treat <- CollDate - treatdate
  num.since.treat <- as.numeric(time.since.treat)
  logvec <- num.since.treat > 0
  possibledates <- num.since.treat[logvec == TRUE]
  num.treat <- length(possibledates)
  num.treat
}

##use apply to make new column in dataset 
collev$NumTreat <- apply(collev, 1, FUN = CountDates)

##Saving
save(collev, file = "~/MEGA/Analyses16/Data/Edited_CollEvents.RData")
