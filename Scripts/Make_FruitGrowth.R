##Taking only fruit measures and making growth curve with them
rm(list=ls())
setwd("~/")

## Libraries
library(lme4)
library(lmerTest)

## Data
load("~/Box Sync/Analysis_Projects/Cacao/Analyses/Data/Fruit_Measures.RData")
load("~/Box Sync/Analysis_Projects/Cacao/Analyses/Data/EditedCollEvents.RData")

## Load Lauren's predict function
source("~/Box Sync/Analysis_Projects/Starter Project Emily&Adrian/Analyses/Functions/predictIntervals.R")

##Merging collev and fruit to get all dates
fruit.dates <- merge(fruitmeas, collev, by.x = "Collection_Event", by.y = "Coll_Event")

##parsing down file and editing dates
fruit.parsed <- fruit.dates[,c(1:4,14,10,5:7,12)]

colnames(fruit.parsed) <- c("Coll_Event", "Chakra", "Tree", "Branch", "Marked",
                             "Checked", "Dist", "Height", "FruitSize1",
                            "FruitSize2")

##making dates actual dates
fruit.parsed$DChecked <- as.Date(fruit.parsed$Checked, format = "%Y-%m-%d")
fruit.parsed$DMarked <- as.Date(fruit.parsed$Marked, format =
                                 '%m/%d/%Y')

##Number of days between checks
fruit.parsed$Days <- as.numeric(fruit.parsed$DChecked -
                                fruit.parsed$DMarked)

##Growth between checks
fruit.parsed$Growth <- (as.numeric(fruit.parsed$FruitSize2)
                                  - as.numeric(fruit.parsed$FruitSize1))

##make fruit sizes numeric
fruit.parsed$NFruitSize1 <- as.numeric(fruit.parsed$FruitSize1)  

##Get rid of NAs
fruit.ready <- fruit.parsed[!is.na(fruit.parsed$Growth),]

##Basic LM model:
basic <- lmer(Growth ~ Days*NFruitSize1 + (1|Chakra), data =
              fruit.ready)

##Predict graph
dd.Sfruit <- expand.grid(Days=c(0,5,10,15,20,30),
                        NFruitSize1 = c(1,2,5,8), Growth = 0)


dd.Lfruit <- expand.grid(Days=c(0,10,20,30,40,50),
                        NFruitSize1 = c(10,15,20,25,30), Growth = 0)

# seq(from= min(SA.Crop$DOY), to= max(SA.Crop$DOY), length=20)
## DOY2=seq(from= min(SA.Crop$DOY2), to= max(SA.Crop$DOY2), length=10)
## GenusSpecies = unique(SA.Crop$GenusSpecies)

abund.piS <- predict.int(mod=basic,
                        dd=dd.Sfruit,
                        y="Growth",
                        family="gaussian")

abund.piL <- predict.int(mod=basic,
                        dd=dd.Lfruit,
                        y="Growth",
                        family="gaussian")

abund.pi <- rbind(abund.piS,abund.piL)

abund.pi$Size <- abund.pi$NFruitSize1 + abund.pi$Growth

abund.pi$Sizeplo <- abund.pi$NFruitSize1 + abund.pi$plo

abund.pi$Sizephi <- abund.pi$NFruitSize1 + abund.pi$phi

abund.pi$DFS <- abund.pi$Days +
  c(rep(0,6),rep(10,6),rep(35,6),rep(60,6),rep(70,6),
    rep(105,6),rep(140,6),rep(175,6), rep(210,6))


## quartz(width = 6, height = 5)
## par(mar=c(4.2,4.4,1.5,1.5))

plot(Size ~ DFS, type = 'n', ylim = c(0, 40),
ylab = 'Size', xlab = 'Days', data = abund.pi)

phi <- abund.pi[, 9]
plo <- abund.pi[, 8]
DOY <- abund.pi$DFS

polygon(c(DOY, rev(DOY)), c(phi, rev(plo)),
col = adjustcolor("blue", alpha =0.2), border = NA)

lines(Size ~ DFS, col = "blue",  data =
      abund.pi)

### Fruits grow around 1.4mm a day which means it takes them 215-230
### days to reach ~35cm or maturity

legend("bottomright", legend = c("Control", "Maturing", "Mature"), col = cols, c, lty = c(1,1), lwd = 2)

