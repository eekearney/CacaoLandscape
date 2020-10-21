##Make spreadsheet of landscape type with fields
rm(list=ls())
setwd("~/") 

## Libraries

##Load in Data
load("~/MEGA/Analyses16/Data/CollEvents.RData")
load("~/Box Sync/Analysis_Projects/Cacao/Analyses15/Data/Landscape.RData")

##Making an empty data.frame
field <- data.frame(matrix(ncol = 2, nrow = 6))

##Column Names
colnames(field) <- c("Chakra", "Type")

##Chakras
field$Chakra <- unique(collev$Chakra)[1:6]

##fieldscape Types
## 3 = One-side
## 2 = Fragment
## 1 = Agriculture 

field$Type <- c(3, 3, 3, 1, 2, 2)


######## Slope & Aspect #############
# Season1 = Season2 
# RB3 = RB2 
# RB2 = RB1
# CC1 = CC3
# CC2 = CC4
# SJ1 = SJ1
# SJ2 = SJ2

land$S2Chakra <- c(NA, "RB1", "RB2", NA, NA, NA, "CC1", "CC2", NA, "SJ1", "SJ2")

land2 <- land[!is.na(land$S2Chakra),]

land3 <- land2[match(field$Chakra,land2$S2Chakra),]

field.new <- cbind(field, land3[, c("Lat", "Long", "slope", "northness", "eastness")])

field <- field.new

##save
save(field, file = "~/MEGA/Analyses16/Data/FieldData.RData")