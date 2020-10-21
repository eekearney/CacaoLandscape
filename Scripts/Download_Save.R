##Converting googlespreadshet data to real data
rm(list=ls())
setwd("~/")

##Since data is in google spreadsheets - need this
## install.packages("googlesheets")

##Libraries
library(googlesheets)

##Input the data files
gs_ls() ##See the sheets in the account

##recognizing different sheets
gs_marking <- gs_title("FlowerMarking")
gs_collev <- gs_title("CollectingEvents")
gs_treatdates <- gs_title("TreatmentDates")
gs_fruitmark <- gs_title("FruitMarking")
gs_trees <- gs_title("TreeData")
gs_insects <-  gs_title("Collection")
gs_fruitsurv <-  gs_title("FruitSurvey")
gs_fruitmeas <- gs_title("FruitSizes")

##getting data from those sheets
marking_sheet <-  gs_read_csv(gs_marking, ws = "Sheet1")
collev_sheet <-  gs_read_csv(gs_collev, ws = "Sheet1")
treatdates_sheet <-  gs_read_csv(gs_treatdates, ws = "Sheet1")
fruitmark_sheet <-  gs_read_csv(gs_fruitmark, ws = "Sheet1")
trees_sheet <-  gs_read_csv(gs_trees, ws = "Sheet1")
insects_sheet <-  gs_read_csv(gs_insects, ws = "Sheet1")
fruitsurv_sheet <-  gs_read_csv(gs_fruitsurv, ws = "Sheet1")
fruitmeas_sheet <-  gs_read_csv(gs_fruitmeas, ws = "Sheet1")

##cleaning data
marking <- as.data.frame(marking_sheet)
head(marking)
marking <- marking[,1:11]

collev <- as.data.frame(collev_sheet)
head(collev)
collev <-  collev[,c(1:10)]

treatdates <- as.data.frame(treatdates_sheet)
head(treatdates)

fruitmark <- as.data.frame(fruitmark_sheet)
head(fruitmark)

trees <- as.data.frame(trees_sheet)
head(trees)

insects <-  as.data.frame(insects_sheet)
head(insects)
insects <- insects[1:13]

fruitsurv <-  as.data.frame(fruitsurv_sheet)
head(fruitsurv)
fruitsurv <-  fruitsurv[,1:35]

fruitmeas <-  as.data.frame(fruitmeas_sheet)
head(fruitmeas)
fruitmeas <-  fruitmeas[,1:13]

##saving
save(marking, file =
     "~/MEGA/Analyses16/Data/Marking.RData")

save(collev, file =
     "~/MEGA/Analyses16/Data/CollEvents.RData")

save(treatdates, file =
     "~/MEGA/Analyses16/Data/TreatDates.RData")

save(fruitmark, file =
     "~/MEGA/Analyses16/Data/FruitMark.RData")

save(trees, file =
     "~/MEGA/Analyses16/Data/Trees.RData") 

save(insects, file =
     "~/MEGA/Analyses16/Data/Insects_Cnts.RData")

save(fruitsurv, file =
     "~/MEGA/Analyses16/Data/Fruit_Survey.RData")

save(fruitmeas, file =
     "~/MEGA/Analyses16/Data/Fruit_Measures.RData")

