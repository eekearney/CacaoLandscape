### Merge data files to make marking data for analysis 
rm(list=ls()) 
setwd("~/")

## Libraries
library(lme4)
library(lmerTest)
library(glmmTMB)
library(chron)
library(MuMIn)
library(car) 
library(boot)
library(Matrix)
library(RColorBrewer)

## Load in spreadsheets
load("~/Dropbox/Analyses16/Data/MarkingFlowerLvl_Ready.RData")
load("~/Dropbox/Analyses16/Data/MarkingTreeLvl_Ready.RData")
 
# ##Unused
# load("~/Dropbox/Analyses16/Data/Edited_Trees.RData")
# load("~/Dropbox/Analyses16/Data/Edited_CollEvents.RData")
# load("~/Dropbox/Analyses16/Data/FieldData.RData")

##Exploring data
plot(ProOpen ~ Treatment, data = markingTreeLvl)
aggregate(markingTreeLvl$ProOpen, by = list(markingTreeLvl$Treatment), mean)
aggregate(markingTreeLvl$ProOpen, by = list(markingTreeLvl$Treatment), sd)

aggregate(markingTreeLvl$ProHand, by = list(markingTreeLvl$Treatment), mean)

##histograms
par(mfrow = c(2,4))
hist(markingTreeLvl$ProOpen[markingTreeLvl$Treatment =="LB"], main = "LB", breaks = 20, xlim = c(0,1))
hist(markingTreeLvl$ProOpen[markingTreeLvl$Treatment =="NB"], main = "NB", breaks = 20, xlim = c(0,1))
hist(markingTreeLvl$ProOpen[markingTreeLvl$Treatment =="LN"], main = "LN", breaks = 20, xlim = c(0,1))
hist(markingTreeLvl$ProOpen[markingTreeLvl$Treatment =="NN"], main = "NN", breaks = 20, xlim = c(0,1))

hist(markingTreeLvl$ProHand[markingTreeLvl$Treatment =="LB"], main = "LB - Hand", breaks = 20, xlim = c(0,1))
hist(markingTreeLvl$ProHand[markingTreeLvl$Treatment =="NB"], main = "NB - Hand", breaks = 20, xlim = c(0,1))
hist(markingTreeLvl$ProHand[markingTreeLvl$Treatment =="LN"], main = "LN - Hand", breaks = 20, xlim = c(0,1))
hist(markingTreeLvl$ProHand[markingTreeLvl$Treatment =="NN"], main = "NN - Hand", breaks = 20, xlim = c(0,1))

##look like LB might have a slight effect 

##Testing null model
null.mod <- glmer(ProOpen ~ Treatment + 
                    (1|Site) + (1|Chakra), family = "binomial",  weights=LengthOpen,
                  data= markingTreeLvl, control=glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=1e9)))

##random effects
getME(null.mod,"theta")
##Should drop Site at least

### Without Site
null.mod2 <- glmer(ProOpen ~ Treatment + 
                    (1|Chakra), family = "binomial",  weights=LengthOpen,
                  data= markingTreeLvl, control=glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=1e9)))

##random effects
getME(null.mod2,"theta")
###drop field too

###simple model
weight.mod <- glmer(ProOpen ~ Treatment + (1|Chakra), family = "binomial",  weights=LengthOpen,
                  data= markingTreeLvl, control=glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=1e9)))

##Add1
add.D1 <- add1(weight.mod, scope = ~ Treatment + SDaysFirst + 
                    SDOYMarked + SDiffDOY + SDaysRecent + 
                    ProHand +
                    AvgTemp + AvgWeather + Time_Start + 
                    slope + Type + northness + eastness +
                    Trunk_Cir + Can_Ht + Water + PerCover + PerShade + Tree_Type, 
                    test = "Chisq", k = 2)
##Add in Tree_Type 
weight.mod2 <- update(weight.mod, . ~ .+ Tree_Type)
##eigen value error but still converges 

##Add2
add.D2 <- add1(weight.mod2, scope = ~ Treatment + SDaysFirst + 
                    SDOYMarked + SDiffDOY + SDaysRecent + 
                    ProHand +
                    AvgTemp + AvgWeather + Time_Start + 
                    slope + Type + northness + eastness +
                    Trunk_Cir + Can_Ht + Water + PerCover + PerShade + Tree_Type, 
                    test = "Chisq", k = 2)
##eastness
weight.mod3 <- update(weight.mod2, . ~ .+ eastness)

##Add3
add.D3 <- add1(weight.mod3, scope = ~ Treatment + SDaysFirst + 
                    SDOYMarked + SDiffDOY + SDaysRecent + 
                    ProHand +
                    AvgTemp + AvgWeather + Time_Start + 
                    slope + Type + northness + eastness +
                    Trunk_Cir + Can_Ht + Water + PerCover + PerShade + Tree_Type, 
                    test = "Chisq", k = 2)
##SDaysRecent is next best but only at p=0.1 
weight.mod4 <- update(weight.mod3, . ~ .+ SDaysRecent)
##doesn't converge with it in so taking weight.mod3 as best

#####Best Model
summary(weight.mod3)

##Keeping simplest model 
##Functions
source("~/Box Sync//Analysis_Projects/Starter Project Emily&Adrian/Analyses/Functions/plotdata.R")
source("~/Box Sync//Analysis_Projects/Starter Project Emily&Adrian/Analyses/Functions/Multiplot.R")
source("~/Box Sync//Analysis_Projects/Starter Project Emily&Adrian/Analyses/Functions/predictIntervals.R")

fmt <- function(){
    function(x) format(x,nsmall = 3, scientific = FALSE)
  }

##
TreatConversion <- function(x){
    if (x == "LB"){
    y <- 1
  } else if (x == "LN") {
    y <- 2
  } else if (x == "NB"){
    y <- 3
  } else {
    y <- 4
  }
  y
}

### 
predict.int <- function(mod,
                        dd,
                        y,
                        family="gaussian"){
  ## matrix-multiply X by the parameter vector β to get the predictions
  ## (or linear predictor in the case of GLM(M)s);
  mm <- model.matrix(terms(mod), dd)
  dd[,y] <- mm %*% fixef(mod)
  ## extract the variance-covariance matrix of the parameters V
  ## compute XVX′^-1 to get the variance-covariance matrix of the
  ## predictions and extract the diagonal of this matrix to get
  ## variances of predictions
  pvar1 <- diag(mm %*% vcov(mod) %*% t(mm))
  ## tvar1 <- pvar1 + VarCorr(mod)$Plot[1] + VarCorr(mod)$Transect[1] +
  ##   VarCorr(mod)$SurveyYear[1]
  ## take the square-root of the variances to get the standard
  ## deviations (errors) of the predictions;
  ## compute confidence intervals based on a Normal approximation
  new.dd <- data.frame(dd,
                       plo=dd[,y] - 2*sqrt(pvar1),
                       phi=dd[,y] + 2*sqrt(pvar1))
  ## run the confidence interval boundaries through the inverse-link
  ## function.
  if(family=="binomial"){
    new.dd[,y] <- inv.logit(new.dd[,y])
    new.dd$plo <- inv.logit(new.dd$plo)
    new.dd$phi <- inv.logit(new.dd$phi)
  } else if(family=="poisson"){
    new.dd[,y] <- exp(new.dd[,y])
    new.dd$plo <- exp(new.dd$plo)
    new.dd$phi <- exp(new.dd$phi)
   }
  return(new.dd)
}

###Making TreatNum
markingTreeLvl$TreatNum <- sapply(markingTreeLvl$Treatment, FUN = TreatConversion)
markingTreeLvl$TreatNum <- jitter(markingTreeLvl$TreatNum)

Types <- c("LB", "LN", "NB", "NN")
##Colors
library(RColorBrewer)
myColors <- brewer.pal(11, 'Spectral')
ColsTreat <- c(myColors[9], myColors[8], 
              myColors[5], myColors[4])
names(ColsTreat) <- c("LB", "LN", "NB", "NN")

##Plotting GLMM (simple) model output 
dd.predict <- expand.grid(Treatment= c("LB", "LN", "NB", "NN"), 
                        eastness = c(0), Tree_Type = c("G", "R"),
                             ProOpen = 0)

dd.predict$Treatment <- as.factor(dd.predict$Treatment)

poll.pi <- predict.int(mod=weight.mod3,
  dd=dd.predict, y="ProOpen", family="binomial")

poll.pi2 <- poll.pi[poll.pi$Tree_Type == "G",]
poll.pi2$TreatNum <- c(1:4)

p1 <- ggplot(markingTreeLvl, aes(TreatNum, ProHand, colour = Treatment))  ##basic data
p2 <- p1 + geom_point(markingTreeLvl, mapping = aes(size = 4, alpha = 1/10, colour = Treatment)) 


#############################################################################################
#####Plotting
#############################################################################################
quartz()
p1 <- ggplot(poll.pi2, aes(TreatNum, ProOpen, colour = Treatment))  ##basic data
p2 <- p1 + geom_point(markingTreeLvl, mapping = aes(size = 4, alpha = 1/10, colour = Treatment)) 
p3 <- p2 + scale_y_continuous(breaks=seq(from = 0, to = 0.7, by = .1),  
              limits=c(-0.01, 0.75)) + xlim(0.5,4.5) +
              scale_colour_manual(values= ColsTreat, guide = FALSE) ##making points and colors different for treatments

p4 <- p3 + theme(panel.border = element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),  
       panel.background = element_rect(fill = "transparent",colour = NA), 
       plot.background = element_rect(fill = "transparent",colour = NA)) #reformatting plot 

p5 <- p4 + theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black")) + 
      theme(axis.text.x = element_text(colour = "transparent", size = 20), 
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.ticks.x = element_blank()) +
      xlab("") + 
      ylab("Chance of Fruit Production") + 
      guides(size=FALSE, shape = FALSE, alpha = FALSE)
p5

##No model output! 
pdf(file = "~/Box Sync//Analysis_Projects/Cacao/Analyses15/Results/FieldMgmtPollPlot.pdf", width = 9, height = 6)
p5
dev.off()

##With model output! 
p6 <- p5 + geom_point(poll.pi2, mapping=aes(size = 4, colour = Treatment)) + 
        geom_errorbar(aes(ymax = phi, ymin = plo, colour = Treatment), width=0)
p6

pdf(file = "~/Box Sync//Analysis_Projects/Cacao/Analyses15/Results/FieldMgmtPollModelPlot.pdf", width = 9, height = 6)
p6
dev.off()

