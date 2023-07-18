#load requried packages
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(tidyverse)
library(caret)
library(emmeans)
library(refund)
library(car)




# set working directory
setwd("C:/Users/mason/Dropbox/git/Vicia/")

# load cleaned and final version of data
dat<- read.csv("vicia_final_data.csv")


# check class of each variable
sapply(dat, class)

# change to factor: plantID, Branch, PosSeq, Pos
dat$PlantID<- as.factor(dat$PlantID)
dat$Branch<- as.factor(dat$Branch)
dat$PosSeq<- as.factor(dat$PosSeq)
dat$Pos<- as.factor(dat$Pos)

#
# Calculate Mean, variance, and sd for each PlantID
#

#remove missing FL values
dat2<- dat[!is.na(dat$FL), ]

fl.mean<- aggregate(dat2$FL, by=list(dat2$PlantID), mean)
fl.mean$Group.1<- NULL

fl.var<- aggregate(dat2$FL, by=list(dat2$PlantID), var)
fl.var$Group.1<- NULL

fl.sd<- aggregate(dat2$FL, by=list(dat2$PlantID), sd)
fl.sd$Group.1<- NULL

#remove missing FD values
dat2<- dat[!is.na(dat$FD), ]

fd.mean<- aggregate(dat2$FD, by=list(dat2$PlantID), mean)
fd.mean$Group.1<-NULL

fd.var<- aggregate(dat2$FD, by=list(dat2$PlantID), var)
fd.var$Group.1<- NULL

fd.sd<- aggregate(dat2$FD, by=list(dat2$PlantID), sd)
fd.sd$Group.1<- NULL

#remove missing B values
dat2<- dat[!is.na(dat$B), ]

b.mean<- aggregate(dat2$B, by=list(dat2$PlantID), mean)
b.mean$Group.1<-NULL

b.var<- aggregate(dat2$B, by=list(dat2$PlantID), var)
b.var$Group.1<- NULL

b.sd<- aggregate(dat2$B, by=list(dat2$PlantID), sd)

b.sd$Group.1<- NULL

# total seeds for each PlantID
sumseeds<- aggregate(dat$seeds, by=list(dat$PlantID), sum)

sumseeds$Group.1<- NULL

# total flower number
sumflower<- aggregate(as.numeric(dat$PosSeq), by=list(dat$PlantID), max)

sumflower$Group.1<- NULL

var.mean<- cbind(fl.mean, fl.var, fl.sd, fd.mean, fd.var, fd.sd, b.mean, b.var,
                 b.sd, sumseeds, sumflower)

colnames(var.mean)<- c("fl.mean", "fl.var", "fl.sd", "fd.mean", "fd.var", "fd.sd", "b.mean",
                       "b.var", "b.sd", "sumseeds", "sumflower")
var.mean$PlantID<- as.factor(1:40)

var.mean


# glmm of B, with flw numer as offset vs. covariate 
a<- glmmTMB(sumseeds ~  b.mean, family="nbinom1", data=var.mean)
b<- glmmTMB(sumseeds ~  b.mean + sumflower, family="nbinom1", data=var.mean)
c<- glmmTMB(sumseeds ~ b.mean + log(sumflower), family="nbinom1", data= var.mean)
d<- glmmTMB(sumseeds ~  b.mean, offset = log(sumflower), family="nbinom1", data=var.mean)
e<- glmmTMB(sumseeds ~  b.mean, offset = sumflower, family="nbinom1", data=var.mean)

anova(a, b, c, d, e)

summary(b)

testDispersion(b)
simulateResiduals(fittedModel = b, plot = T)
Anova(b, type=3)


# no real difference between covariate and offset (log or otherwise), so 
# go with covariate. No reason to believe a 1:1 increase between flower number
# and seet set

# now add var/sd
b<- glmmTMB(sumseeds ~  b.mean + sumflower, family="nbinom1", data=var.mean)
b1<- glmmTMB(sumseeds ~  b.mean + sumflower + b.var, family="nbinom1", data=var.mean)
b2<- glmmTMB(sumseeds ~  b.mean + sumflower + b.sd, family="nbinom1", data=var.mean)

b3<- glmmTMB(sumseeds ~  b.var + sumflower, family="nbinom1", data=var.mean)

anova(b, b1, b2)

summary(b)
summary(b1)
summary(b2)
summary(b3)

AIC(b, b1, b2, b3)
#################################################################################

# glmm of FL, with flw numer as offset vs. covariate 
a<- glmmTMB(sumseeds ~  fl.mean, family="nbinom1", data=var.mean)
b<- glmmTMB(sumseeds ~  fl.mean + sumflower, family="nbinom1", data=var.mean)
c<- glmmTMB(sumseeds ~ fl.mean + log(sumflower), family="nbinom1", data= var.mean)
d<- glmmTMB(sumseeds ~  fl.mean, offset = log(sumflower), family="nbinom1", data=var.mean)
e<- glmmTMB(sumseeds ~  fl.mean, offset = sumflower, family="nbinom1", data=var.mean)

anova(a, b, c, d, e)
anova(b, c) # again no difference between cov. and offset

summary(b)

testDispersion(b)
simulateResiduals(fittedModel = b, plot = T)
Anova(b, type=3)

# now add var/sd
b<- glmmTMB(sumseeds ~  fl.mean + sumflower, family="nbinom1", data=var.mean)
b1<- glmmTMB(sumseeds ~  fl.mean + sumflower + fl.var, family="nbinom1", data=var.mean)
b2<- glmmTMB(sumseeds ~  fl.mean + sumflower + fl.sd, family="nbinom1", data=var.mean)

b3<- glmmTMB(sumseeds ~  fl.var + sumflower, family="nbinom1", data=var.mean)

anova(b, b1, b2)

summary(b)
summary(b1)
summary(b2)
summary(b3)

AIC(b, b1, b2, b3)
###################################################################################

# glmm of FD, with flw numer as offset vs. covariate 
a<- glmmTMB(sumseeds ~  fd.mean, family="nbinom1", data=var.mean)
b<- glmmTMB(sumseeds ~  fd.mean + sumflower, family="nbinom1", data=var.mean)
c<- glmmTMB(sumseeds ~ fd.mean + log(sumflower), family="nbinom1", data= var.mean)
d<- glmmTMB(sumseeds ~  fd.mean, offset = log(sumflower), family="nbinom1", data=var.mean)
e<- glmmTMB(sumseeds ~  fd.mean, offset = sumflower, family="nbinom1", data=var.mean)

anova(a, b, c, d, e)
anova(b, c) # again no difference between cov. and offset

summary(b)

testDispersion(b)
simulateResiduals(fittedModel = b, plot = T)
Anova(b, type=3)

# now add var/sd
b<- glmmTMB(sumseeds ~  fd.mean + sumflower, family="nbinom1", data=var.mean)
b1<- glmmTMB(sumseeds ~  fd.mean + sumflower + fd.var, family="nbinom1", data=var.mean)
b2<- glmmTMB(sumseeds ~  fd.mean + sumflower + fd.sd, family="nbinom1", data=var.mean)

b3<- glmmTMB(sumseeds ~  fd.var + sumflower, family="nbinom1", data=var.mean)

anova(b, b1, b2)

summary(b)
summary(b1)
summary(b2)
summary(b3)

AIC(b, b1, b2, b3)

testDispersion(b1)
simulateResiduals(fittedModel = b1, plot = T)
Anova(b1, type=3)

#################################################################################

# Functional Regression (SOFR) Work

#################################################################################

#maximum seed set per plant
sumseeds<- aggregate(dat$seeds, by=list(dat$PlantID), sum)
sumseeds$Group.1<- NULL

#maximum frt set per plant
frt<- aggregate(dat$FlwFate, by=list(dat$PlantID), sum)
frt$Group.1<- NULL

#calcualte total flower number
flw.no<- aggregate(as.numeric(dat$PosSeq), by=list(dat$PlantID), max)
flw.no$Group.1<- NULL

#calculate total number of branches
branch.no<- aggregate(as.numeric(dat$Branch), by=list(dat$PlantID), max)
branch.no$Group.1<- NULL

######
# Prepare functional predictors
b<- subset(dat, as.numeric(PosSeq) < 31) # just first 30 flowers

#max flw no with new b data set

flw.no2<- aggregate (as.numeric(b$PosSeq), by=list(b$PlantID), max)
flw.no2$Group.1<-NULL

flw.no2<- as.matrix(flw.no2)

B<- dat[c("PlantID","PosSeq", "B")]

FL<- dat[c("PlantID","PosSeq", "FL")]

FD<- dat[c("PlantID","PosSeq", "FD")]
# Reshape into long-format matrix
long<- reshape(B, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

#rename
banner<-long

# Reshape into long-format matrix
long<- reshape(FL, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

#rename
FL<-long

# Reshape into long-format matrix
long<- reshape(FD, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

#rename
FD<-long

################################################################################

# distribution work for SOFR
fit.1<- pfr(sumseeds ~ lf.vd(banner)+ unlist(flw.no),family='poisson')
fit.2<- pfr(sumseeds ~ lf.vd(banner)+ unlist(flw.no),family='ziP')
fit.3<- pfr(sumseeds ~ lf.vd(banner)+ unlist(flw.no),family='nb')
fit.4<- pfr(sumseeds ~ lf.vd(banner)+ unlist(flw.no))

AIC(fit.1, fit.2, fit.3, fit.4)# ziP best fit (delta AIC > 20)

summary(fit.1) # therefore, use zero-inflated Poisson

# test covariate vs. offset

fit.2.1<- pfr(sumseeds ~ lf.vd(banner), offset= unlist(flw.no),family='ziP')
fit.2.2<- pfr(sumseeds ~ lf.vd(banner), offset= unlist(log(flw.no)),family='ziP')
fit.2.3<- pfr(sumseeds ~ lf.vd(banner) + unlist(log(flw.no)),family='ziP')
fit.2.4<- pfr(sumseeds ~ lf.vd(banner) + unlist(flw.no),family='ziP')


AIC(fit.2, fit.2.1, fit.2.2, fit.2.3, fit.2.4)
anova(fit.2, fit.2.1, fit.2.2, fit.2.3, fit.2.4)

summary(fit.2.1)
summary(fit.2.2)
summary(fit.2.3)
summary(fit.2.4)

# variable domain work

fit.2.4<- pfr(sumseeds ~ lf.vd(banner,basistype="s", vd=unlist(branch.no)), offset= unlist(flw.no),family='ziP')
fit.2.4.1<- pfr(sumseeds ~ lf.vd(banner, vd=unlist(branch.no), transform='standardized')
                + unlist(flw.no),family='ziP')

AIC(fit.2.4, fit.2.4.1)
anova(fit.2.4, fit.2.4.1)

summary(fit.2.4.1)

# quick plot of model fit.2.2

#output of results
fit<- coef(fit.2.4.1)   #Note: are these transformed?

#make absolute independent var.
fit$x<- fit$FD.arg * fit$FD.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$FD.arg, fit$value, type="l", main="relative")

# add z-score

fit$z <- ave(fit$value, fit$banner.vd, FUN=scale)
fit$z2<- scale(fit$value)

write.table(fit, file="./final_results/banner_full.csv",
           sep=",")

##############################################################################
#
# Fruit Set data

# distribution work for SOFR
fit.1<- pfr(frt ~ lf.vd(banner)+ unlist(flw.no),family='poisson')
fit.2<- pfr(frt ~ lf.vd(banner)+ unlist(flw.no),family='ziP')
fit.3<- pfr(frt ~ lf.vd(banner)+ unlist(flw.no),family='nb')
fit.4<- pfr(frt ~ lf.vd(banner)+ unlist(flw.no))

AIC(fit.1, fit.2, fit.3, fit.4)# ziP best fit (delta AIC > 20)

summary(fit.1) 

# offset vs covariate
fit.2.0<- pfr(frt ~ lf.vd(banner),family='poisson')
fit.2.1<- pfr(frt ~ lf.vd(banner), offset= unlist(flw.no),family='poisson')
fit.2.2<- pfr(frt ~ lf.vd(banner), offset= unlist(log(flw.no)),family='nb')
fit.2.3<- pfr(frt ~ lf.vd(banner) + unlist(log(flw.no)),family='nb')
fit.2.4<- pfr(frt ~ lf.vd(banner) + unlist(flw.no),family='poisson')


AIC(fit.2.0, fit.2.1, fit.2.2, fit.2.3, fit.2.4)
anova(fit.2, fit.2.1, fit.2.2, fit.2.3, fit.2.4)

summary(fit.2.0)
summary(fit.2.1)
summary(fit.2.2)
summary(fit.2.3)
summary(fit.2.4)


fit.2.2.1<- pfr(frt ~ lf.vd(FL,basistype = 's'),
                offset= unlist(log(flw.no)),family='poisson')
fit.2.3.1<- pfr(frt ~ lf.vd(FD, transform = 'standardized') + unlist(log(flw.no)),family='nb')

AIC(fit.2.2.1, fit.2.3.1)

summary(fit.2.2.1)
summary(fit.2.3.1)

#output of results
fit<- coef(fit.2.2.1)
#make absolute independent var.
fit$x<- fit$FL.arg * fit$FL.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$banner.arg, fit$value, type="l", main="relative")

# add z-score

fit$z <- ave(fit$value, fit$banner.vd, FUN=scale)
fit$z2<- scale(fit$value)

write.table(fit, file="./final_results/banner_full_FRT.csv",
            sep=",")









