
#home computer
setwd("C:/Users/mason/Dropbox/git/Vicia/")

#office computer
setwd("C:/Users/mason.kulbaba/Dropbox/git/Vicia")

# Load required packages
library(refund)
library(dplyr)
library(tidyr)
library(tibble)

# Read data
dat <- read.csv("vicia_final_data.csv")

############## Functional Regression ########################################
#############################################################################
#
# Functional Regression with Seed Set
#

dat$PlantID<- as.factor(dat$PlantID)
dat$Branch<- as.factor(dat$Branch)



#maximum fruit set per plant
seed<- aggregate(dat$seeds, by=list(dat$PlantID), sum)

seed$Group.1<- NULL

#calcualte total flower number
flw.no<- aggregate(dat$PosSeq, by=list(dat$PlantID), max)
flw.no$Group.1<- NULL

#calculate total number of branches

bno<- dat[c("PlantID", "Branch")]

bno$Branch<- as.numeric(bno$Branch)

branch.no<- aggregate(bno$Branch, by=list(bno$PlantID), max)
branch.no$Group.1<- NULL

######
# Prepare functional predictors
B<- dat[c("PlantID","PosSeq", "B")]

FL<- dat[c("PlantID","PosSeq", "FL")]

FD<- dat[c("PlantID","PosSeq", "FD")]
# Reshape into long-format matrix
long<- reshape(B, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

#rename
B<-long

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


# set up variable domain:
# "seqpos" -> position of flower in continuous sequence (across all branches)
seqpos<- aggregate(B$PosSeq, by=list(B$PlantID), max)
seqpos$Group.1<- NULL

seqpos<- as.matrix(seqpos)
seqpos<- as.vector(seqpos)




#load Refund - June 4: after chat with Lawrence: check Fig. 4 vd. 
library(refund)


fit.1<- pfr(seed ~ lf.vd(B, vd=unlist(flw.no) , transform='standardized')
            + unlist(flw.no),family='ziP')

fit.2<- pfr(seed ~ lf.vd(FL, vd=unlist(flw.no) , transform='standardized')
            + unlist(flw.no),family='ziP')

fit.3<- pfr(seed ~ lf.vd(FD, vd=unlist(flw.no) , transform='standardized')
            + unlist(flw.no),family='ziP')

summary(fit.1)


##### combined

fit.all <- pfr(seed ~ 
                 lf.vd(B, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FD, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FL,  vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 unlist(log(flw.no)),
               family = "negbin")

summary(fit.all)

########### check dists
# Full formula (same for all models)
model_formula <- seed ~ 
  lf.vd(FL, vd = unlist(flw.no), transform = "standardized") +
  lf.vd(FD, vd = unlist(flw.no), transform = "standardized") +
  lf.vd(B,  vd = unlist(flw.no), transform = "standardized") +
  unlist(log(flw.no))

summary(model_formula)

#output of results
fit<- coef(fit.all)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$B.arg * fit$B.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$B.arg, fit$value, type="l", main="relative")
