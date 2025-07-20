#############################################################################
#############################################################################
#
# Within raceme (branch variation)
#
#############################################################################
#############################################################################

#home computer
setwd("C:/Users/mason/Dropbox/git/Vicia/")

#office computer
setwd("C:/Users/mason.kulbaba/Dropbox/git/Vicia")

dat<- read.csv("vicia_final_data.csv")

library(refund)

# Isolate first branch
Br1<- subset(dat, Branch ==1)

# isoalte first five flowers on Branch 1
Br1_flw<- subset(Br1, Pos== 1 | Pos ==2 | Pos==3 | Pos==4 | Pos==5)

library(dplyr)

flw.no <-as.data.frame( dat %>%
  group_by(PlantID) %>%
  summarise(total_flowers = n(), .groups = "drop"))
flw.no$PlantID<- NULL

flw.vd<- aggregate(Br1_flw$Pos, by=list(Br1_flw$PlantID), max)
flw.vd$Group.1<- NULL

seeds<- aggregateNULLseeds<- aggregate(dat$seeds, by=list(dat$PlantID), sum)seemax()ds<- aggregate(dat$seeds, by=list(dat$PlantID), sum)
seeds$Group.1<-NULL

branch.no<- aggregate(as.numeric(dat$Branch), by=list(dat$PlantID), max)
branch.no$Group.1<-NULL

B<- Br1_flw[c("PlantID","Pos", "B")]
FL<- Br1_flw[c("PlantID","Pos", "FL")]
FD<- Br1_flw[c("PlantID","Pos", "FD")]

# Reshape into long-format matrix
long<- reshape(B, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

banner<- as.matrix(long)

long<- reshape(FL, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FL<- as.matrix(long)

long<- reshape(FD, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FD<- as.matrix(long)

#########
#
# functional regression


# Banner flw1
fit.1<- pfr(seeds ~ lf.vd(banner,vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp" ,transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp" ,transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)*unlist(branch.no)
             ,family='poisson')
AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d)

summary(fit.1)
summary(fit.1a)
summary(fit.1b)
summary(fit.1c)
summary(fit.1d)

#######################################################
#
# Plotting

#output of results
fit<- coef(fit.1c)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$banner.arg * fit$banner.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$banner.arg, fit$value, type="l", main="relative")

fit$Z_Score<- fit$value/fit$se

plot(fit$x, fit$Z_Score, type="l", main="absolute & Z-Score")

write.table(fit, "C:/Users/mason/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Within/within_raceme_banner_branch1.csv", sep="," , row.names=F, quote=F)



# FL flw1
fit.1<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)*unlist(branch.no)
             ,family='poisson')
AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d)

summary(fit.1)
summary(fit.1a)
summary(fit.1b)
summary(fit.1c)
summary(fit.1d)


#######################################################
#
# Plotting

#output of results
fit<- coef(fit.1c)   

#make absolute frstart date
fit$x<- fit$FL.arg * fit$FL.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$FL.arg, fit$value, type="l", main="relative")

fit$Z_Score<- fit$value/fit$se

plot(fit$x, fit$Z_Score, type="l", main="absolute & Z-Score")

write.table(fit, "C:/Users/mason/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Within/within_raceme_FL_branch1.csv", sep="," , row.names=F, quote=F)



# FD flw1
fit.1<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", ,transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.vd),k=13, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)*unlist(branch.no)
             ,family='poisson')
AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d)

summary(fit.1)
summary(fit.1a)
summary(fit.1b)
summary(fit.1c)
summary(fit.1d)


#######################################################
#
# Plotting

#output of results
fit<- coef(fit.1c)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$FD.arg * fit$FD.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$FD.arg, fit$value, type="l", main="relative")

fit$Z_Score<- fit$value/fit$se

plot(fit$x, fit$Z_Score, type="l", main="absolut & LDH Z-Score")

write.table(fit, "C:/Users/mason/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Within/within_raceme_FD_branch1.csv", sep="," , row.names=F, quote=F)


###############################################################################
#
# Branch 5

# load reduced data
#dat2<- read.csv("branch3.csv")

# Isolate fifth branch
Br5<- subset(dat, Branch ==2)

# isoalte first five flowers on Branch 1
Br5_flw<- subset(Br5, Pos== 1 | Pos ==2 | Pos==3 | Pos==4 | Pos==5)


B<- Br5_flw[c("PlantID","Pos", "B")]
FL<- Br5_flw[c("PlantID","Pos", "FL")]
FD<- Br5_flw[c("PlantID","Pos", "FD")]


flw.no<- aggregate(as.numeric(Br5$Pos), by=list(Br5$PlantID), max)
flw.no$Group.1<-NULL

seeds<- aggregate(Br5$seeds, by=list(Br5$PlantID), sum)
seeds$Group.1<-NULL

branch.no<- aggregate(as.numeric(Br5$Branch), by=list(Br5$PlantID), max)
branch.no$Group.1<-NULL

# Reshape into long-format matrix
long<- reshape(B, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

banner<- as.matrix(long)

long<- reshape(FL, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FL<- as.matrix(long)

long<- reshape(FD, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FD<- as.matrix(long)

#########
#
# functional regression


# Banner flw1
fit.1<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp" ,transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp" ,transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(banner, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)*unlist(branch.no)
             ,family='poisson')
AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d)

summary(fit.1)
summary(fit.1a)
summary(fit.1b)
summary(fit.1c)
summary(fit.1d)

#######################################################
#
# Plotting

#output of results
fit<- coef(fit.1c)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$banner.arg * fit$banner.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$banner.arg, fit$value, type="l", main="relative")

fit$Z_Score<- fit$value/fit$se

plot(fit$x, fit$Z_Score, type="l", main="absolute & Z-Score")

write.table(fit, "C:/Users/mason/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Within/within_raceme_banner_branch5.csv", sep="," , row.names=F, quote=F)



# FL flw1
fit.1<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(FL, vd=unlist(flw.no),k=26, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)*unlist(branch.no)
             ,family='poisson')
AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d)

summary(fit.1)
summary(fit.1a)
summary(fit.1b)
summary(fit.1c)
summary(fit.1d)


#######################################################
#
# Plotting

#output of results
fit<- coef(fit.1c)   

#make absolute frstart date
fit$x<- fit$FL.arg * fit$FL.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$FL.arg, fit$value, type="l", main="relative")

fit$Z_Score<- fit$value/fit$se

plot(fit$x, fit$Z_Score, type="l", main="absolute & Z-Score")

write.table(fit, "C:/Users/mason/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Within/within_raceme_FL_branch5.csv", sep="," , row.names=F, quote=F)



# FD flw1
fit.1<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.no), k=26,basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.no),  k=26,basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.no),  k=26,basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.no), k=26, basistype = "s", bs = "tp", ,transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(FD, vd=unlist(flw.no), k=26, basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)*unlist(branch.no)
             ,family='poisson')
AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d)

summary(fit.1)
summary(fit.1a)
summary(fit.1b)
summary(fit.1c)
summary(fit.1d)


#######################################################
#
# Plotting

#output of results
fit<- coef(fit.1c)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$FD.arg * fit$FD.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$FD.arg, fit$value, type="l", main="relative")

fit$Z_Score<- fit$value/fit$se

plot(fit$x, fit$Z_Score, type="l", main="absolut & LDH Z-Score")

write.table(fit, "C:/Users/mason/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Within/within_raceme_FD_branch5.csv", sep="," , row.names=F, quote=F)
