#############################################################################
#############################################################################
#
# Among raceme (branch variation)
#
#############################################################################
#############################################################################

#home computer
setwd("C:/Users/mason/Dropbox/git/Vicia/")

#office computer
setwd("C:/Users/mason.kulbaba/Dropbox/git/Vicia")

dat<- read.csv("vicia_final_data.csv")

library(refund)

# Isolate Flower Position # 1 
pos1<- subset(dat, Pos ==1)


flw.no<- aggregate(as.numeric(dat$Pos), by=list(dat$PlantID), max)
flw.no$Group.1<-NULL

seeds<- aggregate(dat$seeds, by=list(dat$PlantID), sum)
seeds$Group.1<-NULL

branch.no<- aggregate(as.numeric(dat$Branch), by=list(dat$PlantID), max)
branch.no$Group.1<-NULL

B<- pos1[c("PlantID","Branch", "B")]
FL<- pos1[c("PlantID","Branch", "FL")]
FD<- pos1[c("PlantID","Branch", "FD")]

# Reshape into long-format matrix
long<- reshape(B, timevar="Branch", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

banner<- as.matrix(long)

long<- reshape(FL, timevar="Branch", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FL<- as.matrix(long)

long<- reshape(FD, timevar="Branch", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FD<- as.matrix(long)

#########
#
# functional regression


# Banner flw1
fit.1<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
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

write.table(fit, "C:/Users/mason.kulbaba/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Among/among_raceme_banner_flw1.csv", sep="," , row.names=F, quote=F)


# FL flw1
fit.1<- pfr(seeds ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seed ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seed ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seed ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seed ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
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

write.table(fit, "C:/Users/mason.kulbaba/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Among/among_raceme_FL_flw1.csv", sep="," , row.names=F, quote=F)



# FD flw1
fit.1<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", ,transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
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

write.table(fit, "C:/Users/mason.kulbaba/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Among/among_raceme_FD_flw1_int.csv", sep="," , row.names=F, quote=F)


########################################################
#
# flower 4 (5)

pos4<- subset(dat, Pos ==4)



B<- pos4[c("PlantID","Bpos", "B")]
FL<- pos4[c("PlantID","Bpos", "FL")]
FD<- pos4[c("PlantID","Bpos", "FD")]

# Reshape into long-format matrix
long<- reshape(B, timevar="Bpos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

banner<- as.matrix(long)

long<- reshape(FL, timevar="Bpos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FL<- as.matrix(long)

long<- reshape(FD, timevar="Bpos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FD<- as.matrix(long)

#########


# Banner flw4
fit.1<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no5)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(banner, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
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

write.table(fit, "C:/Users/mason.kulbaba/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Among/among_raceme_banner_flw5.csv", sep="," , row.names=F, quote=F)


# FL flw1
fit.1<- pfr(seeds ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(FL, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
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
fit$x<- fit$FL.arg * fit$FL.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$FL.arg, fit$value, type="l", main="relative")

fit$Z_Score<- fit$value/fit$se

plot(fit$x, fit$Z_Score, type="l", main="absolut & Z-Score")

write.table(fit, "C:/Users/mason.kulbaba/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Among/among_raceme_FL_flw5.csv", sep="," , row.names=F, quote=F)



# FD flw1
fit.1<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp" ,transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)
             ,family='poisson')

fit.1b<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             +  unlist(branch.no)
             ,family='poisson')

fit.1c<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
             + unlist(flw.no)+  unlist(branch.no)
             ,family='poisson')

fit.1d<- pfr(seeds ~ lf.vd(FD, vd=unlist(branch.no), basistype = "s", bs = "tp", transform='standardized')
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

plot(fit$x, fit$Z_Score, type="l", main="absolut & Z-Score")

write.table(fit, "C:/Users/mason.kulbaba/Dropbox/git/Vicia/Vicia Analysis/Results/FR/Among/among_raceme_FD_flw5.csv", sep="," , row.names=F, quote=F)
