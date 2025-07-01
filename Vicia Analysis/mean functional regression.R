

library(dplyr)
library(glmmTMB)
library(car)
library(refund)

################
means.ind <- data %>%
  group_by(PlantID, flw_date) %>%
  summarise(
    mean_FL = mean(FL, na.rm = TRUE),
    sd_FL = sd(FL, na.rm = TRUE),
    var_FL = var(FL, na.rm = TRUE),
    
    
    mean_FD = mean(FD, na.rm = TRUE),
    sd_FD = sd(FD, na.rm = TRUE),
    var_FD = var(FD, na.rm = TRUE),
    
    
    mean_B = mean(B, na.rm = TRUE),
    sd_B = sd(B, na.rm = TRUE),
    var_B = var(B, na.rm = TRUE),
    
    
    mean_flw_vol = mean(flw_vol, na.rm = TRUE),
    sd_flw_vol = sd(flw_vol, na.rm = TRUE),
    var_flw_vol = var(flw_vol, na.rm = TRUE),
    
    
    .groups = "drop"
  )

mn<- as.data.frame(means.ind)


# summarize just by flower date, not by plant
## this was just used for plotting population-level chance in means/sd
means.date <- data %>%
  group_by(flw_date) %>%
  summarise(
    mean_FL = mean(FL, na.rm = TRUE),
    sd_FL = sd(FL, na.rm = TRUE),
    var_FL = var(FL, na.rm = TRUE),
    
    
    mean_FD = mean(FD, na.rm = TRUE),
    sd_FD = sd(FD, na.rm = TRUE),
    var_FD = var(FD, na.rm = TRUE),
    
    
    mean_B = mean(B, na.rm = TRUE),
    sd_B = sd(B, na.rm = TRUE),
    var_B = var(B, na.rm = TRUE),
    
    
    mean_flw_vol = mean(flw_vol, na.rm = TRUE),
    sd_flw_vol = sd(flw_vol, na.rm = TRUE),
    var_flw_vol = var(flw_vol, na.rm = TRUE),
    
    
    .groups = "drop"
  )

md<- as.data.frame(means.date)


# Use above means in functional regressoin

#maximum seed set per plant
seed<- aggregate(data$seeds, by=list(data$PlantID), sum)
seed$Group.1<- NULL

#calcualte total flower number
flw.no<- aggregate(data$PosSeq, by=list(data$PlantID), max)
flw.no$Group.1<- NULL

#calculate total branch number
branch.no<- aggregate(data$Branch, by=list(data$PlantID), max)
branch.no$Group.1<- NULL


# Prepare functional predictors
B<- mn[c("PlantID","flw_date", "mean_B")]

FL<- mn[c("PlantID","flw_date", "mean_FL")]

FD<- mn[c("PlantID","flw_date", "mean_FD")]

# Reshape into long-format matrix
long<- reshape(B, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
banner<-long

# Reshape into long-format matrix
long<- reshape(FL, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
fl<-long

# Reshape into long-format matrix
long<- reshape(FD, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

#rename
fd<-long

# set up variable domain:
# 'flw_date' -> day of flowering season (1-18)
flw_day<- aggregate(mn$flw_date, by=list(mn$PlantID), max)
flw_day$Group.1<- NULL

flw_day<- as.matrix(flw_day)
flw_day<- as.vector(flw_day)


#################################################################
# Functional Regression

###############
###############
# Banner ######
###############
###############

# try sd as a single (i.e., plant-level) covariate
sd.b.cov<- aggregate(data$B, by=list(data$PlantID), sd)
sd.b.cov$Group.1<- NULL


# Prepare functional predictors
B.sd<- mn[c("PlantID","flw_date", "sd_B")]
# Reshape into long-format matrix
long<- reshape(B.sd, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
sd.banner<-long

fit.1<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized")
          ,family='poisson')

fit.2<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.b.cov)
          ,family='poisson')

fit.3<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + 
            lf.vd(sd.banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized")
          ,family='poisson')

AIC(fit.1, fit.2, fit.3)
summary(fit.2)

fit.2<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.b.cov)
            ,family='poisson')

fit.2.1<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.b.cov) +unlist(flw.no)
            ,family='poisson')

fit.2.2<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.b.cov)
              +unlist(branch.no)
            ,family='poisson')

fit.2.3<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.b.cov)
              +unlist(branch.no) + unlist(flw.no)
              ,family='poisson')

fit.2.4<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.b.cov)
              +unlist(branch.no):unlist(flw.no)
              ,family='poisson')

AIC(fit.2, fit.2.1, fit.2.2, fit.2.3, fit.2.4)
summary(fit.2.4)

fit<- coef(fit.2.4)
fit$x<- fit$banner.arg * fit$banner.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$banner.arg, fit$value, type="l", main="relative")


#######################
#######################
##### Flower Length####
#######################
# try sd as a single (i.e., plant-level) covariate
sd.fl.cov<- aggregate(data$FL, by=list(data$PlantID), sd)
sd.fl.cov$Group.1<- NULL

# Prepare functional predictors
fl.sd<- mn[c("PlantID","flw_date", "sd_FL")]
# Reshape into long-format matrix
long<- reshape(fl.sd, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
sd.fl<-long

fit.1<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized")
            ,family='poisson')

fit.2<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fl.cov)
            ,family='poisson')

fit.3<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + 
              lf.vd(sd.fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized")
            ,family='poisson')

AIC(fit.1, fit.2, fit.3)
summary(fit.3)

fit.2<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fl.cov)
            ,family='poisson')

fit.2.1<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fl.cov) +unlist(flw.no)
              ,family='poisson')

fit.2.2<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fl.cov)
              +unlist(branch.no)
              ,family='poisson')

fit.2.3<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fl.cov)
              +unlist(branch.no) + unlist(flw.no)
              ,family='poisson')

fit.2.4<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fl.cov)
              +unlist(branch.no):unlist(flw.no)
              ,family='poisson')

AIC(fit.2, fit.2.1, fit.2.2, fit.2.3, fit.2.4)
summary(fit.2.4)

fit<- coef(fit.2.4)
fit$x<- fit$fl.arg * fit$fl.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fl.arg, fit$value, type="l", main="relative")


#######################
#######################
##### Flower Diameter####
#######################
# try sd as a single (i.e., plant-level) covariate
sd.fd.cov<- aggregate(data$FD, by=list(data$PlantID), sd)
sd.fd.cov$Group.1<- NULL

# Prepare functional predictors
fd.sd<- mn[c("PlantID","flw_date", "sd_FD")]
# Reshape into long-format matrix
long<- reshape(fd.sd, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
sd.fd<-long

fit.1<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized")
            ,family='poisson')

fit.2<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fd.cov)
            ,family='poisson')

fit.3<- pfr(seed ~ lf.vd(fd) + 
              lf.vd(sd.fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized")
            ,family='poisson')

AIC(fit.1, fit.2, fit.3)
summary(fit.3)

fit.2<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fd.cov)
            ,family='poisson')

fit.2.1<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fd.cov) +unlist(flw.no)
              ,family='poisson')

fit.2.2<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fd.cov)
              +unlist(branch.no)
              ,family='poisson')

fit.2.3<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fd.cov)
              +unlist(branch.no) + unlist(flw.no)
              ,family='poisson')

fit.2.4<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = "s", bs = "tp", transform = "standardized") + unlist(sd.fd.cov)
              + unlist(branch.no):unlist(flw.no)
              ,family='poisson')

AIC(fit.2, fit.2.1, fit.2.2, fit.2.3, fit.2.4)
summary(fit.2.4)

fit<- coef(fit.2.4)
fit$x<- fit$fd.arg * fit$fd.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fd.arg, fit$value, type="l", main="relative")
