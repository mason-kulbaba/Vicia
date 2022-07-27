
setwd("C:/Users/mkulbaba/Dropbox/git/students/Getsemani Arteaga/")

dat<- read.csv("vicia_data.csv")

#remove empty rows19.5


dat2<- dat[!is.na(dat$B), ]

dat2$PlantID<- as.factor(dat2$PlantID)
dat2$Branch<- as.factor(dat2$Branch)
dat2$Pos<- as.factor(dat2$Pos)



#website link:
# http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/


library(tidyverse)
library(caret)


#first visualize FL, FD, and B

ggplot(dat2, aes(PosSeq, FL) ) +
  geom_point() +
  stat_smooth()

ggplot(dat2, aes(PosSeq, FD) ) +
  geom_point() +
  stat_smooth()

ggplot(dat2, aes(PosSeq, B) ) +
  geom_point() +
  stat_smooth()

# Work with Banner Length: seems to be most nonlinear


# start with regular linear regression
# Build the model
model <- lm(B ~ PosSeq, data = dat2)
# Make predictions
predictions <- model %>% predict(dat2)
# Model performance
data.frame(
  RMSE = RMSE(predictions, dat2$B),
  R2 = R2(predictions, dat2$B)
)


#visualize it
ggplot(dat2, aes(PosSeq, B) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)


# Try polynomial term
lm(B ~ PosSeq + I(PosSeq^2), data = dat2)

# alternative code to make changing order of polynomial faster
  lm(B ~ poly(PosSeq, 2, raw = TRUE), data = dat2)

  
  #Sixth order polynomial 
  lm(B ~ poly(PosSeq, 6, raw = TRUE), data = dat2) %>%
    summary()  

  
  #From above results, polynomial beyon 2'nd order are ns. So just 
  # go with second order polynomial for now
  
  
  # Build the model
  model <- lm(B ~ poly(PosSeq, 2, raw = TRUE), data = dat2)
  # Make predictions
  predictions <- model %>% predict(dat2)
  # Model performance
  data.frame(
    RMSE = RMSE(predictions, dat2$B),
    R2 = R2(predictions, dat2$B)
  )  

  
  # Visualize the polynomial fit
  ggplot(dat2, aes(PosSeq, B) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))
  
 # When you have a non-linear relationship, you can also try a logarithm 
#  transformation of the predictor variables:
 
  #  Not likely appropriate here, but including for completeness 
  
    # Build the model
    model <- lm(B ~ log(PosSeq), data = dat2)
  # Make predictions
  predictions <- model %>% predict(dat2)
  # Model performance
  data.frame(
    RMSE = RMSE(predictions, dat2$B),
    R2 = R2(predictions, dat2$B)
  )
  
  
  # visualize the log-transformed approach
  ggplot(dat2, aes(PosSeq, B) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ log(x))
  
# SPLINE REGRESSION
  
  # Polynomial regression only captures a certain amount of curvature in a 
  # nonlinear relationship. An alternative, and often superior, approach to 
  # modeling nonlinear relationships is to use splines (P. Bruce and Bruce 2017).
  
  #Splines provide a way to smoothly interpolate between fixed points, called 
  # knots. Polynomial regression is computed between knots. In other words, 
  # splines are series of polynomial segments strung together, joining at knots 
  # (P. Bruce and Bruce 2017).
  
  # The R package splines includes the function bs for creating a b-spline term 
  # in a regression model.
  
  # You need to specify two parameters: the degree of the polynomial and the 
  # location of the knots. In our example, we'll place the knots at the lower 
  # quartile, the median quartile, and the upper quartile:
  
  
  
  knots <- quantile(dat2$B, p = c(0.25, 0.5, 0.75))
  
  knots
  
  #We'll create a model using a cubic spline (degree = 3):
    
    library(splines)
  # Build the model
  knots <- quantile(dat2$B, p = c(0.25, 0.5, 0.75))
  
  model.spline <- lm (B ~ bs(PosSeq, knots = knots) + Branch + PlantID, data = dat2)
  summary(model.spline)
  
  
  # Make predictions
  predictions <- model %>% predict(dat2)
  # Model performance
  data.frame(
    RMSE = RMSE(predictions, dat2$B),
    R2 = R2(predictions, dat2$B)
  )
  
  #visualize spline regression
  ggplot(dat2, aes(PosSeq, B) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
  
  # Generalized additive models
  
  #Once you have detected a non-linear relationship in your data, the polynomial 
  # terms may not be flexible enough to capture the relationship, and spline terms
  # require specifying the knots.
  
  # Generalized additive models, or GAM, are a technique to automatically fit a 
  # spline regression. This can be done using the mgcv R package:
    
    library(mgcv)
  # Build the model
  
  #The term s(PosSeq) tells the gam() function to find the "best" knots 
  # for a spline term.
  
  
  model.gam <- gam(B ~ s(PosSeq) + Branch + PlantID, data = dat2)
  
  summary(model.gam)
  # Make predictions
  predictions <- model.gam %>% predict(dat2)
  # Model performance
  data.frame(
    RMSE = RMSE(predictions, dat2$B),
    R2 = R2(predictions, dat2$B)
  )
  
  # plot the gam model
  gam.plot<- ggplot(dat2, aes(PosSeq, B))  +
    geom_point(aes(colour = Branch)) +
    geom_smooth(method = gam, formula = y ~ s(x))

    gam.plot
    
    
    gam.plot + ggtitle("Plot of banner length") +
      xlab("Continuous Flower Sequence") + ylab("Banner Length (mm)")
    
    
    #####################################################
  
  #playing with custom colours
  #  gam.plot + scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7", "8"),
   #       values=c("red", "blue", "#CC6600", "black", "#FF3399", "gold", "green", "white"))  
    
    # redo for FL measure
    
    # plot the gam model
    gam.plot<- ggplot(dat2, aes(PosSeq, FL))  +
      geom_point(aes(colour = Branch)) +
      stat_smooth(method = gam, formula = y ~ s(x))
    
    gam.plot
    
    #playing with custom colours
    #gam.plot + scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7", "8"),
     #                             values=c("red", "blue", "#CC6600", "black", "#FF3399", "gold", "green", "white"))  

    
    #and for FD
    # plot the gam model
    gam.plot<- ggplot(dat2, aes(PosSeq, FD))  +
      geom_point(aes(colour = Branch)) +
      stat_smooth(method = gam, formula = y ~ s(x))
    
    gam.plot
    
    #playing with custom colours
    #gam.plot + scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7", "8"),
     #                             values=c("red", "blue", "#CC6600", "black", "#FF3399", "gold", "green", "white"))  
   
    
    
    ###############################################################
    #
    # fit for all PlantID
    
    model.gam <- gam(B ~ s(PosSeq) + PlantID, data=dat2)
model.gam    

summary(model.gam)


co<- as.data.frame(model.gam$coefficients)
######################################################################
#
# Playing with plots

#droplevels(d2$Branch)

#
# reduced data load
d<- read.csv("C:/Users/mkulbaba/Dropbox/git/Vicia/vicia_reduced.csv")

d$PlantID<- as.factor(d$PlantID)
d$Branch<- as.factor(d$Branch)

d2<- d[!is.na(d$B), ]

#figure with no branch
g <- ggplot(d2, aes(x = PosSeq, y = B, 
                   color = PlantID,
                   )) +  geom_point()

g + geom_smooth(se = F, method = "loess") + ggtitle("Banner Length no Branch")+
  xlab("Continuous Flower Sequence") + ylab("Banner Length (mm)")

#################################3
# Incorporate Branch

g <- ggplot(d2, aes(x = PosSeq, y = B, 
                    color = PlantID)) +  geom_point(aes(shape = Branch))

g + geom_smooth(method = "loess", span=.2, se =F) + ggtitle("Banner Length with Branch") +
  xlab("Continuous Flower Sequence") + ylab("Banner Length (mm)")






#############################################3
gam.plot<- ggplot(d, aes(PosSeq, B))  +
  geom_point(aes(colour = PlantID)) +
  geom_smooth(method = gam, formula = y ~ s(x))

gam.plot


gam.plot + ggtitle("Plot of banner length") +
  xlab("Continuous Flower Sequence") + ylab("Banner Length (mm)")



#############################################################################
#
# Functional Regression with Fruit Set
#

dat$PlantID<- as.factor(dat$PlantID)
dat$Branch<- as.factor(dat$Branch)

#maximum fruit set per plant
frt<- aggregate(dat$FlwFate, by=list(dat$PlantID), sum)

frt$Group.1<- NULL

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


# set up variable domain:
# "seqpos" -> position of flower in continuous sequence (across all branches)
seqpos<- aggregate(B$PosSeq, by=list(B$PlantID), max)
seqpos$Group.1<- NULL

seqpos<- as.matrix(seqpos)
seqpos<- as.vector(seqpos)


#load Refund
library(refund)


fit.1<- pfr(frt ~ lf.vd(banner, vd=seqpos,basistype = "te", transform='standardized')
            ,family='poisson')

summary(fit.1)

fit.2<- pfr(frt ~ lf.vd(banner, vd=seqpos, basistype = "t2", transform='standardized')
            , family='poisson')


summary(fit.2)


fit.3<- pfr(frt ~ lf.vd(banner, vd=seqpos,basistype="s",transform='standardized')
            , family='poisson')

summary(fit.3)

AIC(fit.1, fit.2, fit.3)

# Add additional independent variables
fit.1<- pfr(frt ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
            ,family='poisson')
fit.1a<- pfr(frt ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
             , offset= unlist(flw.no)  ,family='poisson')
fit.1b<- pfr(frt ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
             +unlist(branch.no)
            ,family='poisson')

summary(fit.1a)
summary(fit.1b) # branch number sig. on its own, but not with flower number.

AIC(fit.1a, fit.1)

#output of results
fit<- coef(fit.1)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$banner.arg * fit$banner.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$banner.arg, fit$value, type="l", main="relative")


#write.table(fit, "C:/Users/mkulbaba/Dropbox/git/Vicia/Results and Figures/fruit_banner_branch.csv", sep="," , row.names=F, quote=F)


fit2<- read.csv("C:/Users/mkulbaba/Dropbox/git/Vicia/fit_reduced.csv")

fit2$banner.vd<- as.factor(fit2$banner.vd)

fit2$Flower_Number <- fit2$banner.vd

p<- ggplot(fit2, aes(x= banner.arg, y= value, color=Flower_Number)) +
  geom_line(aes(colour = Flower_Number)) 
  
  #geom_smooth(method =gam, formula = y ~ s(x) )

  p
  
  p + ggtitle("Selection on Banner Length") +
    xlab("Relative flower position") + ylab("Partial regression coefficient")
  
  
####################################################################3
  #
  # Flower Length (FL)
  
  fit.1<- pfr(frt ~ lf.vd(FL, vd=seqpos,basistype = "te", transform='standardized')
              ,family='poisson')
  
  summary(fit.1)
  
  fit.2<- pfr(frt ~ lf.vd(FL, vd=seqpos, basistype = "t2", transform='standardized')
              , family='poisson')
  
  
  summary(fit.2)
  
  
  fit.3<- pfr(frt ~ lf.vd(FL, vd=seqpos,basistype="s",transform='standardized')
              , family='poisson')
  
  summary(fit.3)
  
  AIC(fit.1, fit.2, fit.3)

  # Covariates
  fit.1<- pfr(frt ~ lf.vd(FL, vd=seqpos,basistype = "te", transform='standardized')
              ,family='poisson')
  
  fit.1a<- pfr(frt ~ lf.vd(FL, vd=seqpos,basistype = "te", transform='standardized')
               + unlist(flw.no)
               ,family='poisson')
  
  fit.1b<- pfr(frt ~ lf.vd(FL, vd=seqpos,basistype = "te", transform='standardized')
               + unlist(branch.no)
               ,family='poisson')
  summary(fit.1a)
  summary(fit.1b)
  #######################################################
  #
  # Plotting
  
  #output of results
  fit<- coef(fit.1)   #Note: are these transformed?
  
  #make absolute frstart date
  fit$x<- fit$FL.arg * fit$FL.vd
  
  plot(fit$x, fit$value, type="l", main="absolute")
  
  plot(fit$FL.arg, fit$value, type="l", main="relative")
  
  
  write.table(fit, "C:/Users/mkulbaba/Dropbox/git/Vicia/Results and Figures/fruit_FL.csv", sep="," , row.names=F, quote=F)
  
  
  fit2<- read.csv("C:/Users/mkulbaba/Dropbox/git/Vicia/fit_FL_reduced.csv")
  
  fit2$FL.vd<- as.factor(fit2$FL.vd)
  
  fit2$Flower_Number <- fit2$FL.vd
  
  p<- ggplot(fit2, aes(x= x, y= value, color=Flower_Number)) +
    geom_line(aes(colour = Flower_Number)) 
  
  #geom_smooth(method =gam, formula = y ~ s(x) )
  
  p
  
  p + ggtitle("Selection on Flower Length") +
    xlab("Continuous flower position") + ylab("Partial regression coefficient")

  ###########################################################################3
  #
  # Flower Diameter: FD
  
  fit.1<- pfr(frt ~ lf.vd(FD, vd=seqpos,basistype = "te", transform='standardized')
              ,family='poisson')
  
  summary(fit.1)
  
  fit.2<- pfr(frt ~ lf.vd(FD, vd=seqpos, basistype = "t2", transform='standardized')
              , family='poisson')
  
  
  summary(fit.2)
  
  
  fit.3<- pfr(frt ~ lf.vd(FD, vd=seqpos,basistype="s",transform='standardized')
              , family='poisson')
  
  summary(fit.3)
  
  AIC(fit.1, fit.2, fit.3)
  
  # Covariates
  fit.1<- pfr(frt ~ lf.vd(FD, vd=seqpos,basistype = "te", transform='standardized')
              ,family='poisson')
  
  fit.1a<- pfr(frt ~ lf.vd(FD, vd=seqpos,basistype = "te", transform='standardized')
               + unlist(flw.no)
               ,family='poisson')
  
  fit.1b<- pfr(frt ~ lf.vd(FD, vd=seqpos,basistype = "te", transform='standardized')
               +  unlist(branch.no)
               ,family='poisson')
  summary(fit.1a)
  summary(fit.1b)
  #######################################################
  #
  # Plotting
  
  #output of results
  fit<- coef(fit.1b)   #Note: are these transformed?
  
  #make absolute frstart date
  fit$x<- fit$FD.arg * fit$FD.vd
  
  plot(fit$x, fit$value, type="l", main="absolute")
  
  plot(fit$FD.arg, fit$value, type="l", main="relative")
  
  
  write.table(fit, "C:/Users/mkulbaba/Dropbox/git/Vicia/Results and Figures/fruit_FD.csv", sep="," , row.names=F, quote=F)
  
  
  fit2<- read.csv("C:/Users/mkulbaba/Dropbox/git/Vicia/fit_FD_reduced.csv")
  
  fit2$FD.vd<- as.factor(fit2$FD.vd)
  
  fit2$Flower_Number <- fit2$FD.vd
  
  p<- ggplot(fit2, aes(x= x, y= value, color=Flower_Number)) +
    geom_line(aes(colour = Flower_Number)) 
  
  #geom_smooth(method =gam, formula = y ~ s(x) )
  
  p
  
  p + ggtitle("Selection on Flower Length") +
    xlab("Relative flower position") + ylab("Partial regression coefficient")
  