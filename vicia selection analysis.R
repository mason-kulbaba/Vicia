
setwd("C:/Users/mason/Dropbox/git/Vicia/")

dat<- read.csv("vicia_final_data.csv")

#correlation among traits
xmat<-as.matrix(cbind(dat$FD, dat$FL, dat$B))

colnames(xmat)<- c("FD", "FL", "B")

library(Hmisc)
rcorr(xmat)

#remove empty rows19.5


dat2<- dat[!is.na(dat$B), ]



dat2$PlantID<- as.factor(dat2$PlantID)
dat2$Branch<- as.factor(dat2$Branch)
dat2$Pos<- as.factor(dat2$Pos)

# calculate total number of ovules

dat2$ovules<- dat2$seeds + dat2$aborted + dat2$unfert

a<- glmmTMB(ovules ~  Branch, family="nbinom1", data=dat2)

b<- glmmTMB(ovules ~  Branch + Pos, family="nbinom1", data=dat2)

c<- glmmTMB(ovules ~  Branch + Pos + B, family="nbinom1", data=dat2)

testDispersion(c)
simulateResiduals(fittedModel = c, plot = T)
Anova(c, type=3)

summary(c)

AIC(a, b, c)


m<- emmeans(a, "Branch", type='response')
plot(m)

summary(a)
summary(b)
summary(c)

AIC(a, b, c)



f<- fitted(b)

plot(f)
abline(glm(ovules ~ PosSeq + Branch, data=dat2))

#website link:
# http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/

library(ggplot2)
library(tidyverse)
library(caret)
library(emmeans)

# Generate LS means for banner height across branches

dat$PlantID<- as.factor(dat$PlantID)
dat$Branch<- as.factor(dat$Branch)
dat$Pos<- as.factor(dat$Pos)

fit<- glm.nb(B ~ Pos + Branch, data = dat)

summary(fit)



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


ggplot(dat2, aes(PosSeq, ovules) ) +
  geom_point() +
  stat_smooth()



# Work with Banner Length: seems to be most nonlinear


# start with regular linear regression
# Build the model
model <- lm(seeds ~ PosSeq, data = dat2)
# Make predictions
predictions <- model %>% predict(dat2)
# Model performance
data.frame(
  RMSE = RMSE(predictions, dat2$seeds),
  R2 = R2(predictions, dat2$seeds)
)


#visualize it
ggplot(dat2, aes(PosSeq, seeds) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)


# Try polynomial term
lm(seeds ~ PosSeq + I(PosSeq^2), data = dat2)

# alternative code to make changing order of polynomial faster
  lm(seeds ~ poly(PosSeq, 2, raw = TRUE), data = dat2)

  
  #Sixth order polynomial 
  lm(seeds ~ poly(PosSeq, 6, raw = TRUE), data = dat2) %>%
    summary()  

  
  #From above results, polynomial beyon 2'nd order are ns. So just 
  # go with second order polynomial for now
  
  
  # Build the model
  model <- lm(seeds ~ poly(PosSeq, 2, raw = TRUE), data = dat2)
  # Make predictions
  predictions <- model %>% predict(dat2)
  # Model performance
  data.frame(
    RMSE = RMSE(predictions, dat2$seeds),
    R2 = R2(predictions, dat2$seeds)
  )  

  
  # Visualize the polynomial fit
  ggplot(dat2, aes(PosSeq, seeds) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))
  
 # When you have a non-linear relationship, you can also try a logarithm 
#  transformation of the predictor variables:
 
  #  Not likely appropriate here, but including for completeness 
  
    # Build the model
    model <- lm(log(B) ~ PosSeq, data = dat2)
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
  
  model.spline <- lm (seeds ~ bs(PosSeq, knots = knots) + Branch + PlantID, data = dat2)
  summary(model.spline)
  
  
  # Make predictions
  predictions <- model.spline %>% predict(dat2)
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
  
  
  model.gam <- gam(seeds ~ s(PosSeq) + Branch , data = dat2)
  
  summary(model.gam)
  # Make predictions
  predictions <- model.gam %>% predict(dat2)
  # Model performance
  data.frame(
    RMSE = RMSE(predict(model.gam), dat2$ovules),
    R2 = R2(predictions, dat2$ovules)
  )
  
  # plot the gam model
  gam.plot<- ggplot(dat2, aes(PosSeq, B))  +
    geom_point(aes(colour = Branch)) +
    stat_smooth(method = gam, formula = y ~ s(x))

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


#figure with no branch
g <- ggplot(dat2, aes(x = PosSeq, y = B, 
                   color = PlantID,
                   )) +  geom_point()

g + geom_smooth(se = F, method = "loess") + ggtitle("Banner Length no Branch")+
  xlab("Continuous Flower Sequence") + ylab("Banner Length (mm)")

#################################3
# Incorporate Branch

g <- ggplot(dat2, aes(x = PosSeq, y = B, 
                    color = PlantID)) +  geom_point(aes(shape = Branch))

g + geom_smooth(method = "loess", span=.2, se =F) + ggtitle("Banner Length with Branch") +
  xlab("Continuous Flower Sequence") + ylab("Banner Length (mm)")






#############################################3
gam.plot<- ggplot(dat2, aes(PosSeq, B))  +
  geom_point(aes(colour = PlantID)) +
  geom_smooth(method = gam, formula = y ~ s(x))

gam.plot


gam.plot + ggtitle("Plot of banner length") +
  xlab("Continuous Flower Sequence") + ylab("Banner Length (mm)")


############## Functional Regression ########################################
#############################################################################
#
# Functional Regression with Seed Set
#

dat$PlantID<- as.factor(dat$PlantID)
dat$Branch<- as.factor(dat$Branch)

# variance in banner height

b<- glm(B ~ Branch, data = dat)
summary(b)

var(b)

first<- subset(dat2, Pos==1)

first.mean<- aggregate(first$B, by=list(first$Branch), mean)

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


fit.1<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "te", transform='standardized')
            ,family='poisson')

summary(fit.1)

fit.2<- pfr(seed ~ lf.vd(banner, vd=seqpos, basistype = "t2", transform='standardized')
            , family='poisson')


summary(fit.2)


fit.3<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype="s",transform='standardized')
            , family='poisson')

summary(fit.3)

AIC(fit.1, fit.2, fit.3) # use basistype "s"

# Add additional independent variables
fit.1<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
            ,family='poisson')

fit.1a<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
             , offset= unlist(flw.no)  ,family='poisson')

fit.1b<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
             + unlist(flw.no)  ,family='poisson')

fit.1c<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
             +unlist(branch.no)
            ,family='poisson')


fit.1d<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized')
             +unlist(branch.no) + unlist(log(flw.no)),
             family='poisson')

fit.1e<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized') # interaction term
             +unlist(branch.no)*unlist(flw.no),
             family='poisson')

fit.1f<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized') +unlist(branch.no)
             , offset= unlist(flw.no)  ,family='poisson')


fit.1g<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized') # ":" interaction term (just interaction)
             +unlist(branch.no):unlist(flw.no), family='poisson')

fit.1h<- pfr(seed ~ lf.vd(banner, vd=seqpos,basistype = "s", transform='standardized') +unlist(log(branch.no))
             , offset= unlist(flw.no)  ,family='nb')



summary(fit.1a)
summary(fit.1b) # flw no significant, but no convergence
summary(fit.1c) # branch no, sig on it's own
summary(fit.1d) #  branch sig, flw.no not sig
summary(fit.1e) # interaction and flw.no sig., branch not significant
summary(fit.1f) # no convergence, branch significant
summary(fit.1g)
summary(fit.1h)


AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d, fit.1e, fit.1f, fit.1g, fit.1h) # fit.1g smallest AIC

#output of results
fit<- coef(fit.1h)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$banner.arg * fit$banner.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$banner.arg, fit$value, type="l", main="relative")


#write.table(fit, "C:/Users/mason/Dropbox/git/students/Getsemani Arteaga/Results and Figures/banner_seeds_best_fit.csv", sep="," , row.names=F, quote=F)


#fit2<- read.csv("C:/Users/mason/Dropbox/git/Vicia/fit_reduced.csv")

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
  
  fit.1<- pfr(seed ~ lf.vd(FL, vd=seqpos,basistype = "te", transform='standardized')
              ,family='poisson')
  
  summary(fit.1)
  
  fit.2<- pfr(seed ~ lf.vd(FL, vd=seqpos, basistype = "t2", transform='standardized')
              , family='poisson')
  
  
  summary(fit.2)
  
  
  fit.3<- pfr(seed ~ lf.vd(FL, vd=seqpos,basistype="s",transform='standardized')
              , family='poisson')
  
  summary(fit.3)
  
  AIC(fit.1, fit.2, fit.3)

  # Covariates
  fit.1<- pfr(seed ~ lf.vd(FL, vd=seqpos,basistype = "s", transform='standardized')
              ,family='poisson')
  
  fit.1a<- pfr(seed ~ lf.vd(FL, vd=seqpos,basistype = "s", transform='standardized')
               + unlist(flw.no)
               ,family='poisson')
  
  fit.1b<- pfr(seed ~ lf.vd(FL, vd=seqpos,basistype = "s", transform='standardized')
               + unlist(branch.no)
               ,family='poisson')
  
  fit.1c<- pfr(seed ~ lf.vd(FL, vd=seqpos,basistype = "s", transform='standardized')
               + unlist(branch.no) + unlist(flw.no)
               ,family='poisson')
  
  fit.1d<- pfr(seed ~ lf.vd(FL, vd=seqpos,basistype = "s", transform='standardized')
               + unlist(branch.no)*unlist(flw.no)
               ,family='poisson')
  
  
  summary(fit.1a)
  summary(fit.1b)
  summary(fit.1c)
  summary(fit.1d)
  
  AIC(fit.1, fit.1a, fit.1b, fit.1c, fit.1d)
  
  
  #######################################################
  #
  # Plotting
  
  #output of results
  fit<- coef(fit.1d)   #Note: are these transformed?
  
  #make absolute frstart date
  fit$x<- fit$FL.arg * fit$FL.vd
  
  plot(fit$x, fit$value, type="l", main="absolute")
  
  plot(fit$FL.arg, fit$value, type="l", main="relative")
  
  
  #write.table(fit, "C:/Users/mkulbaba/Dropbox/git/students/Getsemani Arteaga/FL_seeds_flwNo_branch.csv", sep="," , row.names=F, quote=F)
  
  
  #fit2<- read.csv("C:/Users/mkulbaba/Dropbox/git/Vicia/fit_FL_reduced.csv")
  
  fit$FL.vd<- as.factor(fit$FL.vd)
  
  fit$Flower_Number <- fit$FL.vd
  
  p<- ggplot(fit, aes(x= x, y= value, color=Flower_Number)) +
    geom_line(aes(colour = Flower_Number)) 
  
  #geom_smooth(method =gam, formula = y ~ s(x) )
  
  p
  
  p + ggtitle("Selection on Flower Length") +
    xlab("Continuous flower position") + ylab("Partial regression coefficient")

  ###########################################################################3
  #
  # Flower Diameter: FD
  
  fit.1<- pfr(seed ~ lf.vd(FD, vd=seqpos,basistype = "te", transform='standardized')
              ,family='poisson')
  
  summary(fit.1)
  
  fit.2<- pfr(seed ~ lf.vd(FD, vd=seqpos, basistype = "t2", transform='standardized')
              , family='poisson')
  
  
  summary(fit.2)
  
  
  fit.3<- pfr(seed ~ lf.vd(FD, vd=seqpos,basistype="s",transform='standardized')
              , family='poisson')
  
  summary(fit.3)
  
  AIC(fit.1, fit.2, fit.3)
  
  # Covariates
  fit.1<- pfr(seed ~ lf.vd(FD, vd=seqpos,basistype = "s", transform='standardized')
              ,family='poisson')
  
  fit.1a<- pfr(seed ~ lf.vd(FD, vd=seqpos,basistype = "s", transform='standardized')
               + unlist(flw.no)
               ,family='poisson')
  
  fit.1b<- pfr(seed ~ lf.vd(FD, vd=seqpos,basistype = "s", transform='standardized')
               +  unlist(branch.no)
               ,family='poisson')
  
  fit.1c<- pfr(seed ~ lf.vd(FD, vd=seqpos,basistype = "s", transform='standardized')
               + unlist(flw.no)+  unlist(branch.no)
               ,family='poisson')
  
  fit.1d<- pfr(seed ~ lf.vd(FD, vd=seqpos,basistype = "s", transform='standardized')
               + unlist(flw.no)*unlist(branch.no)
               ,family='poisson')
  
  summary(fit.1a)
  summary(fit.1b)
  summary(fit.1c)
  summary(fit.1d)
  
  
  #######################################################
  #
  # Plotting
  
  #output of results
  fit<- coef(fit.1d)   #Note: are these transformed?
  
  #make absolute frstart date
  fit$x<- fit$FD.arg * fit$FD.vd
  
  plot(fit$x, fit$value, type="l", main="absolute")
  
  plot(fit$FD.arg, fit$value, type="l", main="relative")
  
  
  #write.table(fit, "C:/Users/mkulbaba/Dropbox/git/students/Getsemani Arteaga/seed_FD_flwNo_branch.csv", sep="," , row.names=F, quote=F)
  
  
  #fit2<- read.csv("C:/Users/mkulbaba/Dropbox/git/Vicia/fit_FD_reduced.csv")
  
  fit$FD.vd<- as.factor(fit$FD.vd)
  
  fit$Flower_Number <- fit$FD.vd
  
  p<- ggplot(fit, aes(x= x, y= value, color=Flower_Number)) +
    geom_line(aes(colour = Flower_Number)) 
  
  #geom_smooth(method =gam, formula = y ~ s(x) )
  
  p
  
  p + ggtitle("Selection on Flower Length") +
    xlab("Relative flower position") + ylab("Partial regression coefficient")
  
  
  ###################################################
  #
  # EM means for first few branches (Banner Length)
  #
  
  dat$PlantID<- as.factor(dat$PlantID)
  dat$Branch<- as.integer(dat$Branch)
  dat$Pos<- as.factor(dat$Pos)

    
 b<- aggregate(dat$Branch, by=list(dat$PlantID), max)

 summary(b)
hist(b$x)  

#take first seven branches

fin<- read.csv("vicia_reduced.csv")
  
fin$PlantID<- as.factor(fin$PlantID)
fin$Branch<- as.factor(fin$Branch)
fin$Pos<- as.factor(fin$Pos)

library(car)
library(emmeans)
library(DHARMa)
library(ggplot2)
library(glmmTMB)


f<- glmmTMB(B ~  Branch + Pos, family="nbinom1", data=fin)
f2<- glmmTMB(B ~  Branch + Pos, family="nbinom2", data=fin)
f3<- glmmTMB(B ~  Branch + Pos, family="gaussian", data=fin)
f4<- glmmTMB(B ~  Branch + Pos, family="Gamma", data=fin)
f5<- glmmTMB(B ~  Branch + Pos, family="poisson", data=fin)


AIC(f, f2, f3, f4, f5)

testDispersion(f4)
simulateResiduals(fittedModel = f4, plot = T)
Anova(f4, type=3)

summary(f4)

f4.1<- glmmTMB(B ~  Branch, family="Gamma", data=fin)

f4.2<- glmmTMB(B ~  Branch + Pos, family="Gamma", data=fin)

AIC(f4.1, f4.2)# Pos effect is significant 


m<-emmeans(f4, "Branch", "Pos", type="response")

plot(m)

#write.table(m, "emmeans_branch_pos.csv", sep=",")

# flower length

fl<- glmmTMB(FL ~  Branch + Pos, family="nbinom1", data=fin)
fl2<- glmmTMB(FL ~  Branch + Pos, family="nbinom1", data=fin)
fl3<- glmmTMB(FL ~  Branch + Pos, family="gaussian", data=fin)
fl4<- glmmTMB(FL ~  Branch + Pos, family="Gamma", data=fin)
fl5<- glmmTMB(FL ~  Branch + Pos, family="poisson", data=fin)

AIC(fl, fl2, fl3, fl4, fl5)


summary(fl4)


fl4.1<- glmmTMB(FL ~  Branch, family="Gamma", data=fin)

fl4.2<- glmmTMB(FL ~  Branch + Pos, family="Gamma", data=fin)

AIC(fl4.1, fl4.2)# Pos effect is significant 


m<-emmeans(fl4, "Branch", "Pos", type="response")

plot(m)

#write.table(m, "emmeans_branch_pos_FL.csv", sep=",")


# Flower diameter
fd<- glmmTMB(FD ~  Branch + Pos, family="nbinom1", data=fin)
fd2<- glmmTMB(FD ~  Branch + Pos, family="nbinom1", data=fin)
fd3<- glmmTMB(FD ~  Branch + Pos, family="gaussian", data=fin)
fd4<- glmmTMB(FD ~  Branch + Pos, family="Gamma", data=fin)
fd5<- glmmTMB(FD ~  Branch + Pos, family="poisson", data=fin)

AIC(fd, fd2, fd3, fd4, fd5)


summary(fd4)


fd4.1<- glmmTMB(FD ~  Branch, family="Gamma", data=fin)

fd4.2<- glmmTMB(FD ~  Branch + Pos, family="Gamma", data=fin)

AIC(fd4.1, fd4.2)# Pos effect is significant 


m<-emmeans(fd4, "Branch", "Pos", type="response")

plot(m)

#write.table(m, "emmeans_branch_pos_FD.csv", sep=",")


##############################################################################
#
# Comparing patterns of within and among branch varation 
#


setwd("C:/Users/mason/Dropbox/git/students/Getsemani Arteaga2/")


fin<- read.csv("vicia_reduced.csv")

fin$PlantID<- as.factor(fin$PlantID)
fin$Branch<- as.factor(fin$Branch)
fin$Pos<- as.factor(fin$Pos)

library(car)
library(emmeans)
library(DHARMa)
library(ggplot2)
library(glmmTMB)



#remove empty rows
fin2<- fin[!is.na(fin$B), ]

fin<- fin2
#standardize fitness (seeds) and straits to a mean of 1

fin$std_B<- fin$B/(mean(fin$B))
fin$std_FL<- fin$FL/(mean(fin$FL))
fin$std_FD<- fin$FD/(mean(fin$FD))


#sum seeds and standardize to mean of 1

seeds<- aggregate(fin$seeds, by=list(fin$PlantID), sum)

colnames(seeds)<- c("PlantID", "seed")

seeds$std_seed<- seeds$seed/mean(seeds$seed)


# make squared term for three floral traits
fin$std_FD2<- fin$std_FD*fin$std_FD
fin$std_FL2<- fin$std_FL*fin$std_FL
fin$std_B2<- fin$std_B*fin$std_B


#max flowers

max.flw<- aggregate(fin$PosSeq, by=list(fin$PlantID), max)
colnames(max.flw)<- c("PlantID", "max.flw")

#max branches
max.branch<-aggregate(as.integer(fin$Branch), by=list(fin$PlantID), max)
colnames(max.branch)<- c("PlantID", "max.branch")

sub<- merge(max.flw, max.branch)

#merge seeds with fin data

dat1<- merge(fin, seeds)

dat<- merge(dat1, sub)

# among branch variation - compare first, second, third, fourth, and fifth flower across branches
one<- subset(dat, Pos == 1)
two<- subset(dat, Pos == 2)
three<- subset(dat, Pos == 3)
four<- subset(dat, Pos == 4)
five<- subset(dat, Pos == 5)


# isolate branches one, two, three, four, five 
b1<- subset(dat, Branch == 1)
b2<- subset(dat, Branch == 2)
b3<- subset(dat, Branch == 3)
b4<- subset(dat, Branch == 4)
b5<- subset(dat, Branch == 5)
b6<- subset(dat, Branch == 6)
b7<- subset(dat, Branch == 7)


all.1<- glmmTMB(B ~ Pos + Branch + max.branch + max.flw + (1|PlantID), 
                family= 'gaussian',
                dispformula = ~Branch,
                data= dat)


testDispersion(all.1)
simulateResiduals(fittedModel = all.1, plot = T)
Anova(all.1, type=3)

summary(all.1)


all.e<- emmeans(all.1, c("Branch", "Pos"), type='response')
plot(all.e)


s<- glmmTMB(std_seed ~  std_B, family="nbinom1", data=one)
s2<- glmmTMB(std_seed ~  std_B, family="nbinom2", data=one)
s3<- glmmTMB(std_seed ~  std_B, family="gaussian", data=one)
s4<- glmmTMB(std_seed ~  std_B, family="Gamma", data=one)
s5<- glmmTMB(std_seed ~  std_B, family="poisson", data=one)

AIC(s, s2, s3, s5)

testDispersion(s3)
simulateResiduals(fittedModel = s3, plot = T)
Anova(s3, type=3)

summary(s3)


# flower one banner
banner_one<- glmmTMB(std_seed ~  std_B, family="gaussian", data=one)

testDispersion(banner_one)
simulateResiduals(fittedModel = banner_one, plot = T)
Anova(banner_one, type=3)

summary(banner_one)

# flower two banner
banner_two<- glmmTMB(std_seed ~  std_B, family="gaussian", data=two)

testDispersion(banner_two)
simulateResiduals(fittedModel = banner_two, plot = T)
Anova(banner_two, type=3)

summary(banner_two)


# flower three banner
banner_three<- glmmTMB(std_seed ~  std_B, family="gaussian", data=three)

testDispersion(banner_three)
simulateResiduals(fittedModel = banner_three, plot = T)
Anova(banner_three, type=3)

summary(banner_three)

# flower four banner
banner_four<- glmmTMB(std_seed ~  std_B, family="gaussian", data=four)

testDispersion(banner_four)
simulateResiduals(fittedModel = banner_four, plot = T)
Anova(banner_four, type=3)

summary(banner_four)

# flower five banner
banner_five<- glmmTMB(std_seed ~  std_B, family="gaussian", data=five)

testDispersion(banner_five)
simulateResiduals(fittedModel = banner_five, plot = T)
Anova(banner_five, type=3)

summary(banner_five)

#########################
# Flower Length Analysis#
#########################


# flower one flower length
fl_one<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=one)

testDispersion(fl_one)
simulateResiduals(fittedModel = fl_one, plot = T)
Anova(fl_one, type=3)

summary(fl_one)


# flower two flower length
fl_two<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=two)

testDispersion(fl_two)
simulateResiduals(fittedModel = fl_two, plot = T)
Anova(fl_two, type=3)

summary(fl_two)


# flower three flower length
fl_three<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=three)

testDispersion(fl_three)
simulateResiduals(fittedModel = fl_three, plot = T)
Anova(fl_three, type=3)

summary(fl_three)

# flower four flower length
fl_four<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=four)

testDispersion(fl_four)
simulateResiduals(fittedModel = fl_four, plot = T)
Anova(fl_four, type=3)

summary(fl_four)

# flower five flower length
fl_five<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=five)

testDispersion(fl_five)
simulateResiduals(fittedModel = fl_five, plot = T)
Anova(fl_five, type=3)

summary(fl_five)


###########################
# Flower Diameter Analysis#
###########################

# flower one flower length
fd_one<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=one)

testDispersion(fd_one)
simulateResiduals(fittedModel = fd_one, plot = T)
Anova(fd_one, type=3)

summary(fd_one)

# flower two flower length
fd_two<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=two)

testDispersion(fd_two)
simulateResiduals(fittedModel = fd_two, plot = T)
Anova(fd_two, type=3)

summary(fd_two)


# flower three flower length
fd_three<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=three)

testDispersion(fd_three)
simulateResiduals(fittedModel = fd_three, plot = T)
Anova(fd_three, type=3)

summary(fd_three)


# flower four flower length
fd_four<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=four)

testDispersion(fd_four)
simulateResiduals(fittedModel = fd_four, plot = T)
Anova(fd_four, type=3)

summary(fd_four)


# flower five flower length
fd_five<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=five)

testDispersion(fd_five)
simulateResiduals(fittedModel = fd_five, plot = T)
Anova(fd_five, type=3)

summary(fd_five)


######################################
# Combined selection across branches

# flower one

t_one<- glmmTMB(std_seed ~  std_FD + std_FL + std_B, family="gaussian", data=one)

testDispersion(t_one)
simulateResiduals(fittedModel = t_one, plot = T)
Anova(t_one, type=3)

summary(t_one)

# check for variance inflation
all.one<- lm(std_seed ~  std_FD + std_FL + std_B, data=one)

summary(all.one)

vif(all.one) # all vif < 2.4

## plotting for Banner

mbanner.dat<-data.frame(one$std_B)
mbanner.dat$B<- one$B

mbanner.dat$adj.banner <- (1.28599 -0.05156*mean(one$FD) + 0.54318*mean(one$FL) -0.58208*(one$B)+ residuals(t_one))


plot(mbanner.dat$one.std_B, mbanner.dat$adj.banner)



# flower five

t_five<- glmmTMB(std_seed ~  std_FD + std_FL + std_B, family="gaussian", data=five)

testDispersion(t_five)
simulateResiduals(fittedModel = t_five, plot = T)
Anova(t_five, type=3)

summary(t_five)


banner.five<- data.frame(five$std_B)
banner.five$B<- five$B

banner.five$adj.banner.five<- (1.4008 + 1.2157*(mean(five$FD) + 0.3112*(mean(five$FD) -1.7478*five$B + residuals(t_five))))

plot(banner.five$B, banner.five$adj.banner.five)

##########################################################
#
# Analysis of Selection across positions within branches #
#
##########################################################


#################
# Banner Height #
#################


# branch 1 banner
banner_bone<- glmmTMB(std_seed ~  std_B, family="gaussian", data=b1)

testDispersion(banner_bone)
simulateResiduals(fittedModel = banner_bone, plot = T)
Anova(banner_bone, type=3)

summary(banner_bone)

# branch 2 banner
banner_btwo<- glmmTMB(std_seed ~  std_B, family="gaussian", data=b2)

testDispersion(banner_btwo)
simulateResiduals(fittedModel = banner_btwo, plot = T)
Anova(banner_btwo, type=3)

summary(banner_btwo)

# branch 3 banner
banner_bthree<- glmmTMB(std_seed ~  std_B, family="gaussian", data=b3)

testDispersion(banner_bthree)
simulateResiduals(fittedModel = banner_bthree, plot = T)
Anova(banner_bthree, type=3)

summary(banner_bthree)

# branch 4 banner
banner_bfour<- glmmTMB(std_seed ~  std_B, family="gaussian", data=b4)

testDispersion(banner_bfour)
simulateResiduals(fittedModel = banner_bfour, plot = T)
Anova(banner_bfour, type=3)

summary(banner_bfour)

# branch 5 banner
banner_bfive<- glmmTMB(std_seed ~  std_B, family="gaussian", data=b5)

testDispersion(banner_bfive)
simulateResiduals(fittedModel = banner_bfive, plot = T)
Anova(banner_bfive, type=3)

summary(banner_bfive)


##########################
# Flower Length Analysis #
##########################

# branch 1 fl
fl_bone<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=b1)

testDispersion(fl_bone)
simulateResiduals(fittedModel = fl_bone, plot = T)
Anova(fl_bone, type=3)

summary(fl_bone)


# branch 2 fl
fl_btwo<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=b2)

testDispersion(fl_btwo)
simulateResiduals(fittedModel = fl_btwo, plot = T)
Anova(fl_btwo, type=3)

summary(fl_btwo)


# branch 3 fl
fl_bthree<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=b3)

testDispersion(fl_bthree)
simulateResiduals(fittedModel = fl_bthree, plot = T)
Anova(fl_bthree, type=3)

summary(fl_bthree)

# branch 4 fl
fl_bfour<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=b4)

testDispersion(fl_bfour)
simulateResiduals(fittedModel = fl_bfour, plot = T)
Anova(fl_bfour, type=3)

summary(fl_bfour)

# branch 5 fl
fl_bfive<- glmmTMB(std_seed ~  std_FL, family="gaussian", data=b5)

testDispersion(fl_bfive)
simulateResiduals(fittedModel = fl_bfive, plot = T)
Anova(fl_bfive, type=3)

summary(fl_bfive)


############################
# Flower Diameter Analysis #
############################

# branch 1 fd
fd_bone<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=b1)

testDispersion(fd_bone)
simulateResiduals(fittedModel = fd_bone, plot = T)
Anova(fd_bone, type=3)

summary(fd_bone)


# branch 2 fd
fd_btwo<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=b2)

testDispersion(fd_btwo)
simulateResiduals(fittedModel = fd_btwo, plot = T)
Anova(fd_btwo, type=3)

summary(fd_btwo)


# branch 3 fd
fd_bthree<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=b3)

testDispersion(fd_bthree)
simulateResiduals(fittedModel = fd_bthree, plot = T)
Anova(fd_bthree, type=3)

summary(fd_bthree)


# branch 4 fd
fd_bfour<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=b4)

testDispersion(fd_bfour)
simulateResiduals(fittedModel = fd_bfour, plot = T)
Anova(fd_bfour, type=3)

summary(fd_bfour)

# branch 5 fd
fd_bfive<- glmmTMB(std_seed ~  std_FD, family="gaussian", data=b5)

testDispersion(fd_bfive)
simulateResiduals(fittedModel = fd_bfive, plot = T)
Anova(fd_bfive, type=3)

summary(fd_bfive)




######################################
# Combined selection within branches

# flower one

t_bone<- glmmTMB(std_seed ~  std_FD + std_FD2 + std_FL + std_FL2 + 
                   std_B + std_B2, family="gaussian", data=b1)

testDispersion(t_bone)
simulateResiduals(fittedModel = t_bone, plot = T)
Anova(t_bone, type=3)

summary(t_bone)

# check for variance inflation
all.one<- lm(std_seed ~  std_FD + std_FD2 + std_FL + std_FL2 + 
               std_B + std_B2, data=b1)

summary(all.one)

vif(all.one) # ...hmmmm something with ^2 terms?

## plotting for Banner

fl1.dat<-data.frame(b1$std_FL)
fl1.dat$FL<- b1$FL


fl1.dat$adj.fl <- (11.0759 -1.6565*mean(b1$std_FD) + 0.6016*mean(b1$std_FD2) -13.7769*(b1$std_FL) + 6.0398*(b1$std_FL2)
                       -1.7462*mean(b1$std_B) + 0.4937*mean(b1$std_B2) + residuals(t_bone))
fl1.dat$pred.fl <- (11.0759 -1.6565*mean(b1$std_FD) + 0.6016*mean(b1$std_FD2) -13.7769*(b1$std_FL) + 6.0398*(b1$std_FL2)
                         -1.7462*mean(b1$std_B) + 0.4937*mean(b1$std_B2))


plot(fl1.dat$b1.std_FL, fl1.dat$adj.fl)

#write.table(fl1.dat, "mbanner1.csv", sep=",")


# branch 5
# flower one

t_bfive<- glmmTMB(std_seed ~  std_FD + std_FL  + 
                    std_B , family="gaussian", data=b5)

testDispersion(t_bfive)
simulateResiduals(fittedModel = t_bfive, plot = T)
Anova(t_bfive, type=3)

summary(t_bfive)


fl5.dat<-data.frame(b5$std_FL)
fl5.dat$FL<- b5$FL

fl5.dat$adj.fl <- (0.5940 + 2.3651*mean(b5$std_FD) -3.1158*(b5$std_FL) + 2.7319*mean(b5$std_B)+ residuals(t_bfive))
fl5.dat$pred.fl <- (0.5940 + 2.3651*mean(b5$std_FD) -3.1158*(b5$std_FL) + 2.7319*mean(b5$std_B))

plot(fl5.dat$FL, fl5.dat$adj.fl)

#write.table(fl5.dat, "mbanner5.csv", sep=",")

#FD plot data
fd5.dat<-data.frame(b5$std_FD)
fd5.dat$FD<- b5$FD

fd5.dat$adj.fd <- (0.5940 + 2.3651*(b5$std_FD) -3.1158*mean(b5$std_FL) + 2.7319*mean(b5$std_B)+ residuals(t_bfive))
fd5.dat$pred.fd <- (0.5940 + 2.3651*(b5$std_FD) -3.1158*mean(b5$std_FL) + 2.7319*mean(b5$std_B))

plot(fd5.dat$FD, fd5.dat$adj.fd)

#write.table(fd5.dat, "FD_b5.csv", sep=",", row.names = F)


# Banner plotting data

#Banner plot data
fd5_B.dat<-data.frame(b5$std_B)
fd5_B.dat$B<- b5$B

fd5_B.dat$adj.b <- (0.5940 + 2.3651*mean(b5$std_FD) -3.1158*mean(b5$std_FL) + 2.7319*(b5$std_B)+ residuals(t_bfive))
fd5_B.dat$pred.b <- (0.5940 + 2.3651*mean(b5$std_FD) -3.1158*mean(b5$std_FL) + 2.7319*(b5$std_B))

plot(fd5_B.dat$B, fd5_B.dat$adj.b)

#write.table(fd5_B.dat, "B_b5.csv", sep=",", row.names = F)


####################################################
## CHARACTERIZE WITHIN AND AMONG BRANCH VARIATION ##
####################################################

#########################
##  Among Branches


## Banner Height b1
among.b.1<- glmmTMB(B ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=one)

testDispersion(among.b.1)
simulateResiduals(fittedModel = among.b.1, plot = T)
Anova(among.b.1, type=3)

summary(among.b.1)

among.b.1.emm<- emmeans(among.b.1, "Branch", type='response')
plot(among.b.1.emm)
emmeans(among.b.1, "Branch", type='response')


## Banner Height b2
among.b.2<- glmmTMB(B ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=two)

testDispersion(among.b.2)
simulateResiduals(fittedModel = among.b.2, plot = T)
Anova(among.b.2, type=3)

summary(among.b.2)

among.b.2.emm<- emmeans(among.b.2, "Branch", type='response')
plot(among.b.2.emm)
emmeans(among.b.2, "Branch", type='response')

## Banner Height b3
among.b.3<- glmmTMB(B ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=three)

testDispersion(among.b.3)
simulateResiduals(fittedModel = among.b.3, plot = T)
Anova(among.b.3, type=3)

summary(among.b.3)

among.b.3.emm<- emmeans(among.b.3, "Branch", type='response')
plot(among.b.3.emm)
emmeans(among.b.3, "Branch", type='response')


## Banner Height b4
among.b.4<- glmmTMB(B ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=four)

testDispersion(among.b.4)
simulateResiduals(fittedModel = among.b.4, plot = T)
Anova(among.b.4, type=3)

summary(among.b.4)

among.b.4.emm<- emmeans(among.b.4, "Branch", type='response')
plot(among.b.4.emm)
emmeans(among.b.4, "Branch", type='response')


## Banner Height b5
among.b.5<- glmmTMB(B ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=five)

testDispersion(among.b.5)
simulateResiduals(fittedModel = among.b.5, plot = T)
Anova(among.b.5, type=3)

summary(among.b.5)

among.b.5.emm<- emmeans(among.b.5, "Branch", type='response')
plot(among.b.5.emm)
emmeans(among.b.5, "Branch", type='response')

################################################################################
#
# FLOWER LENGTH
#

## FL b1
among.b.1<- glmmTMB(FL ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=one)

testDispersion(among.b.1)
simulateResiduals(fittedModel = among.b.1, plot = T)
Anova(among.b.1, type=3)

summary(among.b.1)

among.b.1.emm<- emmeans(among.b.1, "Branch", type='response')
plot(among.b.1.emm)
emmeans(among.b.1, "Branch", type='response')


## FL b2
among.b.2<- glmmTMB(FL ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=two)

testDispersion(among.b.2)
simulateResiduals(fittedModel = among.b.2, plot = T)
Anova(among.b.2, type=3)

summary(among.b.2)

among.b.2.emm<- emmeans(among.b.2, "Branch", type='response')
plot(among.b.2.emm)
emmeans(among.b.2, "Branch", type='response')

## FL b3
among.b.3<- glmmTMB(FL ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=three)

testDispersion(among.b.3)
simulateResiduals(fittedModel = among.b.3, plot = T)
Anova(among.b.3, type=3)

summary(among.b.3)

among.b.3.emm<- emmeans(among.b.3, "Branch", type='response')
plot(among.b.3.emm)
emmeans(among.b.3, "Branch", type='response')


## FL b4
among.b.4<- glmmTMB(FL ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=four)

testDispersion(among.b.4)
simulateResiduals(fittedModel = among.b.4, plot = T)
Anova(among.b.4, type=3)

summary(among.b.4)

among.b.4.emm<- emmeans(among.b.4, "Branch", type='response')
plot(among.b.4.emm)
emmeans(among.b.4, "Branch", type='response')


## FL b5
among.b.5<- glmmTMB(FL ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=five)

testDispersion(among.b.5)
simulateResiduals(fittedModel = among.b.5, plot = T)
Anova(among.b.5, type=3)

summary(among.b.5)

among.b.5.emm<- emmeans(among.b.5, "Branch", type='response')
plot(among.b.5.emm)
emmeans(among.b.5, "Branch", type='response')


################################################################################
#
# FLOWER Diameter
#

## FD b1
among.b.1<- glmmTMB(FD ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=one)

testDispersion(among.b.1)
simulateResiduals(fittedModel = among.b.1, plot = T)
Anova(among.b.1, type=3)

summary(among.b.1)

among.b.1.emm<- emmeans(among.b.1, "Branch", type='response')
plot(among.b.1.emm)
emmeans(among.b.1, "Branch", type='response')


## FD b2
among.b.2<- glmmTMB(FD ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=two)

testDispersion(among.b.2)
simulateResiduals(fittedModel = among.b.2, plot = T)
Anova(among.b.2, type=3)

summary(among.b.2)

among.b.2.emm<- emmeans(among.b.2, "Branch", type='response')
plot(among.b.2.emm)
emmeans(among.b.2, "Branch", type='response')

## FD b3
among.b.3<- glmmTMB(FD ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=three)

testDispersion(among.b.3)
simulateResiduals(fittedModel = among.b.3, plot = T)
Anova(among.b.3, type=3)

summary(among.b.3)

among.b.3.emm<- emmeans(among.b.3, "Branch", type='response')
plot(among.b.3.emm)
emmeans(among.b.3, "Branch", type='response')


## FD b4
among.b.4<- glmmTMB(FD ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=four)

testDispersion(among.b.4)
simulateResiduals(fittedModel = among.b.4, plot = T)
Anova(among.b.4, type=3)

summary(among.b.4)

among.b.4.emm<- emmeans(among.b.4, "Branch", type='response')
plot(among.b.4.emm)
emmeans(among.b.4, "Branch", type='response')


## FD b5
among.b.5<- glmmTMB(FD ~ Branch + max.flw + max.branch + (1|PlantID), 
                    dispformula = ~Branch,
                    family='gaussian', data=five)

testDispersion(among.b.5)
simulateResiduals(fittedModel = among.b.5, plot = T)
Anova(among.b.5, type=3)

summary(among.b.5)

among.b.5.emm<- emmeans(among.b.5, "Branch", type='response')
plot(among.b.5.emm)
emmeans(among.b.5, "Branch", type='response')

###############################################################################
#
#   WITHING BRANCH FLROAL VARIATION
#
#

## Banner Height b1
among.b.1<- glmmTMB(B ~ Pos + max.flw + max.branch + (1|PlantID), 
                   
                    family='gaussian', data=b1)

testDispersion(among.b.1)
simulateResiduals(fittedModel = among.b.1, plot = T)
Anova(among.b.1, type=3)

summary(among.b.1)

among.b.1.emm<- emmeans(among.b.1, "Pos", type='response')
plot(among.b.1.emm)
emmeans(among.b.1, "Pos", type='response')


## Banner Height b2
among.b.2<- glmmTMB(B ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b2)

testDispersion(among.b.2)
simulateResiduals(fittedModel = among.b.2, plot = T)
Anova(among.b.2, type=3)

summary(among.b.2)

among.b.2.emm<- emmeans(among.b.2, "Pos", type='response')
plot(among.b.2.emm)
emmeans(among.b.2, "Pos", type='response')

## Banner Height b3
among.b.3<- glmmTMB(B ~ Pos + max.flw + max.branch + (1|PlantID), 
                 
                    family='gaussian', data=b3)

testDispersion(among.b.3)
simulateResiduals(fittedModel = among.b.3, plot = T)
Anova(among.b.3, type=3)

summary(among.b.3)

among.b.3.emm<- emmeans(among.b.3, "Pos", type='response')
plot(among.b.3.emm)
emmeans(among.b.3, "Pos", type='response')


## Banner Height b4
among.b.4<- glmmTMB(B ~ Pos + max.flw + max.branch + (1|PlantID), 
                   
                    family='gaussian', data=b4)

testDispersion(among.b.4)
simulateResiduals(fittedModel = among.b.4, plot = T)
Anova(among.b.4, type=3)

summary(among.b.4)

among.b.4.emm<- emmeans(among.b.4, "Pos", type='response')
plot(among.b.4.emm)
emmeans(among.b.4, "Pos", type='response')


## Banner Height b5
among.b.5<- glmmTMB(B ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b5)

testDispersion(among.b.5)
simulateResiduals(fittedModel = among.b.5, plot = T)
Anova(among.b.5, type=3)

summary(among.b.5)

among.b.5.emm<- emmeans(among.b.5, "Pos", type='response')
plot(among.b.5.emm)
emmeans(among.b.5, "Pos", type='response')

## Banner Height b6
among.b.6<- glmmTMB(B ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b6)

testDispersion(among.b.6)
simulateResiduals(fittedModel = among.b.6, plot = T)
Anova(among.b.6, type=3)

summary(among.b.6)

among.b.6.emm<- emmeans(among.b.6, "Pos", type='response')
plot(among.b.6.emm)
emmeans(among.b.6, "Pos", type='response')

## Banner Height b7
among.b.7<- glmmTMB(B ~ Pos + max.flw + (1|PlantID), 
                    
                    family='gaussian', data=b7)

testDispersion(among.b.7)
simulateResiduals(fittedModel = among.b.7, plot = T)
Anova(among.b.7, type=3)

summary(among.b.7)

among.b.7.emm<- emmeans(among.b.6, "Pos", type='response')
plot(among.b.7.emm)
emmeans(among.b.7, "Pos", type='response')


######################3
#
# Flower Length

## FL b1
among.b.1<- glmmTMB(FL ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b1)

testDispersion(among.b.1)
simulateResiduals(fittedModel = among.b.1, plot = T)
Anova(among.b.1, type=3)

summary(among.b.1)

among.b.1.emm<- emmeans(among.b.1, "Pos", type='response')
plot(among.b.1.emm)
emmeans(among.b.1, "Pos", type='response')


## FL b2
among.b.2<- glmmTMB(FL ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b2)

testDispersion(among.b.2)
simulateResiduals(fittedModel = among.b.2, plot = T)
Anova(among.b.2, type=3)

summary(among.b.2)

among.b.2.emm<- emmeans(among.b.2, "Pos", type='response')
plot(among.b.2.emm)
emmeans(among.b.2, "Pos", type='response')

## FL b3
among.b.3<- glmmTMB(FL ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b3)

testDispersion(among.b.3)
simulateResiduals(fittedModel = among.b.3, plot = T)
Anova(among.b.3, type=3)

summary(among.b.3)

among.b.3.emm<- emmeans(among.b.3, "Pos", type='response')
plot(among.b.3.emm)
emmeans(among.b.3, "Pos", type='response')


## FL b4
among.b.4<- glmmTMB(FL ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b4)

testDispersion(among.b.4)
simulateResiduals(fittedModel = among.b.4, plot = T)
Anova(among.b.4, type=3)

summary(among.b.4)

among.b.4.emm<- emmeans(among.b.4, "Pos", type='response')
plot(among.b.4.emm)
emmeans(among.b.4, "Pos", type='response')


## FL b5
among.b.5<- glmmTMB(FL ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b5)

testDispersion(among.b.5)
simulateResiduals(fittedModel = among.b.5, plot = T)
Anova(among.b.5, type=3)

summary(among.b.5)

among.b.5.emm<- emmeans(among.b.5, "Pos", type='response')
plot(among.b.5.emm)
emmeans(among.b.5, "Pos", type='response')

## FL b6
among.b.6<- glmmTMB(FL ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b6)

testDispersion(among.b.6)
simulateResiduals(fittedModel = among.b.6, plot = T)
Anova(among.b.6, type=3)

summary(among.b.6)

among.b.6.emm<- emmeans(among.b.6, "Pos", type='response')
plot(among.b.6.emm)
emmeans(among.b.6, "Pos", type='response')

## FL b7
among.b.7<- glmmTMB(FL ~ Pos + max.flw + (1|PlantID), 
                    
                    family='gaussian', data=b7)

testDispersion(among.b.7)
simulateResiduals(fittedModel = among.b.7, plot = T)
Anova(among.b.7, type=3)

summary(among.b.7)

among.b.7.emm<- emmeans(among.b.7, "Pos", type='response')
plot(among.b.7.emm)
emmeans(among.b.7, "Pos", type='response')



######################3
#
# Flower Diameter

## FD b1
among.b.1<- glmmTMB(FD ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b1)

testDispersion(among.b.1)
simulateResiduals(fittedModel = among.b.1, plot = T)
Anova(among.b.1, type=3)

summary(among.b.1)

among.b.1.emm<- emmeans(among.b.1, "Pos", type='response')
plot(among.b.1.emm)
emmeans(among.b.1, "Pos", type='response')


## FD b2
among.b.2<- glmmTMB(FD ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b2)

testDispersion(among.b.2)
simulateResiduals(fittedModel = among.b.2, plot = T)
Anova(among.b.2, type=3)

summary(among.b.2)

among.b.2.emm<- emmeans(among.b.2, "Pos", type='response')
plot(among.b.2.emm)
emmeans(among.b.2, "Pos", type='response')

## FD b3
among.b.3<- glmmTMB(FD ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b3)

testDispersion(among.b.3)
simulateResiduals(fittedModel = among.b.3, plot = T)
Anova(among.b.3, type=3)

summary(among.b.3)

among.b.3.emm<- emmeans(among.b.3, "Pos", type='response')
plot(among.b.3.emm)
emmeans(among.b.3, "Pos", type='response')


## FD b4
among.b.4<- glmmTMB(FD ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b4)

testDispersion(among.b.4)
simulateResiduals(fittedModel = among.b.4, plot = T)
Anova(among.b.4, type=3)

summary(among.b.4)

among.b.4.emm<- emmeans(among.b.4, "Pos", type='response')
plot(among.b.4.emm)
emmeans(among.b.4, "Pos", type='response')


## FD b5
among.b.5<- glmmTMB(FD ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b5)

testDispersion(among.b.5)
simulateResiduals(fittedModel = among.b.5, plot = T)
Anova(among.b.5, type=3)

summary(among.b.5)

among.b.5.emm<- emmeans(among.b.5, "Pos", type='response')
plot(among.b.5.emm)
emmeans(among.b.5, "Pos", type='response')

## FD b6
among.b.6<- glmmTMB(FD ~ Pos + max.flw + max.branch + (1|PlantID), 
                    
                    family='gaussian', data=b6)

testDispersion(among.b.6)
simulateResiduals(fittedModel = among.b.6, plot = T)
Anova(among.b.6, type=3)

summary(among.b.6)

among.b.6.emm<- emmeans(among.b.6, "Pos", type='response')
plot(among.b.6.emm)
emmeans(among.b.6, "Pos", type='response')

## FD b7
among.b.7<- glmmTMB(FD ~ Pos + max.flw + (1|PlantID), 
                    
                    family='gaussian', data=b7)

testDispersion(among.b.7)
simulateResiduals(fittedModel = among.b.7, plot = T)
Anova(among.b.7, type=3)

summary(among.b.7)

among.b.7.emm<- emmeans(among.b.7, "Pos", type='response')
plot(among.b.7.emm)
emmeans(among.b.7, "Pos", type='response')


#############################################################################
#
# Functional Regression on within vs. among variatin
#
dat<- read.csv("vicia_final_data.csv")



dat$PlantID<- as.factor(dat$PlantID)
dat$Branch<- as.factor(dat$Branch)
dat$Pos<- as.factor(dat$Pos)

# Position OR Branch 1
dat2<- subset(dat, Branch==2)

#first<- subset(dat2, Branch ==1 & Branch ==2 | Branch ==3 | Branch==4 | Branch ==5 | Branch==6 | Branch==7)

#seed set per plant (response)
seed<- aggregate(dat2$seeds, by=list(dat2$PlantID), sum)

seed$Group.1<- NULL

#calcualte total flower number
flw.no<- aggregate(as.numeric(dat2$Pos), by=list(dat2$PlantID), max)
flw.no$Group.1<- NULL

flw.no.vd<-as.matrix(flw.no)
flw.no.vd<- as.vector(flw.no.vd)


#calculate total number of branches

bno<- dat2[c("PlantID", "Branch")]

bno$Branch<- as.numeric(bno$Branch)

branch.no<- aggregate(bno$Branch, by=list(bno$PlantID), max)
branch.no$Group.1<- NULL

branch.no<- as.matrix(branch.no)
branch.no<- as.vector(branch.no)

b.no<- as.data.frame(branch.no)

######
# Prepare functional predictors
B<- dat2[c("PlantID","Pos", "B")]

FL<- dat2[c("PlantID","Pos", "FL")]

FD<- dat2[c("PlantID","Pos", "FD")]

# Reshape into long-format matrix
long<- reshape(B, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

b<-long

long<- reshape(FL, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

FL<-long

long<- reshape(FD, timevar="Pos", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

FD<-long

#load Refund
library(refund)




fit<- pfr(seed ~ lf.vd(FL, vd=flw.no.vd,basistype = "te", transform='standardized')
           ,family='poisson')

fit1<- pfr(seed ~ lf.vd(FL, vd=flw.no.vd,basistype = "te", transform='standardized')
            ,family='nb')

fit2<- pfr(seed ~ lf.vd(FL, vd=flw.no.vd,basistype = "te", transform='standardized')
           ,family='gaussian')

AIC(fit, fit1, fit2)# nb

summary(fit1)

fit1<- pfr(seed ~ lf.vd(FL, vd=flw.no.vd,basistype = "te", transform='standardized')
           ,family='nb')

fit1.1<- pfr(seed ~ lf.vd(FL, vd=flw.no.vd,basistype = "te", transform='standardized')
           + unlist(flw.no),family='nb')

fit1.2<- pfr(seed ~ lf.vd(FL, vd=flw.no.vd,basistype = "te", transform='standardized')
           + unlist(flw.no) + unlist(b.no),family='nb')

fit1.3<- pfr(seed ~ lf.vd(FL, vd=flw.no.vd,basistype = "te", transform='standardized')
             + unlist(b.no),family='nb')


AIC(fit1, fit1.1, fit1.2, fit1.3)# no cov

summary(fit1.2)

fit<- coef(fit1.3)   #Note: are these transformed?

#make absolute frstart date
fit$x<- fit$b.arg * fit$b.vd

plot(fit$x, fit$value, type="l", main="absolute")

plot(fit$b.arg, fit$value, type="l", main="relative")

write.table(fit, "banner_branch2_branchNo.csv", sep=",")
