
#home computer
setwd("C:/Users/mason/Dropbox/git/Vicia/")

#office computer
setwd("C:/Users/mason.kulbaba/Dropbox/git/Vicia")

dat<- read.csv("vicia_final_data.csv")


######
# Prepare functional predictors and variable domain 
B<- dat[c("PlantID","PosSeq", "B")]

FL<- dat[c("PlantID","PosSeq", "FL")]

FD<- dat[c("PlantID","PosSeq", "FD")]
# Reshape into long-format matrix
long<- reshape(B, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
Banner.functional<- as.matrix(long)

# Reshape into long-format matrix
long<- reshape(FL, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FL.functional<- as.matrix(long)


# Reshape into long-format matrix
long<- reshape(FD, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
FD.functional<- as.matrix(long)

# set up variable domain:
# "seqpos" -> position of flower in continuous sequence (across all branches)
seqpos<- aggregate(B$PosSeq, by=list(B$PlantID), max)
seqpos$Group.1<- NULL

seqpos<- as.matrix(seqpos)
seqpos<- as.vector(seqpos)

#############################################################################
#############################################################################
# assemble seeds set (fitness) and covariate data (branch no. and flower no.)
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

PlantID<- as.data.frame(unique(bno$PlantID))
colnames(PlantID)<- c("PlantID")

################################################################################
################################################################################

# Below is the actual functional regression work

# Load required packages
library(refund)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MuMIn)  # For model comparison using AIC
library(caret)  # For cross-validation
library(lme4)   # For mixed-effects models
library(mgcv)   # For GAM models (nonlinear interactions)

# Set seed for reproducibility
set.seed(1729)




### --- 1. Adding Random Effects for Plants ---
fit_random_effects <- pfr(seed ~ 
                            lf.vd(seqpos, Banner.functional, id = interaction(plant_id, branch_id), 
                                  basistype = "bspline", nbasis = 10) + 
                            total_flowers + total_branches + total_flowers:total_branches + site + 
                            (1 | plant_id),  # Random intercept for plants
                          method = "REML")

summary(fit_random_effects)
plot(fit_random_effects)

### --- 2. Checking for Nonlinear Interactions ---
# Fit a generalized additive model (GAM) to allow for nonlinear interactions
fit_gam <- gam(seed_set ~ 
                 s(total_flowers, total_branches, bs = "tp") +  # Smooth interaction between flower & branch number
                 s(flower_position, bs = "bs") + 
                 site, data = plant_covariates, method = "REML")

summary(fit_gam)
plot(fit_gam, pages = 1)

### --- 3. Model Comparison (AIC and Cross-Validation) ---
# Compare AIC values
aic_results <- AIC(fit_random_effects, fit_gam)
print(aic_results)

# Select best model based on AIC
best_model <- ifelse(aic_results$AIC[1] < aic_results$AIC[2], fit_random_effects, fit_gam)

# Cross-validation setup (5-fold)
cv_folds <- createFolds(seed_set, k = 5, list = TRUE)

cv_rmse <- function(train_idx, test_idx, model) {
  train_data <- plant_covariates[train_idx, ]
  test_data <- plant_covariates[test_idx, ]
  
  # Fit model on training data
  cv_model <- update(model, data = train_data)
  
  # Predict on test data
  preds <- predict(cv_model, newdata = test_data)
  
  # Compute RMSE
  sqrt(mean((test_data$seed_set - preds)^2, na.rm = TRUE))
}

# Perform cross-validation for both models
cv_rmse_random <- sapply(cv_folds, function(fold) cv_rmse(unlist(cv_folds[-fold]), unlist(cv_folds[fold]), fit_random_effects))
cv_rmse_gam <- sapply(cv_folds, function(fold) cv_rmse(unlist(cv_folds[-fold]), unlist(cv_folds[fold]), fit_gam))

# Print cross-validation results
cat("Mean Cross-Validated RMSE (Random Effects Model):", mean(cv_rmse_random), "\n")
cat("Mean Cross-Validated RMSE (GAM Model - Nonlinear Interactions):", mean(cv_rmse_gam), "\n")
