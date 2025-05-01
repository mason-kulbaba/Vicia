
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

###### fit splines to floral traits
library(mgcv)
library(dplyr)

# STEP 1: Standardize PosSeq within each plant (variable domain)
data <- dat %>%
  group_by(PlantID) %>%
  mutate(pos_scaled = (PosSeq - min(PosSeq)) / (max(PosSeq) - min(PosSeq))) %>%
  ungroup()

# STEP 2: Fit GAM models for each trait across PosSeq using plant-specific smooths

# Flower Length (FL)
gam_FL <- gam(FL ~ s(pos_scaled, by = PlantID, bs = "tp") + PlantID, data = data)

summary(gam_FL)

plot(gam_FL, scheme=1)
# Flower Diameter (FD)
gam_FD <- gam(FD ~ s(pos_scaled, by = PlantID, bs = "tp") + PlantID, data = data)

summary(gam_FD)
# Banner height (B)
gam_B <- gam(B ~ s(pos_scaled, by = PlantID, bs = "tp") + PlantID, data = data)

summary(gam_B)

plot(gam_B, scheme = 1)

# Flower volume
gam_vol <- gam(flw_vol ~ s(pos_scaled, by = PlantID, bs = "tp") + PlantID, data = data)


plot(gam_vol, pages = 1, scheme = 1)

##########
#
# test for different trajectories among plants

# Model 1: Global spline only
mod_global <- gam(FL ~ s(pos_scaled), data = data,
                  family = Gamma(link = "log"))
mod_global.fd <- gam(FD ~ s(pos_scaled), data = data,
                     family = Gamma(link = "log"))
mod_global.b <- gam(B ~ s(pos_scaled), data = data,
                    family = Gamma(link = "log"))

# Model 2: Global spline + plant-specific deviations
mod_plant <- gam(FL ~ s(pos_scaled) + s(pos_scaled, PlantID, bs = "fs"), data = data,
                 family = Gamma(link = "log"))
mod_plant.fd <- gam(FD ~ s(pos_scaled) + s(pos_scaled, PlantID, bs = "fs"), data = data,
                    family = Gamma(link = "log"))
mod_plant.b <- gam(B ~ s(pos_scaled) + s(pos_scaled, PlantID, bs = "fs"), data = data,
                   family = Gamma(link = "log"))

# Compare models
anova(mod_global, mod_plant, test = "Chisq")
anova(mod_global.fd, mod_plant.fd, test = "Chisq")
anova(mod_global.b, mod_plant.b, test = "Chisq")
#####
#
# show how individual plant trajectories differ

# Fit the global + plant-specific splines


#############
#
# which dist is best for FL, FD, and B?

# Traits to test
traits <- c("FL", "FD", "B")

# Distributions to test
distributions <- list(
  gaussian = gaussian(),
  gamma = Gamma(link = "log"),
  inv_gauss = inverse.gaussian(link = "log"),
  nb = nb()  # NB works only for count-like traits
)

# Store results
results <- list()

# Loop over traits
for (trait in traits) {
  aic_list <- c()
  models <- list()
  
  # Loop over distributions
  for (dist_name in names(distributions)) {
    family <- distributions[[dist_name]]
    
    # Try-catch to avoid crashes on bad model fits
    formula <- as.formula(paste0(trait, " ~ s(pos_scaled) + s(pos_scaled, PlantID, bs = 'fs') + total_flw"))
    fit <- tryCatch(
      gam(formula, data = data, family = family),
      error = function(e) NULL
    )
    
    if (!is.null(fit)) {
      aic_list[dist_name] <- AIC(fit)
      models[[dist_name]] <- fit
    } else {
      aic_list[dist_name] <- NA
    }
  }
  
  # Store AICs and best model
  best_dist <- names(which.min(aic_list))
  results[[trait]] <- list(
    AICs = aic_list,
    best_distribution = best_dist,
    best_model = models[[best_dist]]
  )
}


# Print summary of best-fitting distributions
for (trait in traits) {
  cat("\nTrait:", trait, "\n")
  print(results[[trait]]$AICs)
  cat("Best-fitting distribution:", results[[trait]]$best_distribution, "\n")
}


library(dplyr)

# Compute total flowers per PlantID
data <- dat %>%
  group_by(PlantID) %>%
  mutate(total_flw = n()) %>%
  ungroup()

data <- dat %>%
  group_by(PlantID) %>%
  mutate(pos_scaled = (PosSeq - min(PosSeq)) / (max(PosSeq) - min(PosSeq))) %>%
  mutate(total_flw = n()) %>%
  ungroup()

mod <- gam(FL ~ s(pos_scaled) + s(pos_scaled, PlantID, bs = "fs") + total_flw, 
           data = data,
           family = Gamma(link = "log"))

mod.fd <- gam(FD ~ s(pos_scaled) + s(pos_scaled, PlantID, bs = "fs") + total_flw, 
           data = data,
           family = Gamma(link = "log"))

mod.b <- gam(B ~ s(pos_scaled) + s(pos_scaled, PlantID, bs = "fs") + total_flw, data = data,
             family = Gamma(link = "log"))


summary(mod)
summary(mod.fd)
summary(mod.b)
library(tidyr)
library(purrr)

# now create prediction data
# Generate prediction grid
unique_plants <- unique(data$PlantID)

# Sequence of positions from 0 to 1 (because pos_scaled is standardized)
pos_grid <- seq(0, 1, length.out = 100)

# Total flower counts for each plant
plant_info <- data %>%
  group_by(PlantID) %>%
  summarise(total_flw = first(total_flw))  # or n(), same result here

# Expand grid for predictions
newdata <- expand_grid(
  PlantID = unique(data$PlantID),
  pos_scaled = pos_grid
) %>%
  left_join(plant_info, by = "PlantID")




newdata.fd <- expand_grid(
  PlantID = unique(data$PlantID),
  pos_scaled = pos_grid
) %>%
  left_join(plant_info, by = "PlantID")


newdata.b <- expand_grid(
  PlantID = unique(data$PlantID),
  pos_scaled = pos_grid
) %>%
  left_join(plant_info, by = "PlantID")
# Predict fitted values for each plant-specific trajectory
newdata$FL_pred <- predict(mod, newdata = newdata, type = "response")

newdata.fd$FD_pred<- predict(mod.fd, newdata = newdata.fd)

newdata.b$B_pred<- predict(mod.b, newdata = newdata.b)
# plot all plant-speific splines + global spline
library(ggplot2)

# 1. Global spline dataset (no PlantID)
mod_global <- gam(FL ~ s(pos_scaled) + total_flw, data = data,
                  family = Gamma(link = "log"))

global_data <- data.frame(
  pos_scaled = pos_grid,
  total_flw = mean(data$total_flw)  # average value
)
global_data$FL_pred <- predict(mod_global, newdata = global_data, type = "response")



mod_global.fd <- gam(FD ~ s(pos_scaled) + total_flw, data = data,
                     family = Gamma(link = "log"))

global_data.fd <- data.frame(
  pos_scaled = pos_grid,
  total_flw = mean(data$total_flw)  # average value
)
global_data.fd$FD_pred <- predict(mod_global.fd, newdata = global_data.fd, type = "response")

#summary of global & plant model, and likelihood ratio test
summary(mod_global.fd)
summary(mod.fd)
anova(mod_global.fd, mod.fd, test = "Chisq")

mod_global.b <- gam(B ~ s(pos_scaled) + total_flw, data = data,
                    family = Gamma(link = "log"))

global_data.b <- data.frame(
  pos_scaled = pos_grid,
  total_flw = mean(data$total_flw)  # average value
)
global_data.b$B_pred <- predict(mod_global.b, newdata = global_data.b, type = "response")

#############################################################
# Global models with mean flower number covariate

mod_global.FL <- gam(FL ~ s(pos_scaled) + total_flw, data = data,
                  family = Gamma(link = "log"))
mod_global.FD <- gam(FD ~ s(pos_scaled) + total_flw, data = data,
                     family = Gamma(link = "log"))
mod_global.B <- gam(B ~ s(pos_scaled) + total_flw, data = data,
                     family = Gamma(link = "log"))

#####################
#
# summary of global and plant-specific models, and likelihood ratio tests


# FL
summary(mod)
summary(mod_global.FL)
anova(mod_global.FL, mod, test="Chisq")


# FD
summary(mod.fd)
summary(mod_global.FD)
anova(mod_global.FD, mod.fd, test="Chisq")

# B
summary(mod.b)
summary(mod_global.B)
anova(mod_global.B, mod.b, test="Chisq")





global_data$fit <- predict(gam(FL ~ s(pos_scaled), data = data), newdata = global_data,
                           family = Gamma(link = "log"))

global_data.fd$fit <- predict(gam(FD ~ s(pos_scaled), data = data), newdata = global_data.fd,
                              family = Gamma(link = "log"))

global_data.b$fit <- predict(gam(B ~ s(pos_scaled), data = data), newdata = global_data.b,
                             family = Gamma(link = "log"))

# 2. Plot individual plant splines (grey) and global spline (black)

# Plot FL data as example. Other figures made in SigmaPlot
ggplot() +
  geom_line(data = newdata, aes(x = pos_scaled, y = FL_pred, group = PlantID),
            color = "grey70", size = 0.6) +
  geom_line(data = global_data, aes(x = pos_scaled, y = FL_pred),
            color = "black", size = 1.2) +
  labs(
    x = "Standardized Flower Position",
    y = "Predicted Flower Length (FL, mm)",
    title = "Flower Length Trajectories Across Plants with Global Average"
  ) +
  theme_minimal()

#write.table(newdata, file="C:/Users/mason/Dropbox/git/Vicia/spline_fitting/pred_FL_plantID.csv", sep=",", row.names = F)
#write.table(global_data, file="C:/Users/mason/Dropbox/git/Vicia/spline_fitting/_pred_FL_global.csv", sep=",", row.names = F)

#write.table(newdata.fd, file="C:/Users/mason/Dropbox/git/Vicia/spline_fitting/FD_plantID.csv", sep=",", row.names = F)
#write.table(global_data.fd, file="C:/Users/mason/Dropbox/git/Vicia/spline_fitting/FD_gloval.csv", sep=",", row.names = F)

#write.table(newdata.b, file="C:/Users/mason/Dropbox/git/Vicia/spline_fitting/B_plantID.csv", sep=",", row.names = F)
#write.table(global_data.b, file="C:/Users/mason/Dropbox/git/Vicia/spline_fitting/B_gloval.csv", sep=",", row.names = F)





# flowering graphs etc.

# Calculate number of flowers opening per day
library(dplyr)
library(ggplot2)
flowers_per_day <- dat %>%
  group_by(flw_date) %>%
  summarise(n_flowers = n())

# View the result
print(flowers_per_day)

plot(flowers_per_day$flw_date, flowers_per_day$n_flowers)

####################################################################
# mean +/- SE per day
# First, count flowers opened per PlantID per day
flowers_by_plant_day <- dat %>%
  group_by(PlantID, flw_date) %>%
  summarise(n_flowers = n(), .groups = "drop")

# Now, for each day, calculate mean and SE across plants
summary_flowers <- flowers_by_plant_day %>%
  group_by(flw_date) %>%
  summarise(
    mean_n_flowers = mean(n_flowers),
    se_n_flowers = sd(n_flowers) / sqrt(n())
  )

# View the summarized data
print(summary_flowers)

# Plot mean ± SE over days
ggplot(summary_flowers, aes(x = flw_date, y = mean_n_flowers)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_n_flowers - se_n_flowers, ymax = mean_n_flowers + se_n_flowers), width = 0.2) +
  labs(x = "Day of Flowering Season", y = "Mean Number of New Flowers per Plant ± SE") +
  theme_minimal()

##############################################################
# horizontal bar plot
# Find first and last day each plant had new flowers
flowering_period <- dat %>%
  group_by(PlantID) %>%
  summarise(
    first_day = min(flw_date, na.rm = TRUE),
    last_day = max(flw_date, na.rm = TRUE),
    .groups = "drop"
  )

# Function to calculate number of overlapping plants
calculate_overlap <- function(id, first, last, all_periods) {
  overlaps <- all_periods %>%
    filter(PlantID != id) %>%  # exclude self
    filter(!(last < first_day | first > last_day))  # flowering periods overlap
  return(nrow(overlaps))
}

# Apply function for each plant
flowering_period$coflowering_plants <- mapply(
  calculate_overlap,
  flowering_period$PlantID,
  flowering_period$first_day,
  flowering_period$last_day,
  MoreArgs = list(all_periods = flowering_period)
)


# View the flowering periods
print(flowering_period)

# First reshape PlantID for ordered plotting
flowering_period <- flowering_period %>%
  mutate(PlantID = factor(PlantID, levels = PlantID[order(first_day)]))

# Now plot
ggplot(flowering_period) +
  geom_segment(aes(x = first_day, xend = last_day, y = PlantID, yend = PlantID),
               size = 2, color = "steelblue") +
  geom_line(aes(x = (first_day + last_day)/2, y = coflowering_plants), 
            color = "firebrick", size = 1) +
  geom_point(aes(x = (first_day + last_day)/2, y = coflowering_plants), 
             color = "firebrick", size = 2) +
  scale_y_discrete(name = "Plant ID") +
  scale_x_continuous(
    name = "Day of Flowering Season",
    sec.axis = sec_axis(~ ., name = "Number of Co-flowering Plants")
  ) +
  theme_minimal() +
  labs(title = "Flowering Periods and Co-flowering Dynamics")

########################################################################
#######
# more detail of above

library(dplyr)
library(tidyr)
library(ggplot2)

# Create flowering periods
flowering_period <- dat %>%
  group_by(PlantID) %>%
  summarise(
    first_day = min(flw_date, na.rm = TRUE),
    last_day = max(flw_date, na.rm = TRUE),
    .groups = "drop"
  )

# Expand to 1 row per plant per day
flowering_days <- flowering_period %>%
  rowwise() %>%
  mutate(day_seq = list(seq(first_day, last_day))) %>%
  unnest(cols = c(day_seq))


# For each day and plant, count how many other plants were flowering
coflowering_by_day <- flowering_days %>%
  group_by(day_seq) %>%
  mutate(coflowering = n() - 1) %>%  # subtract 1 to exclude self
  ungroup()

# Calculate average co-flowering across days for each plant
plant_avg_coflowering <- coflowering_by_day %>%
  group_by(PlantID) %>%
  summarise(avg_coflowering = mean(coflowering), .groups = "drop")

# Merge average coflowering into flowering_period for bar colors
flowering_period <- flowering_period %>%
  left_join(plant_avg_coflowering, by = "PlantID")

# Set threshold for "high" coflowering
high_coflowering_threshold <- quantile(flowering_period$avg_coflowering, 0.9)  # top 10%

flowering_period <- flowering_period %>%
  mutate(highlight = avg_coflowering >= high_coflowering_threshold)

# Now plot
ggplot() +
  # Flowering period bars colored by avg coflowering
  geom_segment(data = flowering_period,
               aes(x = first_day, xend = last_day, y = reorder(PlantID, first_day), yend = PlantID,
                   color = avg_coflowering),
               size = 3) +
  # Overlapping curve (per plant across days)
  geom_line(data = coflowering_by_day,
            aes(x = day_seq, y = coflowering, group = PlantID, color = PlantID),
            inherit.aes = FALSE, alpha = 0.6, size=1.5) +
  # Highlight special plants
  geom_point(data = flowering_period %>% filter(highlight),
             aes(x = (first_day + last_day)/2, y = PlantID),
             shape = 21, fill = "black", color = "black", size = 4, stroke = 1.5) +
  scale_color_viridis_c(name = "Avg Co-flowering\nPlants", option = "plasma") +
  scale_x_continuous(
    name = "Day of Flowering Season",
    sec.axis = sec_axis(~ ., name = "Number of Co-flowering Plants (per day)")
  ) +
  scale_y_discrete(name = "Plant ID") +
  theme_minimal() +
  labs(title = "Flowering Periods and Co-flowering Dynamics by Plant") +
  theme(
    axis.text.y = element_text(size = 6),
    legend.position = "bottom"
  )



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

FVOL<- dat[c("PlantID", "PosSeq", "flw_vol")]

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

# Reshape into long-format matrix
long<- reshape(FVOL, timevar="PosSeq", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)

#rename
FVOL<-long


# set up variable domain:
# "seqpos" -> position of flower in continuous sequence (across all branches)
seqpos<- aggregate(dat$PosSeq, by=list(dat$PlantID), max)
seqpos$Group.1<- NULL

seqpos<- as.matrix(seqpos)
seqpos<- as.vector(seqpos)

# max branch number
branch.no<- aggregate(as.numeric(dat$Branch), by=list(dat$PlantID), max)
branch.no$Group.1<- NULL

branch.no

branch.no<- as.matrix(branch.no)
branch.no<- as.vector(branch.no)
b.no<- as.data.frame(branch.no)

#load Refund - June 4: after chat with Lawrence: check Fig. 4 vd. 
library(refund)


fit.1<- pfr(seed ~ lf.vd(B, vd=unlist(flw.no) , transform='standardized')
            + unlist(flw.no),family='ziP')

fit.2<- pfr(seed ~ lf.vd(FL, vd=unlist(flw.no) , transform='standardized')
            + unlist(flw.no),family='ziP')

fit.3<- pfr(seed ~ lf.vd(FD, vd=unlist(flw.no) , transform='standardized')
            + unlist(flw.no),family='ziP')

fit.4<- pfr(seed ~ lf.vd(FVOL, vd=unlist(flw.no) , transform='standardized')
            + unlist(flw.no),family='ziP')

summary(fit.4)


##### combined

fit.all <- pfr(seed ~ 
                 lf.vd(B, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FD, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FL,  vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FVOL,  vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 unlist(b.no) +
                 unlist(log(flw.no)),
               family = "ziP")

summary(fit.all)

fit.all2 <- pfr(seed ~ 
                 lf.vd(B, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FD, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FL,  vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 lf.vd(FVOL,  vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                 unlist(flw.no),
               family = "ziP")

summary(fit.all2)


fit.all3 <- pfr(seed ~ 
                  lf.vd(B, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                  lf.vd(FD, vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                  lf.vd(FL,  vd = unlist(flw.no), basistype = "te", transform = 'standardized') +
                  lf.vd(FVOL,  vd = unlist(flw.no), basistype = "te", transform = 'standardized'),
                  offset=unlist(flw.no),
                family = "ziP")

summary(fit.all3)


AIC(fit.all, fit.all2, fit.all3)



########### check dists
# Full formula (same for all models)
model_formula <- seed ~ 
  lf.vd(FL, vd = unlist(flw.no), transform = "standardized") +
  lf.vd(FD, vd = unlist(flw.no), transform = "standardized") +
  lf.vd(B,  vd = unlist(flw.no), transform = "standardized") +
  unlist(log(flw.no))

summary(model_formula)

#output of results

#extract coefficients
#Extract coefficient for B
coef_B <- coef(fit.all3, select = 1)

# Extract coefficient for FD
coef_FD <- coef(fit.all3, select = 2)

# Extract coefficient for FL
coef_FL <- coef(fit.all3, select = 3)

# Extract coefficient for FVOL
coef_FVOL <- coef(fit.all3, select = 4)

names(coef_FVOL)


# flower volume
#make absolute frstart date
coef_FVOL$x<- coef_FVOL$FVOL.arg * coef_FVOL$FVOL.vd

plot(coef_FVOL$x, coef_FVOL$value, type="l", main="absolute")

plot(coef_FVOL$FVOL.arg, coef_FVOL$value, type="l", main="relative")


# Banner
#make absolute frstart date
coef_B$x<- coef_B$B.arg * coef_B$B.vd

plot(coef_B$x, coef_B$value, type="l", main="absolute")

plot(coef_B$B.arg, coef_B$value, type="l", main="relative")


######################################################################
#
# how to incorporate flower pos and branch ID?

library(dplyr)
library(tidyr)
library(refund)
library(tibble)

# STEP 1: Create a BranchID for each unique PlantID + Branch combo
library(dplyr)
library(tidyr)
library(refund)
library(tibble)

# STEP 1: Create BranchID
data <- dat %>%
  mutate(BranchID = paste(PlantID, Branch, sep = "_"))

# Optional: Filter to Pos ≤ 30 if flower positions get very high
data <- data %>%
  filter(Pos <= 30)

# STEP 2: Function to create wide matrix of trait values by BranchID × Pos
reshape_branch_trait <- function(data, trait_name) {
  data %>%
    select(BranchID, Pos, !!sym(trait_name)) %>%
    group_by(BranchID, Pos) %>%
    summarize(val = mean(!!sym(trait_name), na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Pos, values_from = val) %>%
    column_to_rownames("BranchID") %>%
    as.matrix()
}

# STEP 3: Create trait matrices (wide format)
FL_mat <- reshape_branch_trait(data, "FL")
FD_mat <- reshape_branch_trait(data, "FD")
B_mat  <- reshape_branch_trait(data, "B")

# STEP 4: Create summary data per Branch
branch_data <- data %>%
  group_by(BranchID) %>%
  summarize(
    Seeds = mean(seeds, na.rm = TRUE),
    PlantID = first(PlantID),
    n_flw = n(),
    .groups = "drop"
  ) %>%
  filter(BranchID %in% rownames(FL_mat)) %>%
  arrange(BranchID)

# Align trait matrices
FL_mat <- FL_mat[branch_data$BranchID, , drop = FALSE]
FD_mat <- FD_mat[branch_data$BranchID, , drop = FALSE]
B_mat  <- B_mat[branch_data$BranchID, , drop = FALSE]

# STEP 5: Run functional regression
fit_branch <- pfr(Seeds ~
                    lf.vd(FL_mat, transform = "standardized") +
                    lf.vd(FD_mat, transform = "standardized") +
                    lf.vd(B_mat, transform = "standardized") +
                    n_flw,
                  data = branch_data)

dim(FL_mat)
summary(rowSums(is.na(FL_mat)))


summary(fit_branch)
plot(fit_branch)

#########################

library(glmmTMB)

dat$ovules<- dat$seeds + dat$aborted + dat$unfert

fit<- glmmTMB(ovules ~ PosSeq:Branch, family = "nbinom1", data=dat)

summary(fit)

plot(dat$PosSeq, dat$ovules)

library(emmeans)

meh<-emmeans(fit, "PosSeq", type="response")
plot(meh)

