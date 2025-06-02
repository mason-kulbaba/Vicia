
#home computer
setwd("C:/Users/mason/Dropbox/git/Vicia/")

#office computer
setwd("C:/Users/mason.kulbaba/Dropbox/git/Vicia")

data<- read.csv("vicia_final_data.csv")


# Linear selection 
library(dplyr)
library(glmmTMB)
library(car)

# mean seeds per branch

b_seeds<- aggregate(data$seeds, by=list(data$Branch), mean)

b_seeds

# Relative fitness calculation
data$PlantID<- as.factor(data$PlantID)

df<- aggregate(data$seeds, by=list(data$PlantID), sum)

colnames(df)<- c("PlantId", "tot_seeds")

df$rel_seeds<- df$tot_seeds/mean(df$tot_seeds)


# aggregate covariate
# total flowers
tot.flw<- aggregate(data$PosSeq, by=list(data$PlantID), max)

#add total flower to new data frame
df$tot.flw<- tot.flw$x

#total branches (racemes)
tot.branch<- aggregate(data$Branch, by=list(data$PlantID), max)

#add to new data frame
df$tot.branch<- tot.branch$x

df$PlantId<- as.factor(df$PlantId)


#standardize flower traits
df$mean_B<- aggregate(data$B, by=list(data$PlantID), mean)



# Selection gradients within racemes 1, 3, and 5
 model.total <- glmmTMB(rel_seeds ~ FL_z + FD_z + B_z + (1 | PlantID),
                          data = df,
                          family = gaussian())
  




# Calculate mean, variance, and CV for each trait by PlantID
trait_stats <- data %>%
  group_by(PlantID) %>%
  summarise(
    FL_mean = mean(FL, na.rm = TRUE),
    FL_var  = var(FL, na.rm = TRUE),
    FL_cv   = sqrt(FL_var) / FL_mean,
    
    FD_mean = mean(FD, na.rm = TRUE),
    FD_var  = var(FD, na.rm = TRUE),
    FD_cv   = sqrt(FD_var) / FD_mean,
    
    B_mean  = mean(B, na.rm = TRUE),
    B_var   = var(B, na.rm = TRUE),
    B_cv    = sqrt(B_var) / B_mean
  )

# FL: variance ~ mean
lm_FL <- lm(FL_var ~ FL_mean, data = trait_stats)
summary(lm_FL)

# FD: variance ~ mean
lm_FD <- lm(FD_var ~ FD_mean, data = trait_stats)
summary(lm_FD)

# B: variance ~ mean
lm_B <- lm(B_var ~ B_mean, data = trait_stats)
summary(lm_B)


library(ggplot2)

ggplot(trait_stats, aes(x = FL_mean, y = FL_var)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Variance scaling for FL", x = "Mean FL", y = "Variance FL")


trait_stats$FL_cv


############ florla integraton/correlaton ################



library(dplyr)
library(tidyr)

# define function to extract, r, se, and P-value
get_cor_stats <- function(x, y) {
  ct <- cor.test(x, y, method = "pearson")
  r <- ct$estimate
  n <- sum(complete.cases(x, y))
  se <- sqrt((1 - r^2) / (n - 2))
  data.frame(correlation = r, se = se, p_value = ct$p.value)
}


#within raceme integration

within_raceme <- data %>%
  filter(Branch %in% 1:5, Pos %in% 1:5) %>%
  group_by(Branch) %>%
  group_modify(~{
    df <- .
    bind_rows(
      get_cor_stats(df$FL, df$FD) %>% mutate(pair = "FL vs FD"),
      get_cor_stats(df$FL, df$B)  %>% mutate(pair = "FL vs B"),
      get_cor_stats(df$FD, df$B)  %>% mutate(pair = "FD vs B")
    )
  }) %>%
  ungroup() %>%
  select(Branch, pair, correlation, se, p_value)

# among racemes
across_pos <- data %>%
  filter(Branch %in% 1:5, Pos %in% 1:5) %>%
  group_by(Pos) %>%
  group_modify(~{
    df <- .
    bind_rows(
      get_cor_stats(df$FL, df$FD) %>% mutate(pair = "FL vs FD"),
      get_cor_stats(df$FL, df$B)  %>% mutate(pair = "FL vs B"),
      get_cor_stats(df$FD, df$B)  %>% mutate(pair = "FD vs B")
    )
  }) %>%
  ungroup() %>%
  select(Pos, pair, correlation, se, p_value)


within_raceme
across_pos


# heat map
library(ggplot2)
library(viridis)   # for color scale
library(dplyr)

# heatmap for within raceme correlations
ggplot(within_raceme, aes(x = pair, y = factor(Branch), fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", correlation)), color = "black", size = 4) +
  scale_fill_viridis(name = "Pearson r", limits = c(-1, 1)) +
  labs(title = "Trait Correlations Within First 5 Racemes",
       x = "Trait Pair", y = "Raceme (Branch #)") +
  theme_minimal(base_size = 13)

# heatmap for among raceme correlations
ggplot(across_pos, aes(x = pair, y = factor(Pos), fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", correlation)), color = "black", size = 4) +
  scale_fill_viridis(name = "Pearson r", limits = c(-1, 1)) +
  labs(title = "Trait Correlations by Flower Position (Across Racemes)",
       x = "Trait Pair", y = "Flower Position") +
  theme_minimal(base_size = 13)


# OK, but now add SEs, and indicated statistically significant correlations

# make label column
within_raceme <- within_raceme %>%
  mutate(sig = ifelse(p_value < 0.05, "*", ""),
         label = sprintf("%.2f\n(%.2f)%s", correlation, se, sig))

across_pos <- across_pos %>%
  mutate(sig = ifelse(p_value < 0.05, "*", ""),
         label = sprintf("%.2f\n(%.2f)%s", correlation, se, sig))


# updated within integration heat map
ggplot(within_raceme, aes(x = pair, y = factor(Branch), fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 4.2, lineheight = 0.9) +
  scale_fill_viridis(name = "Pearson r", limits = c(-1, 1)) +
  labs(
    title = "Trait Correlations Within First 5 Racemes",
    x = "Trait Pair", y = "Raceme (Branch #)",
    caption = "* indicates p < 0.001\n(SE shown in parentheses)"
  ) +
  theme_minimal(base_size = 13)

# updated among-raceme floral integratoin
ggplot(across_pos, aes(x = pair, y = factor(Pos), fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 4.2, lineheight = 0.9) +
  scale_fill_viridis(name = "Pearson r", limits = c(-1, 1)) +
  labs(
    title = "Trait Correlations by Flower Position (Across Racemes)",
    x = "Trait Pair", y = "Flower Position",
    caption = "* indicates p < 0.001\n(SE shown in parentheses)"
  ) +
  theme_minimal(base_size = 13)


###################################################################
###################################################################

# Linear selection 
library(dplyr)
library(glmmTMB)
library(car)

# Relative fitness calculation
data <- data %>%
  group_by(PlantID) %>%
  mutate(rel_fitness = seeds / mean(seeds, na.rm = TRUE)) %>%
  ungroup()


# Selection gradients within racemes 1, 3, and 5
results_within <- list()

for (raceme_id in c(1,2, 3,4, 5)) {
  df <- data %>%
    filter(Branch == raceme_id) %>%
    group_by(PlantID, Branch) %>%
    summarise(
      FL = mean(FL, na.rm = TRUE),
      FD = mean(FD, na.rm = TRUE),
      B  = mean(B, na.rm = TRUE),
      rel_fit = mean(rel_fitness, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      FL_z = scale(FL),
      FD_z = scale(FD),
      B_z  = scale(B)
    )
  
  model_within <- glmmTMB(rel_fit ~ FL_z + FD_z + B_z + (1 | PlantID),
                   data = df,
                   family = gaussian())
  
  results_within[[paste0("Raceme_", raceme_id)]] <- summary(model_within)
}

# raceme-specific results
results_within$Raceme_1
results_within$Raceme_2
results_within$Raceme_3
results_within$Raceme_4
results_within$Raceme_5



# selection gradiens among racemes at position 1, 3, and 5
results_among <- list()

for (pos in c(1,2, 3,4, 5)) {
  df <- data %>%
    filter(Pos == pos, Branch %in% 1:5) %>%
    group_by(PlantID, Pos) %>%
    summarise(
      FL = mean(FL, na.rm = TRUE),
      FD = mean(FD, na.rm = TRUE),
      B  = mean(B, na.rm = TRUE),
      rel_fit = mean(rel_fitness, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      FL_z = scale(FL),
      FD_z = scale(FD),
      B_z  = scale(B)
    )
  
  model <- glmmTMB(rel_fit ~ FL_z + FD_z + B_z + (1 | PlantID),
                   data = df,
                   family = gaussian())
  
  results_among[[paste0("Position_", pos)]] <- summary(model)
}

# raceme-specific results
results_among$Position_1
results_among$Position_2
results_among$Position_3
results_among$Position_4
results_among$Position_5

# combine results
extract_results <- function(model_list, context_label) {
  do.call(rbind, lapply(names(model_list), function(name) {
    smry <- model_list[[name]]
    
    # Pull out conditional fixed effects (traits only)
    coefs <- smry$coefficients$cond
    
    # Convert to data frame and remove intercept
    df <- as.data.frame(coefs)
    df$trait <- rownames(df)
    df <- df[df$trait != "(Intercept)", ]
    
    # Add metadata
    df$context <- context_label
    df$group <- name
    
    # Rename for clarity
    df <- df %>%
      rename(
        beta = Estimate,
        SE = `Std. Error`,
        p = `Pr(>|z|)`
      ) %>%
      select(group, context, trait, beta, SE, p)
    
    return(df)
  }))
}


# apply above function to two analyses
library(dplyr)

# Extract and combine
df_within <- extract_results(results_within, context_label = "within_raceme")
df_among  <- extract_results(results_among, context_label = "among_raceme")

selection_results <- bind_rows(df_within, df_among)

# Combine all into a single data frame
selection_results <- bind_rows(df_within, df_among)

# view
print(selection_results)
dim(selection_results)


#########################################################################
#
# Make a data set with the mean, sd, variance for each trait of each 
# individual plant for each day of flowering 

library(dplyr)

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

# save mean/sd/var data to csv file for plotting

write.table(mn, file="Results and Figures/means.csv", sep = ',', row.names = F)

# plot
library(ggplot2)

#B
ggplot(mn, aes(x = flw_date, y = mean_B, group = PlantID, color = PlantID)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_B - sd_B, ymax = mean_B + sd_B, fill = PlantID), alpha = 0.2, color = NA) +
  labs(
    x = "Day of Flowering Season",
    y = "Mean Banner Height ± SD",
    title = "Banner Height Over Time by Plant"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # hides legend if too many plants

#FL
ggplot(mn, aes(x = flw_date, y = mean_FL, group = PlantID, color = PlantID)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_FL - sd_FL, ymax = mean_FL + sd_FL, fill = PlantID), alpha = 0.2, color = NA) +
  labs(
    x = "Day of Flowering Season",
    y = "Mean Flower Length ± SD",
    title = "FLower Length Over Time by Plant"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # hides legend if too many plants

#FD
ggplot(mn, aes(x = flw_date, y = mean_FD, group = PlantID, color = PlantID)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_FD - sd_FD, ymax = mean_FD + sd_FD, fill = PlantID), alpha = 0.2, color = NA) +
  labs(
    x = "Day of Flowering Season",
    y = "Mean Flower Diameter ± SD",
    title = "FLower Diameter Over Time by Plant"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # hides legend if too many plants

####################################################################################
#
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


#load Refund - . 
library(refund)

##################
#################
# Banner Height
#################
################


fit.1<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no),family='ziP')

fit.2<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.3<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.1)
summary(fit.2)
summary(fit.3)

AIC(fit.1, fit.2, fit.3) # flw.no: branch.no interaction seems important


#make absolute flower day
fit<- coef(fit.1)
fit$x<- fit$banner.arg * fit$banner.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$banner.arg, fit$value, type="l", main="relative")


# what about stanard deviation as a functional predictor (changes over time)

# Prepare functional predictors
B.sd<- mn[c("PlantID","flw_date", "sd_B")]

# Reshape into long-format matrix
long<- reshape(B.sd, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
sd.banner<-long



fit.4.0<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.banner, vd=unlist(flw_day), basistype = 'te')
            , family='ziP')

fit.4<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.banner, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no),family='ziP')

fit.4.1<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.banner, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.4.2<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te')+ lf.vd(sd.banner, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.4.0)
summary(fit.4)
summary(fit.4.1)
summary(fit.4.2)

AIC(fit.1,fit.4.0, fit.4, fit.4.1, fit.4.2) # variance as functional predictor seems important

fit<- coef(fit.4.1)
fit$x<- fit$banner.arg * fit$banner.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$banner.arg, fit$value, type="l", main="relative")
write.table(fit, file="Results and Figures/mean analysis/banner_means_selection_SD_FUNCTIONAL_PRED.csv", sep = ',', row.names = F)

# try sd as a single (i.e., plant-level) covariate

sd.b.cov<- aggregate(data$B, by=list(data$PlantID), sd)
sd.b.cov$Group.1<- NULL

fit.5.0<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te') + unlist(sd.b.cov)
            ,family='ziP')

fit.5<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te') + unlist(sd.b.cov)
            + unlist(flw.no),family='ziP')

fit.5.1<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te') + unlist(sd.b.cov)
              + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.5.2<- pfr(seed ~ lf.vd(banner, vd=unlist(flw_day), basistype = 'te')+ unlist(sd.b.cov)
              + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.5.0)
summary(fit.5)
summary(fit.5.1)
summary(fit.5.2)

AIC(fit.4.0, fit.4, fit.4.1, fit.4.2,fit.5.0, fit.5, fit.5.1, fit.5.2)# fit.5 best AIC fit

fit<- coef(fit.5.0)
fit$x<- fit$banner.arg * fit$banner.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$banner.arg, fit$value, type="l", main="relative")

# save plotting data
write.table(fit, file="Results and Figures/mean analysis/banner_means_selection.csv", sep = ',', row.names = F)

####################
#####################
# FLOWER LENGTH
#####################
####################
fit.1<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no),family='ziP')

fit.2<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.3<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.1)
summary(fit.2)
summary(fit.3)

AIC(fit.1, fit.2, fit.3) # fit. 1 best, low AIC & simple


#make absolute flower day
fit<- coef(fit.1)
fit$x<- fit$fl.arg * fit$fl.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fl.arg, fit$value, type="l", main="relative")


# what about stanard deviation as a functional predictor (changes over time)

# Prepare functional predictors
fl.sd<- mn[c("PlantID","flw_date", "sd_FL")]

# Reshape into long-format matrix
long<- reshape(fl.sd, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
sd.fl<-long



fit.4.0<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.fl, vd=unlist(flw_day), basistype = 'te')
              , family='ziP')

fit.4<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.fl, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no),family='ziP')

fit.4.1<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.fl, vd=unlist(flw_day), basistype = 'te')
              + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.4.2<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te')+ lf.vd(sd.fl, vd=unlist(flw_day), basistype = 'te')
              + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.4.0)
summary(fit.4)
summary(fit.4.1)
summary(fit.4.2)

AIC(fit.1,fit.2, fit.3, fit.4.0, fit.4, fit.4.1, fit.4.2) # sd as functional predictor do not seem important

fit<- coef(fit.4.0)
fit$x<- fit$fl.arg * fit$fl.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fl.arg, fit$value, type="l", main="relative")
write.table(fit, file="Results and Figures/mean analysis/fl_means_selection_SD_FUNCTIONAL_PRED.csv", sep = ',', row.names = F)

# try sd as a single (i.e., plant-level) covariate

sd.fl.cov<- aggregate(data$FL, by=list(data$PlantID), sd)
sd.fl.cov$Group.1<- NULL

fit.5.0<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fl.cov)
              ,family='ziP')

fit.5<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fl.cov)
            + unlist(flw.no),family='ziP')

fit.5.1<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fl.cov)
              + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.5.2<- pfr(seed ~ lf.vd(fl, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fl.cov)
              + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.5.0)
summary(fit.5)
summary(fit.5.1)
summary(fit.5.2)

AIC(fit.4.0, fit.4, fit.4.1, fit.4.2,fit.5.0, fit.5, fit.5.1, fit.5.2)# fit.5.1 best AIC fit

fit<- coef(fit.5.2)
fit$x<- fit$fl.arg * fit$fl.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fl.arg, fit$value, type="l", main="relative")

# save plotting data
write.table(fit, file="Results and Figures/mean analysis/fl_means_selection_BRANCH_FLWNO_INT.csv", sep = ',', row.names = F)

####################
#####################
# FLOWER DIAMETER
#####################
####################
fit.1<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no),family='ziP')

fit.2<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.3<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.1)
summary(fit.2)
summary(fit.3)

AIC(fit.1, fit.2, fit.3) # fit. 1 best, low AIC & simple


#make absolute flower day
fit<- coef(fit.1)
fit$x<- fit$fd.arg * fit$fd.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fd.arg, fit$value, type="l", main="relative")


# what about stanard deviation as a functional predictor (changes over time)

# Prepare functional predictors
fd.sd<- mn[c("PlantID","flw_date", "sd_FD")]

# Reshape into long-format matrix
long<- reshape(fd.sd, timevar="flw_date", idvar=c("PlantID"), direction = "wide")
long$PlantID<- NULL
long<- as.matrix(long)
#rename
sd.fd<-long



fit.4.0<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.fd, vd=unlist(flw_day), basistype = 'te')
              , family='ziP')

fit.4<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.fd, vd=unlist(flw_day), basistype = 'te')
            + unlist(flw.no),family='ziP')

fit.4.1<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te') + lf.vd(sd.fd, vd=unlist(flw_day), basistype = 'te')
              + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.4.2<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te')+ lf.vd(sd.fd, vd=unlist(flw_day), basistype = 'te')
              + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.4.0)
summary(fit.4)
summary(fit.4.1)
summary(fit.4.2)

AIC(fit.1,fit.2, fit.3, fit.4.0, fit.4, fit.4.1, fit.4.2) # sd as functional predictor do not seem important

fit<- coef(fit.4)
fit$x<- fit$fd.arg * fit$fd.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fd.arg, fit$value, type="l", main="relative")

write.table(fit, file="Results and Figures/mean analysis/fd_means_selection_SD_FUNCTIONAL_PRED.csv", sep = ',', row.names = F)

# try sd as a single (i.e., plant-level) covariate

sd.fd.cov<- aggregate(data$FD, by=list(data$PlantID), sd)
sd.fd.cov$Group.1<- NULL

fit.5.0<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fd.cov)
              ,family='ziP')

fit.5<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fd.cov)
            + unlist(flw.no),family='ziP')

fit.5.1<- pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fd.cov)
              + unlist(flw.no) + unlist(branch.no),family='ziP')

fit.5.2<-  pfr(seed ~ lf.vd(fd, vd=unlist(flw_day), basistype = 'te') + unlist(sd.fd.cov)
               + unlist(flw.no) + unlist(branch.no) + unlist(flw.no):unlist(branch.no),family='ziP')

summary(fit.5.0)
summary(fit.5)
summary(fit.5.1)
summary(fit.5.2)

AIC(fit.4.0, fit.4, fit.4.1, fit.4.2,fit.5.0, fit.5, fit.5.1, fit.5.2)# fit.5 best AIC fit

fit<- coef(fit.5)
fit$x<- fit$fd.arg * fit$fd.vd
plot(fit$x, fit$value, type="l", main="absolute")
plot(fit$fd.arg, fit$value, type="l", main="relative")

# save plotting data
write.table(fit, file="Results and Figures/mean analysis/fd_means_selection.csv", sep = ',', row.names = F)
