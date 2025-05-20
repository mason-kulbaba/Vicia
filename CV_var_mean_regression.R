
#home computer
setwd("C:/Users/mason/Dropbox/git/Vicia/")

#office computer
setwd("C:/Users/mason.kulbaba/Dropbox/git/Vicia")

data<- read.csv("vicia_final_data.csv")


# Linear selection 
library(dplyr)
library(glmmTMB)
library(car)

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
