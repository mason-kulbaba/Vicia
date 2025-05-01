library(dplyr)




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
