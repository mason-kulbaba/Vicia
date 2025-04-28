
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
seqpos<- aggregate(B$PosSeq, by=list(B$PlantID), max)
seqpos$Group.1<- NULL

seqpos<- as.matrix(seqpos)
seqpos<- as.vector(seqpos)




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
