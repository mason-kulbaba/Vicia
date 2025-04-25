
#home computer
setwd("C:/Users/mason/Dropbox/git/Vicia/")

#office computer
setwd("C:/Users/mason.kulbaba/Dropbox/git/Vicia")

# load data
df<- read.csv("vicia_final_data.csv")

# Load required packages
library(refund)
library(dplyr)
library(tidyr)
library(ggplot2)

# aggregate seed data
plant_seed_set <- df %>%
  distinct(PlantID, seeds) %>%
  drop_na(seeds)

# function to make matrices for pfr
make_fd_matrices <- function(data, value_col, id_col = "PlantID", arg_col = "PosSeq") {
  data_sub <- data %>%
    select(all_of(c(id_col, arg_col, value_col))) %>%
    drop_na() %>%
    arrange(!!sym(id_col), !!sym(arg_col))
  
  value_list <- split(data_sub[[value_col]], data_sub[[id_col]])
  arg_list   <- split(data_sub[[arg_col]], data_sub[[id_col]])
  
  max_len <- max(lengths(value_list))
  
  value_mat <- t(sapply(value_list, function(x) c(x, rep(NA, max_len - length(x)))))
  arg_mat   <- t(sapply(arg_list,   function(x) c(x, rep(NA, max_len - length(x)))))
  
  plant_ids <- names(value_list)
  rownames(value_mat) <- plant_ids
  rownames(arg_mat)   <- plant_ids
  
  list(value = value_mat, arg = arg_mat)
}

# create functional predictor input
B_fd   <- make_fd_matrices(df, "B")
FL_fd  <- make_fd_matrices(df, "FL")
FD_fd  <- make_fd_matrices(df, "FD")
VOL_fd <- make_fd_matrices(df, "flw_vol")

#match plants across all inuts
# Get PlantIDs for each matrix
get_ids <- function(fd) rownames(fd$value)

common_ids <- Reduce(intersect, list(
  get_ids(B_fd), get_ids(FL_fd), get_ids(FD_fd), get_ids(VOL_fd),
  plant_seed_set$PlantID
))

# Enforce consistent ordering
order_ids <- function(fd) {
  list(
    value = fd$value[common_ids, , drop = FALSE],
    arg   = fd$arg[common_ids, , drop = FALSE]
  )
}

B_fd   <- order_ids(B_fd)
FL_fd  <- order_ids(FL_fd)
FD_fd  <- order_ids(FD_fd)
VOL_fd <- order_ids(VOL_fd)

Y <- plant_seed_set %>%
  filter(PlantID %in% common_ids) %>%
  arrange(factor(PlantID, levels = common_ids)) %>%
  pull(seeds)




# Sanity check
stopifnot(
  nrow(B_fd$value) == length(Y),
  nrow(FL_fd$value) == length(Y),
  nrow(FD_fd$value) == length(Y),
  nrow(VOL_fd$value) == length(Y)
)





# fit model with pfr
fit <- pfr(Y ~
             lf.vd(B_fd$value, B_fd$arg, presmooth = TRUE) +
             lf.vd(FL_fd$value, FL_fd$arg, presmooth = TRUE) +
             lf.vd(FD_fd$value, FD_fd$arg, presmooth = TRUE) +
             lf.vd(VOL_fd$value, VOL_fd$arg, presmooth = TRUE)
)

summary(fit)
plot(fit)
