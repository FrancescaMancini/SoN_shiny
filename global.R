library("tidyverse")
library("plotly")

all_ind <- readRDS(
  "./Data/all_occ_lambda.rds"
)

ind_metric <- sort(unique(all_ind$metric))