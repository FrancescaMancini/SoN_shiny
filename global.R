library("tidyverse")
library("plotly")

all_ind <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/all_occ_lambda.rds"
)

ind_metric <- sort(unique(all_ind$metric))