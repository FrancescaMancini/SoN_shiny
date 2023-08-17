library(tidyverse)
library(plotly)
library(shiny)

all_ind <- readRDS(
  "./Data/all_occ_lambda.rds"
)

ind_metric <- sort(unique(all_ind$metric))