library(tidyverse)
library(plotly)
library(shiny)
library(openxlsx)

# read occupancy data
all_ind <- readRDS(
  "Data/all_occ_lambda.rds"
)

# extract taxonomic groups
ind_groups <- sort(unique(all_ind$group))

# read categories of change data for occupancy
cat_data <- readRDS(
  "Data/cat_data.rds"
)

# read abundance data

all_ind_abnd <- readRDS("Data/all_ind_abnd.rds")

# extract countries

abnd_countries <- sort(unique(all_ind_abnd$country))

# read categories of change data for abundance

abnd_cat_data <- readRDS("Data/abnd_cat_data.rds")
