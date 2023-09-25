# all_invert_UK <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/all_invert_UK_lambda.rds"
#   )$Summary %>%
#   mutate(country = "UK")
# 
# all_invert_ENG <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/all_invert_ENG_lambda.rds"
#   )$Summary %>%
#   mutate(country = "England")
# 
# all_invert_SCO <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/all_invert_SCO_lambda.rds"
#   )$Summary %>%
#   mutate(country = "Scotland")
# 
# all_invert_WAL <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/all_invert_WAL_lambda.rds"
#   )$Summary %>%
#   mutate(country = "Wales")
# 
# all_invert_NI <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/all_invert_NI_lambda.rds"
#   )$Summary %>%
#   mutate(country = "Northern Ireland")
# 
# all_invert <- rbind(all_invert_UK,
#                     all_invert_ENG,
#                     all_invert_SCO,
#                     all_invert_WAL,
#                     all_invert_NI) %>%
#   mutate(group = "All invertebrates")
# 
# rm(all_invert_UK,
#    all_invert_ENG,
#    all_invert_SCO,
#    all_invert_WAL,
#    all_invert_NI)
# 
# poll_UK <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pollination_UK_lambda.rds"
# )$Summary %>%
#   mutate(country = "UK")
# 
# poll_ENG <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pollination_ENG_lambda.rds"
# )$Summary %>%
#   mutate(country = "England")
# 
# poll_SCO <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pollination_SCO_lambda.rds"
# )$Summary %>%
#   mutate(country = "Scotland")
# 
# poll_WAL <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pollination_WAL_lambda.rds"
# )$Summary %>%
#   mutate(country = "Wales")
# 
# 
# poll <- rbind(poll_UK,
#               poll_ENG,
#               poll_SCO,
#               poll_WAL) %>%
#   mutate(group = "Pollination")
# 
# 
# rm(poll_UK,
#    poll_ENG,
#    poll_SCO,
#    poll_WAL)
# 
# 
# pest_con_UK <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pest_control_UK_lambda.rds"
# )$Summary %>%
#   mutate(country = "UK")
# 
# pest_con_ENG <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pest_control_ENG_lambda.rds"
# )$Summary %>%
#   mutate(country = "England")
# 
# pest_con_SCO <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pest_control_SCO_lambda.rds"
# )$Summary %>%
#   mutate(country = "Scotland")
# 
# pest_con_WAL <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Pest_control_WAL_lambda.rds"
# )$Summary %>%
#   mutate(country = "Wales")
# 
# 
# pest_con <- rbind(pest_con_UK,
#                   pest_con_ENG,
#                   pest_con_SCO,
#                   pest_con_WAL) %>%
#   mutate(group = "Pest control")
# 
# 
# 
# rm(pest_con_UK,
#    pest_con_ENG,
#    pest_con_SCO,
#    pest_con_WAL)
# 
# 
# freshw_UK <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_UK_lambda.rds"
# )$Summary %>%
#   mutate(country = "UK")
# 
# freshw_ENG <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_ENG_lambda.rds"
# )$Summary %>%
#   mutate(country = "England")
# 
# freshw_SCO <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_SCO_lambda.rds"
# )$Summary %>%
#   mutate(country = "Scotland")
# 
# freshw_WAL <- readRDS(
#   "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_WAL_lambda.rds"
# )$Summary %>%
#   mutate(country = "Wales")
# 
# 
# freshw <- rbind(freshw_UK,
#                 freshw_ENG,
#                 freshw_SCO,
#                 freshw_WAL) %>%
#   mutate(group = "Freshwater nutrient cycling")
# 
# 
# 
# rm(freshw_UK,
#    freshw_ENG,
#    freshw_SCO,
#    freshw_WAL)
# 
# 
# all_ind <- rbind(all_invert,
#                  poll,
#                  pest_con,
#                  freshw) %>%
#   mutate(metric = "Occupancy")
# 
# 
# ## plant/bryo/lichen
# 
# plant_ind <- read.csv("/data/son-databucket/outputs/brc-indicators/vp_bryo_frescalo_indicators_from70.csv") %>%
#   filter(ellenberg == "all_species") %>%
#   rename(lower = rescale_5., indicator = rescale_50.,
#          upper = rescale_95., year = Year, group = taxa) %>%
#   mutate(metric = "Occupancy") %>%
#   select(-c(Group, ellenberg, X5., X50., X95.))
# 
# 
# 
# ## put them all together
# 
# all_ind <- bind_rows(all_ind, plant_ind)
# 
# ## marine data
# 
# marine_ind <- read.csv(
#   "/data/son-databucket/outputs/SoN_Shiny_data_benthic.csv"
#   ) %>%
#   filter(country == "UK")
# 
# 
# all_ind <- bind_rows(all_ind, marine_ind)
# 
# ## exclude redundant info from the indicator data
# 
# all_ind <- select(all_ind, -metric)
# 
# saveRDS(all_ind,
#         "./Data/all_occ_lambda.rds")
# 
# 
# 
# # category data
# library(stringr)
# library(dplyr)
# 
# ind_files <- list("Freshwater_nutrient_cycling_WAL_lambda.rds",
#                "Freshwater_nutrient_cycling_SCO_lambda.rds",
#                "Freshwater_nutrient_cycling_ENG_lambda.rds",
#                "Freshwater_nutrient_cycling_UK_lambda.rds",
#                "Pest_control_WAL_lambda.rds",
#                "Pest_control_SCO_lambda.rds",
#                "Pest_control_ENG_lambda.rds",
#                "Pest_control_UK_lambda.rds",
#                "Pollination_WAL_lambda.rds",
#                "Pollination_SCO_lambda.rds",
#                "Pollination_ENG_lambda.rds",
#                "Pollination_UK_lambda.rds",
#                "all_invert_NI_lambda.rds",
#                "all_invert_WAL_lambda.rds",
#                "all_invert_SCO_lambda.rds",
#                "all_invert_ENG_lambda.rds",
#                "all_invert_UK_lambda.rds")
# 
# gather_cat_data <- function(file){
# 
#   ind <- readRDS(file.path("/data/son-databucket/outputs/brc-indicators",
#                            file))
#   ind_country <- str_trim(str_extract(file, "([:upper:]|[:space:]){2,}"))
# 
#   ind_group <- str_trim(gsub("_", " ", str_sub(gsub("_lambda.rds", "", file), end=-4)))
# 
#   st <- ind$st$species_assessment$category
# 
#   st <- data.frame(st, rep(as.factor("st"), length(st)))
# 
#   colnames(st) <- c("category","time_period")
# 
#   lt <- ind$lt$species_assessment$category
# 
#   lt <- data.frame(lt, rep(as.factor("lt"), length(lt)))
# 
#   colnames(lt) <- c("category","time_period")
# 
#   dat <- rbind(st,lt) %>%
#     mutate(country = ind_country,
#            group = ind_group) %>%
#     mutate(country = case_when(country == "ENG" ~ "England",
#                                country == "SCO" ~ "Scotland",
#                                country == "WAL" ~ "Wales",
#                                country == "NI" ~ "Northern Ireland",
#                                TRUE ~ country),
#            group = case_when(group == "all invert" ~ "All invertebrates",
#                              TRUE ~ group))
# 
# }
# 
# cat_data <- lapply(ind_files, gather_cat_data) %>%
#   bind_rows()
# 
# cat_data <- cat_data %>%
#   mutate(category = case_when(
#     category == "strong increase" ~ "Strong increase",
#     category == "increase" ~ "Moderate increase",
#     category == "no change" ~ "Little change",
#     category == "strong decrease" ~ "Strong decrease",
#     category == "decrease" ~ "Moderate decrease"
#   )) 
# 
# ## plant category data
# 
# plant_cat_file <- list.files("/data/son-databucket/outputs/brc-indicators",
#                              "Summary.csv")
# 
# gather_palnt_cat_data <- function(file){
# 
#   dat <- read.csv(file.path("/data/son-databucket/outputs/brc-indicators", file))
# 
#   dat <- dat %>%
#     mutate(propnochange = 1-(propDecrease+propIncrease)) %>%
#     relocate(propnochange, .after = propDecrease) %>%
#     pivot_longer(propDecrease:propIncrease, names_to = "category") %>%
#     mutate(category = case_when(category == "propIncrease" ~ "Increase",
#                                 category == "propDecrease" ~ "Decrease",
#                                 category == "propnochange" ~ "Little change")) 
#   
# 
#   dat_exp <- dat[rep(1:nrow(dat), round(dat[,4] * dat[,1])$value),-4]
# 
#   dat_exp <- dat_exp %>%
#     mutate(time_period = "lt",
#            group = gsub("Summary.csv", "", file)) %>%
#     mutate(group = case_when(group == "bryo" ~ "Bryophytes",
#                              group == "lichen" ~ "Lichens",
#                              group == "vasc" ~ "Vascular plants")) %>%
#     select(-numTaxa)
# 
# 
# }
# 
# 
# plant_cat_data <- lapply(plant_cat_file, gather_palnt_cat_data) %>%
#   bind_rows()
# 
# 
# cat_data <- bind_rows(cat_data, plant_cat_data) %>%
#   mutate(category = factor(category, ordered = TRUE,
#                            levels = c("Strong decrease",
#                                       "Moderate decrease",
#                                       "Decrease",
#                                       "Little change",
#                                       "Moderate increase",
#                                       "Increase",
#                                       "Strong increase")),
#          time_period = factor(time_period))
# 
# 
# saveRDS(cat_data,
#         "./Data/cat_data.rds")
# 
# 
# interpretation plot
# 
# int_plot <- ggplot(data = all_ind %>%
#                      filter(group == "All invertebrates") %>%
#                      filter(country == "UK"),
#                    aes(x = year, y = indicator)) +
#   geom_line(colour = "black") +
#   geom_ribbon(
#     aes(x = year, ymax = upper, ymin = lower),
#     fill = "black", alpha = 0.3) +
#   xlab("Year") +
#   ylab("Distribution index") +
#   theme_minimal() +
#   theme(text = element_text(size = 20))
# 
# png("Data/interpretation_plot.png", height = 5,
#     width = 7, units = "in", res = 300)
# int_plot
# dev.off()
# 
# 
# abundance data
# 
# UK_abnd <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/uk/uk_son23_geo_tpsci_ops_testing_240623.csv"
#   ) %>%
#   filter(group == "all") %>%
#   select(rescale_t5tps_ind5, rescale_t5tps_lci5,
#          rescale_t5tps_uci5, rescale_t5index,
#          group, year) %>%
#   rename(indicator = rescale_t5tps_ind5,
#          upper = rescale_t5tps_lci5,
#          lower = rescale_t5tps_uci5,
#          indicator_unsm = rescale_t5index) %>%
#   mutate(country = "UK")
# 
# eng_abnd <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/eng/eng_son23_geo_tpsci_ops_testing.csv"
# ) %>%
#   filter(group == "all") %>%
#   select(rescale_t5tps_ind5, rescale_t5tps_lci5,
#          rescale_t5tps_uci5, rescale_t5index,
#          group, year) %>%
#   rename(indicator = rescale_t5tps_ind5,
#          upper = rescale_t5tps_lci5,
#          lower = rescale_t5tps_uci5,
#          indicator_unsm = rescale_t5index) %>%
#   mutate(country = "England")
# 
# sco_abnd <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/scotland/allgrpscotland_son_geomean_smun_090523.csv"
# ) %>%
#   filter(group == "all") %>%
#   select(rescale_t3tps_ind3, rescale_t3tps_lci3,
#          rescale_t3tps_uci3, rescale_t3index,
#          group, year) %>%
#   rename(indicator = rescale_t3tps_ind3,
#          upper = rescale_t3tps_lci3,
#          lower = rescale_t3tps_uci3,
#          indicator_unsm = rescale_t3index) %>%
#   mutate(country = "Scotland")
# 
# 
# wal_abnd <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/wales/allgrpwales_son_geomean_smun_080523.csv"
# ) %>%
#   filter(group == "all") %>%
#   select(rescale_t3tps_ind3, rescale_t3tps_lci3,
#          rescale_t3tps_uci3, rescale_t3index,
#          group, year) %>%
#   rename(indicator = rescale_t3tps_ind3,
#          upper = rescale_t3tps_lci3,
#          lower = rescale_t3tps_uci3,
#          indicator_unsm = rescale_t3index) %>%
#   mutate(country = "Wales")
# 
# all_ind_abnd <- bind_rows(UK_abnd, eng_abnd, sco_abnd, wal_abnd)
# 
# saveRDS(all_ind_abnd,
#         "./Data/all_ind_abnd.rds")
# 
# 
# # change categories
# 
# UK_abnd_cat_data <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/uk/uk_son23_catchg_sum_geomean_ops_testing.csv"
#   ) %>%
#   filter(group == "TOTAL" & method == "tps") %>%
#   select(-c(group, method)) %>%
#   mutate(country = "UK")
# 
# eng_abnd_cat_data <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/eng/eng_son23_catchg_sum_geomean_ops_testing.csv"
# ) %>%
#   filter(group == "TOTAL" & method == "tps") %>%
#   select(-c(group, method)) %>%
#   mutate(country = "England")
# 
# sco_abnd_cat_data <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/scotland/scot_son23_catchg_sum_geomean_ops_testing.csv"
# ) %>%
#   filter(group == "TOTAL" & method == "tps") %>%
#   select(-c(group, method)) %>%
#   mutate(country = "Scotland")
# 
# wal_abnd_cat_data <- read.csv(
#   "/data/notebooks/rstudio-sonukabund/results/wales/wales_son23_catchg_sum_geomean_ops_testing.csv"
# ) %>%
#   filter(group == "TOTAL" & method == "tps") %>%
#   select(-c(group, method)) %>%
#   mutate(country = "Wales")
# 
# abnd_cat_data <- bind_rows(UK_abnd_cat_data, eng_abnd_cat_data,
#                            sco_abnd_cat_data, wal_abnd_cat_data) %>%
#     mutate(cat_chg = factor(cat_chg, ordered = TRUE,
#                              levels = c("Strong decrease",
#                                         "Moderate decrease",
#                                         "Little change",
#                                         "Moderate increase",
#                                         "Strong increase")),
#            period = factor(period))
# 
# saveRDS(abnd_cat_data, "./Data/abnd_cat_data.rds")
# 
# 
## exclude redundant info
# all_ind_abnd <- readRDS("./Data/all_ind_abnd.rds") %>%
#   select(-group) %>%
#   mutate(indicator = indicator * 100,
#          lower = lower * 100,
#          upper = upper * 100,
#          indicator_unsm = indicator_unsm * 100)
# 
# saveRDS(all_ind_abnd,
#         "./Data/all_ind_abnd.rds")

# line_plot <- all_ind_abnd %>%
#   filter(country == "UK") %>%
#   plot_ly(x = ~year, y = ~upper*100, type = 'scatter', mode = 'lines',
#           line = list(color = 'transparent'),
#           showlegend = FALSE, name = 'Upper CI') %>%
#   add_trace(y = ~lower*100, type = 'scatter', mode = 'lines',
#             fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
#             line = list(color = 'transparent'),
#             showlegend = FALSE, name = 'Lower CI') %>%
#   add_trace(y = ~indicator*100, type = 'scatter', mode = 'lines',
#             line = list(color='rgba(0,100,80)'),
#             name = 'Indicator') %>%
#   add_trace(y = ~indicator_unsm*100, type = 'scatter', mode = 'markers',
#             marker = list(color='rgb(0,100,80)'),
#             name = 'Indicator unsmoothed') %>%
#   layout(xaxis = list(title = 'Year'),
#          yaxis = list(range = c(0, max(all_ind_abnd$upper*100)+5),
#                       title = 'Abundance index'))
# 
# 
# 
# barplot <- abnd_cat_data %>%
#   filter(country == "UK") %>%
#   plot_ly(x = ~period, y = ~cat_prop/100,
#           type = "bar", showlegend=FALSE,
#           color = ~cat_chg) %>%
#   layout(barmode = "stack",
#          xaxis = list(title = '', tickvals = c('short-term', 'long-term'),
#                       ticktext = c('Short term', 'Long term')),
#          yaxis = list(title = 'Proportion of species',
#                       hoverformat = '.2%'))
# 
# subplot(line_plot,barplot, widths = c(0.7, 0.3)) |>
#   layout(title = "")
# 
# 
#  
# ## create metadata for download
# 
# occ_meta <- data.frame(
#   column_name = c("indicator",
#                   "lower",
#                   "upper",
#                   "year",
#                   "Species_Number",
#                   "country",
#                   "group",
#                   "time_period",
#                   "category",
#                   "n_species",
#                   "proportion_species"),
#   description = c(
#     "The distribution index. Scaled to be 100 in the first year.",
#     "For invertebrates and benthic: lower 95% Bayesian credible interval. For plants, bryophytes and lichens: lower 90% uncertainty interval.",
#     "For invertebrates and benthic: upper 95% Bayesian credible interval. For plants, bryophytes and lichens: upper 90% uncertainty interval.",
#     "The year in the time series.",
#     "The number of species contributing to the indicator in that year. This is set to 0 for the first year.",
#     "The country selected by the user.",
#     "The taxonomic group selected by the user.",
#     "The time period for the change category. One of: 'lt' - long term, from first to last year of the indicator; or 'st' - short term, the last 10 years of the indicator.",
#     "The category of change.",
#     "The number of species in that category of change for the time period.",
#     "The proportion of the total number of species in that category of change for the time period.")
# )
# 
# 
# abnd_meta <- data.frame(
#   column_name = c("indicator",
#                   "lower",
#                   "upper",
#                   "indicator_unsm",
#                   "year",
#                   "country",
#                   "period",
#                   "cat_chg",
#                   "cat_num",
#                   "cat_prop"),
#   description = c(
#     "The smoothed abundance index. Scaled to be 100 in the first year.",
#     "Lower 95% confidence interval. ",
#     "Upper 95% confidence interval. ",
#     "The unsmoothed abundance index.",
#     "The year in the time series.",
#     "The country selected by the user.",
#     "The time period for the change category. One of: 'long-term' - from first to last year of the indicator; or 'short-term', the last 10 years of the indicator.",
#     "The category of change",
#     "The number of species in that category of change for the time period.",
#     "The proportion of the total number of species in that category of change for the time period.")
# )
# 
# saveRDS(occ_meta, "Data/occ_meta.rds")
# saveRDS(abnd_meta, "Data/abnd_meta.rds")
# 
