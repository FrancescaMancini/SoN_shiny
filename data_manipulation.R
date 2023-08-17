all_invert_UK <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/all_invert_UK_lambda.rds"
  )$Summary %>%
  mutate(country = "UK")

all_invert_ENG <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/all_invert_ENG_lambda.rds"
  )$Summary %>%
  mutate(country = "England")

all_invert_SCO <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/all_invert_SCO_lambda.rds"
  )$Summary %>%
  mutate(country = "Scotland")

all_invert_WAL <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/all_invert_WAL_lambda.rds"
  )$Summary %>%
  mutate(country = "Wales")

all_invert_NI <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/all_invert_NI_lambda.rds"
  )$Summary %>%
  mutate(country = "Northern Ireland")

all_invert <- rbind(all_invert_UK,
                    all_invert_ENG,
                    all_invert_SCO,
                    all_invert_WAL,
                    all_invert_NI) %>%
  mutate(group = "All invertebrates")

rm(all_invert_UK,
   all_invert_ENG,
   all_invert_SCO,
   all_invert_WAL,
   all_invert_NI)

poll_UK <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pollination_UK_lambda.rds"
)$Summary %>%
  mutate(country = "UK")

poll_ENG <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pollination_ENG_lambda.rds"
)$Summary %>%
  mutate(country = "England")

poll_SCO <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pollination_SCO_lambda.rds"
)$Summary %>%
  mutate(country = "Scotland")

poll_WAL <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pollination_WAL_lambda.rds"
)$Summary %>%
  mutate(country = "Wales")


poll <- rbind(poll_UK,
              poll_ENG,
              poll_SCO,
              poll_WAL) %>%
  mutate(group = "Pollination")


rm(poll_UK,
   poll_ENG,
   poll_SCO,
   poll_WAL)


pest_con_UK <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pest_control_UK_lambda.rds"
)$Summary %>%
  mutate(country = "UK")

pest_con_ENG <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pest_control_ENG_lambda.rds"
)$Summary %>%
  mutate(country = "England")

pest_con_SCO <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pest_control_SCO_lambda.rds"
)$Summary %>%
  mutate(country = "Scotland")

pest_con_WAL <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Pest_control_WAL_lambda.rds"
)$Summary %>%
  mutate(country = "Wales")


pest_con <- rbind(pest_con_UK,
                  pest_con_ENG,
                  pest_con_SCO,
                  pest_con_WAL) %>%
  mutate(group = "Pest control")



rm(pest_con_UK,
   pest_con_ENG,
   pest_con_SCO,
   pest_con_WAL)


freshw_UK <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_UK_lambda.rds"
)$Summary %>%
  mutate(country = "UK")

freshw_ENG <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_ENG_lambda.rds"
)$Summary %>%
  mutate(country = "England")

freshw_SCO <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_SCO_lambda.rds"
)$Summary %>%
  mutate(country = "Scotland")

freshw_WAL <- readRDS(
  "/data/son-databucket/outputs/brc-indicators/Freshwater_nutrient_cycling_WAL_lambda.rds"
)$Summary %>%
  mutate(country = "Wales")


freshw <- rbind(freshw_UK,
                freshw_ENG,
                freshw_SCO,
                freshw_WAL) %>%
  mutate(group = "Freshwater nutrient cycling")



rm(freshw_UK,
   freshw_ENG,
   freshw_SCO,
   freshw_WAL)


all_ind <- rbind(all_invert,
                 poll,
                 pest_con,
                 freshw) %>%
  mutate(metric = "Occupancy")

saveRDS(all_ind,
        "/data/son-databucket/outputs/brc-indicators/all_occ_lambda.rds")
