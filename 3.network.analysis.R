############################################################
# Summer School: Network Analysis in R
# Part 3: Analyse data
############################################################

# ==========================================================
# 1. Load libraries
# ==========================================================

library(tidyverse)

# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2("data/processed/visitation.networks.long.csv")

# ==========================================================
# 3. Select one site and calculate number of visits
# ==========================================================

visits = networks_long %>% 
  filter(Site == "Bernica") %>%
  group_by(Treatment, `Network ID`) %>%
  summarise(
    Visits = sum(`Number of visits`),
    .groups = "drop")

visits