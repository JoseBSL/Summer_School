############################################################
# Summer School: Network Analysis in R
#Part 1: Prepare data for network analysis
############################################################

# Dataset
# - Source: Kaiser-Bunbury et al. (2017)
#   http://www.ecologia.ib.usp.br/iwdb/html/kaiser-bunbury_et_al_2017.html
# - 64 monthly plant–pollinator interaction networks
# - Mahé Island, Seychelles (September 2012–April 2013)
# - Approximately 3 hours of sampling per network

# ==========================================================
# 1. Load libraries
# ==========================================================
library(readr)  # Read CSV files
library(dplyr)  # Data manipulation
library(tidyr)  # Data reshaping

# ==========================================================
# 2. Load data
# ==========================================================
# Network data
# it uses semicolon, so we use read_csv2 instead of read_csv
networks = read_csv2("Raw/Data/visitation.networks.csv")

# Species names
plant.species = read_csv2("Raw/Data/plant.species.csv")
pollinator.species = read_csv2("Raw/Data/pollinator.species.csv")

# ==========================================================
# 3. Replace plant species IDs with species names
# ==========================================================
networks = networks %>%
  left_join(
    plant.species %>%
      select(`Plant species ID`, `Plant species name`),
    by = "Plant species ID"
  ) %>%
  mutate(`Plant species ID` = `Plant species name`)

# Exclude records for which no plant species name was available
networks = networks %>%
  filter(!is.na(`Plant species ID`))

# ==========================================================
# 4. Replace pollinator species IDs with species names
# ==========================================================
# rename pollinator species names
old_columns = tibble(`Pollinator species ID` = colnames(networks))

# Match pollinator species IDs with their species names
# coalesce() keeps the original column name when no match is found
new_columns = old_columns %>%
  left_join(
    pollinator.species %>%
      select(`Pollinator species ID`, `Pollinator species name`),
    by = "Pollinator species ID") %>%
  mutate(renamed.cols = coalesce(`Pollinator species name`, `Pollinator species ID`))
# assign new column names to network data 
colnames(networks) = new_columns$renamed.cols

# ==========================================================
# 5. Reshape network data to long format
# ==========================================================

metadata_columns = c(
  "Treatment",
  "Site",
  "Month",
  "Network ID",
  "Plant species ID",
  "Plant species name",
  "Floral abundance")

networks_long = networks %>%
  pivot_longer(
    cols = -all_of(metadata_columns),
    names_to = "Pollinator species",
    values_to = "Number of visits"
  ) %>%
  filter(`Number of visits` > 0)

# ==========================================================
# 6. Save data
# ==========================================================

write_csv2(
  networks_long,
  "Data/Processed/visitation.networks.long.csv")
