############################################################
# Summer School: Network Analysis in R
# Case Study 1: Plant-pollinator networks
# 3. Network analysis
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)      # Read CSV files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(tibble)     # Data frames
library(bipartite)  # Network analysis
library(ggplot2)    # Data visualization


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv"
)

# Rename variables
names(networks_long) = c(
  "treatment",
  "site",
  "month",
  "network_id",
  "plant_id",
  "floral_abundance",
  "plant_species",
  "pollinator_species",
  "visits"
)

# ==========================================================
# 3. Prepare example network
# ==========================================================

# Check the available network IDs
unique(networks_long$network_id)


# Select one network as an example
example_network = networks_long %>%
  filter(
    network_id == unique(network_id)[1]
  )


# Convert the long-format data into an interaction matrix
example_matrix = example_network %>%
  group_by(
    plant_id,
    pollinator_species
  ) %>%
  summarise(
    visits = sum(visits),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = pollinator_species,
    values_from = visits,
    values_fill = 0
  ) %>%
  column_to_rownames("plant_id") %>%
  as.matrix()

example_matrix

# ==========================================================
# 4. Create interaction matrix
# ==========================================================

example_matrix = example_network %>%
  group_by(
    plant_id,
    pollinator_species
  ) %>%
  summarise(
    visits = sum(visits),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = pollinator_species,
    values_from = visits,
    values_fill = 0
  ) %>%
  column_to_rownames("plant_id") %>%
  as.matrix()

example_matrix


# ==========================================================
# 5. Visualise quantitative network
# ==========================================================

png(
  "example_network_quantitative.png",
  width = 3600,
  height = 2400,
  res = 600,
  bg = "transparent")

par(col = "white")

plotweb(
  example_matrix,
  lower_color = "#8BCF5B",
  higher_color = "#B7B5DD",
  lower_border = "#8BCF5B",
  higher_border = "#B7B5DD",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 0.8,
  text_size = 0.4,
  srt = 0,
  lab_distance = 0.01,
  mar = c(0.5, 0.5, 0.5, 0.5))

dev.off()


# ==========================================================
# 6. Visualise binary network
# ==========================================================

example_matrix_binary = (example_matrix > 0) * 1

png(
  "example_network_binary.png",
  width = 3600,
  height = 2400,
  res = 600,
  bg = "transparent")

par(col = "white")

plotweb(
  example_matrix_binary,
  lower_color = "#8BCF5B",
  higher_color = "#B7B5DD",
  lower_border = "#8BCF5B",
  higher_border = "#B7B5DD",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 0.8,
  text_size = 0.4,
  srt = 0,
  lab_distance = 0.01,
  mar = c(0.5, 0.5, 0.5, 0.5))

dev.off()