############################################################
# Summer School: Network Analysis in R
# Case Study 2: Species-habitat bipartite network
# 2. Network visualization
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(dplyr)
library(tidyr)
library(tibble)
library(bipartite)
library(ggplot2)
library(viridis)


# ==========================================================
# 2. Load interaction matrix
# ==========================================================

species_habitat_matrix = readRDS(
  "data/Processed/species_habitat_matrix.rds")


# ==========================================================
# 3. Plot the network using plotweb()
# ==========================================================

# Create a binary interaction matrix
species_habitat_matrix_binary <- species_habitat_matrix
species_habitat_matrix_binary[
  species_habitat_matrix_binary > 0] <- 1


# Unweighted network
plotweb(
  species_habitat_matrix_binary,
  lower_color = "darkorange2",
  higher_color = "steelblue3",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 1,
  text_size = 0.4,
  srt = 1,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1)
)


# Weighted network
plotweb(
  species_habitat_matrix,
  lower_color = "darkorange2",
  higher_color = "steelblue3",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 1,
  text_size = 0.4,
  srt = 1,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1)
)


# ==========================================================
# 4. Convert the interaction matrix to long format
# ==========================================================

species_habitat_long = species_habitat_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Species") %>%
  pivot_longer(
    cols = -Species,
    names_to = "Habitat",
    values_to = "Affinity"
  )


# ==========================================================
# 5. Plot the network as an interaction matrix
# ==========================================================

ggplot(
  species_habitat_long,
  aes(
    x = Habitat,
    y = Species,
    fill = Affinity)) +
  geom_tile(
    colour = "grey70",
    linewidth = 0.2) +
  scale_fill_viridis_c(
    na.value = "white",
    name = "Habitat\naffinity") +
  coord_equal() +
  labs(
    x = "Habitat",
    y = "Species") +
  #theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5))

