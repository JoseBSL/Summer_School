############################################################
# Summer School: Network Analysis in R
# Case Study 2: Species-habitat bipartite network (Balearic Islands)
############################################################

# Dataset
# - 15 commercially important / charismatic marine species and
#   5 benthic habitat types of the Balearic Islands
# - Edge weight = relative habitat affinity (1 = occasional, 5 = primary habitat)
# - Data defined manually below (this exercise has no raw data file)

# ==========================================================
# 1. Load libraries
# ==========================================================
library(dplyr)    # Data manipulation
library(tidyr)    # Data reshaping
library(tibble)   # tribble() and column_to_rownames()
library(bipartite)  # Network analysis and visualization
library(ggplot2)  # Data visualization
library(viridis)  # Color scales for ggplot2

# ==========================================================
# 2. Define species-habitat association data
# ==========================================================

species_habitat_long = tribble(
  ~Species,                   ~Habitat,              ~Affinity,
  # Dusky grouper: rocky and coralligenous (adults in caves/walls)
  "Epinephelus marginatus",   "Rocky bottoms",        4,
  "Epinephelus marginatus",   "Coralligenous",        3,
  # Gilthead seabream: Posidonia (juveniles/feeding) and sandy bottoms
  "Sparus aurata",            "Posidonia oceanica",   3,
  "Sparus aurata",            "Sandy bottoms",        3,
  # White seabream: Posidonia and rocky bottoms
  "Diplodus sargus",          "Posidonia oceanica",   4,
  "Diplodus sargus",          "Rocky bottoms",        2,
  # Red mullet: sandy and rocky bottoms (forages on sediment)
  "Mullus surmuletus",        "Sandy bottoms",        3,
  "Mullus surmuletus",        "Rocky bottoms",        3,
  # European seabass: sandy, Posidonia and rocky bottoms
  "Dicentrarchus labrax",     "Sandy bottoms",        2,
  "Dicentrarchus labrax",     "Posidonia oceanica",   3,
  "Dicentrarchus labrax",     "Rocky bottoms",        3,
  # Comber: rocky and coralligenous
  "Serranus cabrilla",        "Rocky bottoms",        4,
  "Serranus cabrilla",        "Coralligenous",        3,
  # Red scorpionfish: rocky and coralligenous
  "Scorpaena scrofa",         "Rocky bottoms",        4,
  "Scorpaena scrofa",         "Coralligenous",        3,
  # Atlantic bluefin tuna: pelagic, functionally linked to sandy/coastal passage
  "Thunnus thynnus",          "Sandy bottoms",        2,
  # Common octopus: rocky, sandy and coralligenous
  "Octopus vulgaris",         "Rocky bottoms",        3,
  "Octopus vulgaris",         "Sandy bottoms",        3,
  "Octopus vulgaris",         "Coralligenous",        3,
  # Common cuttlefish: Posidonia (egg-laying) and sandy bottoms
  "Sepia officinalis",        "Posidonia oceanica",   5,
  "Sepia officinalis",        "Sandy bottoms",        2,
  # Spiny lobster: coralligenous and rocky bottoms
  "Palinurus elephas",        "Coralligenous",        4,
  "Palinurus elephas",        "Rocky bottoms",        3,
  # European lobster: rocky bottoms and maerl
  "Homarus gammarus",         "Rocky bottoms",        3,
  "Homarus gammarus",         "Maerl",                4,
  # Purple sea urchin: Posidonia and rocky bottoms
  "Paracentrotus lividus",    "Posidonia oceanica",   4,
  "Paracentrotus lividus",    "Rocky bottoms",        3,
  # Noble pen shell: exclusively Posidonia (protected/charismatic)
  "Pinna nobilis",            "Posidonia oceanica",   5,
  # Short-snouted seahorse: Posidonia and maerl
  "Hippocampus hippocampus",  "Posidonia oceanica",   4,
  "Hippocampus hippocampus",  "Maerl",                3
)

# ==========================================================
# 3. Convert the long-format data into an interaction matrix
# ==========================================================

species_habitat_matrix = species_habitat_long %>%
  pivot_wider(
    names_from = Habitat,
    values_from = Affinity,
    values_fill = 0
  ) %>%
  column_to_rownames("Species") %>%
  as.matrix()

# ==========================================================
# 4. Save prepared data
# ==========================================================

saveRDS(
  species_habitat_matrix,
  "data/Processed/species_habitat_matrix.rds")
