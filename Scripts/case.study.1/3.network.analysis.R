############################################################
# Summer School: Network Analysis in R
# Case Study 2: Species-habitat bipartite network
# 3. Network analysis
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(dplyr)
library(tidyr)
library(tibble)
library(bipartite)


# ==========================================================
# 2. Load interaction matrix
# ==========================================================

species_habitat_matrix = readRDS(
  "data/Processed/species_habitat_matrix.rds")


# ==========================================================
# 3. Degree
# ==========================================================

# Habitat degree = number of associated species
habitat_degree = colSums(species_habitat_matrix > 0)

habitat_degree

# Plot habitat degree
habitat_degree %>%
  enframe(
    name = "Habitat",
    value = "Degree"
  ) %>%
  ggplot(
    aes(
      x = reorder(Habitat, Degree),
      y = Degree
    )
  ) +
  geom_col(
    fill = "steelblue3") +
  coord_flip() +
  labs(
    x = "Habitat",
    y = "Degree") +
  theme_bw() +
  scale_y_continuous(
    breaks = scales::breaks_width(1)
  )

# Species degree = number of habitats used
species_degree = rowSums(species_habitat_matrix > 0)

species_degree

# Plot habitat degree
species_degree %>%
  enframe(
    name = "Habitat",
    value = "Degree"
  ) %>%
  ggplot(
    aes(
      x = reorder(Habitat, Degree),
      y = Degree
    )
  ) +
  geom_col(
    fill = "steelblue3"
  ) +
  coord_flip() +
  labs(
    x = "Species",
    y = "Degree"
  ) +
  theme_bw()

# ==========================================================
# 4. Connectance
# ==========================================================

# Connectance is the proportion of realised interactions
# out of all possible interactions:
#
# C = L / (P * H)
#
# where:
# L = number of realised links
# P = number of species
# H = number of habitats


# Number of realised links
L = sum(species_habitat_matrix > 0)

L


# Number of species
P = nrow(species_habitat_matrix)

P


# Number of habitats
H = ncol(species_habitat_matrix)

H


# Number of possible links
possible_links = P * H

possible_links


# Connectance
connectance_manual = L / possible_links

connectance_manual

network_connectance = networklevel(
  species_habitat_matrix,
  index = "connectance")

network_connectance


# Interpretation:
# 40% of all possible species-habitat combinations occur.
# Most species use 2-3 of the 5 broad habitat categories,
# and each habitat is shared by several species.


# ==========================================================
# 5. Centrality
# ==========================================================

# Degree, betweenness and closeness for each species and habitat

network_centrality = specieslevel(
  species_habitat_matrix,
  index = c(
    "degree",
    "betweenness"),
  level = "both")

# Species centrality
species_centrality = network_centrality[["lower level"]] %>%
  rownames_to_column("Species") %>%
  as_tibble() %>%
  arrange(desc(degree))

species_centrality


# Plot species degree
ggplot(
  species_centrality,
  aes(
    x = reorder(Species, betweenness),
    y = betweenness
  )
) +
  geom_col(
    fill = "darkorange2"
  ) +
  coord_flip() +
  labs(
    x = "Species",
    y = "Betweenness"
  ) +
  theme_bw()


# Habitat centrality
habitat_centrality = network_centrality[["higher level"]] %>%
  rownames_to_column("Habitat") %>%
  as_tibble() %>%
  arrange(desc(degree))

habitat_centrality


# Plot species degree
ggplot(
  habitat_centrality,
  aes(
    x = reorder(Habitat, betweenness),
    y = betweenness
  )
) +
  geom_col(
    fill = "darkorange2"
  ) +
  coord_flip() +
  labs(
    x = "Habitat",
    y = "Betweenness"
  ) +
  theme_bw()


# Interpretation:
# - Dicentrarchus labrax and Octopus vulgaris are among the most
#   generalist and central species.
# - Thunnus thynnus and Pinna nobilis are specialised species,
#   each associated with a single habitat.
# - Rocky bottoms is a highly connected habitat.
# - Maerl is associated with relatively few species.


# ==========================================================
# 6. Modularity
# ==========================================================

# Identify groups of species and habitats that interact more
# strongly with each other than with the rest of the network

modules = computeModules(
  species_habitat_matrix)


# Modularity score
modularity_Q = modules@likelihood

modularity_Q
# Ranges from 0 to 1: 0 = no modularity, 1 = maximum modularity

# Visualise modules
plotModuleWeb(modules)


