############################################################
# Summer School: Network Analysis in R
# Part 4: Species-habitat bipartite network (Balearic Islands)
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

species_habitat_long <- tribble(
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

species_habitat_matrix <- species_habitat_long %>%
  pivot_wider(
    names_from = Habitat,
    values_from = Affinity,
    values_fill = 0
  ) %>%
  column_to_rownames("Species") %>%
  as.matrix()

# ==========================================================
# 4. Plot the network as a bipartite graph
#
# a) Unweighted network using bipartite
# b) Weighted network using bipartite
# ==========================================================
graphics.off()

# Create an unweighted interaction matrix
species_habitat_matrix_binary <- species_habitat_matrix
species_habitat_matrix_binary[species_habitat_matrix_binary > 0] <- 1

# Unweighted network
plotweb(
  species_habitat_matrix_binary,
  lower_color = "darkorange2",
  higher_color = "steelblue3",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 1,
  text_size = "auto",
  srt = 90,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1))

# Weighted network
plotweb(
  species_habitat_matrix,
  lower_color = "darkorange2",
  higher_color = "steelblue3",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 1,
  text_size = "auto",
  srt = 90,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1))

# ==========================================================
# 5. Plot the network as an interaction matrix (ggplot2)
# ==========================================================

# Complete the interaction matrix so empty pairs show as blank tiles
species_habitat_complete <- species_habitat_long %>%
  complete(
    Species,
    Habitat,
    fill = list(Affinity = 0)
  ) %>%
  mutate(Affinity = na_if(Affinity, 0))

# Plot the interaction matrix
ggplot(
  species_habitat_complete,
  aes(
    x = Habitat,
    y = Species,
    fill = Affinity
  )
) +
  geom_tile(
    colour = "grey70",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    na.value = "white",
    name = "Habitat\naffinity"
  ) +
  coord_equal() +
  labs(
    x = "Habitat",
    y = "Species"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5))

# ==========================================================
# 6. Plot the weighted bipartite network using ggplot2
# ==========================================================

# Order species by their total habitat affinity
species_nodes <- species_habitat_long %>%
  group_by(Species) %>%
  summarise(
    total_affinity = sum(Affinity),
    .groups = "drop"
  ) %>%
  arrange(desc(total_affinity)) %>%
  mutate(
    x_species = seq_along(Species),
    y_species = 0
  )

# Order habitats by their total affinity
habitat_nodes <- species_habitat_long %>%
  group_by(Habitat) %>%
  summarise(
    total_affinity = sum(Affinity),
    .groups = "drop"
  ) %>%
  arrange(desc(total_affinity)) %>%
  mutate(
    x_habitat = seq(
      1,
      nrow(species_nodes),
      length.out = n()
    ),
    y_habitat = 1
  )

# Add node coordinates to each interaction
network_edges <- species_habitat_long %>%
  left_join(
    species_nodes %>%
      select(Species, x_species, y_species),
    by = "Species"
  ) %>%
  left_join(
    habitat_nodes %>%
      select(Habitat, x_habitat, y_habitat),
    by = "Habitat"
  )

# Plot the weighted bipartite network
ggplot() +
  geom_segment(
    data = network_edges,
    aes(
      x = x_species,
      y = y_species,
      xend = x_habitat,
      yend = y_habitat,
      linewidth = Affinity
    ),
    colour = "grey60",
    alpha = 0.7
  ) +
  geom_point(
    data = species_nodes,
    aes(x = x_species, y = y_species, size = total_affinity),
    colour = "darkorange2",
    show.legend = FALSE
  ) +
  geom_point(
    data = habitat_nodes,
    aes(x = x_habitat, y = y_habitat, size = total_affinity),
    colour = "steelblue3",
    show.legend = FALSE
  ) +
  geom_text(
    data = species_nodes,
    aes(
      x = x_species,
      y = y_species,
      label = Species
    ),
    angle = 90,
    hjust = 1.1,
    size = 3
  ) +
  geom_text(
    data = habitat_nodes,
    aes(
      x = x_habitat,
      y = y_habitat,
      label = Habitat
    ),
    angle = 90,
    hjust = -0.1,
    size = 3
  ) +
  scale_linewidth_continuous(
    name = "Habitat affinity",
    range = c(0.2, 3)
  ) +
  coord_cartesian(
    ylim = c(-0.35, 1.35),
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.margin = margin(70, 70, 70, 70))

# ==========================================================
# 7. Basic network metrics
# ==========================================================

# Habitat degree = number of associated species (generalist vs specialist habitats)
habitat_degree <- species_habitat_long %>%
  count(Habitat, name = "n_species") %>%
  arrange(desc(n_species))

habitat_degree

# Species degree = number of habitats used (generalists vs specialists)
species_degree <- species_habitat_long %>%
  count(Species, name = "n_habitats") %>%
  arrange(desc(n_habitats))

species_degree

# ==========================================================
# 8. Connectance
# ==========================================================
# Proportion of realised links out of all possible species-habitat links

network_connectance <- networklevel(species_habitat_matrix, index = "connectance")
network_connectance

# Interpretation: 40% of all possible species-habitat combinations occur.
# This is comparatively high for an ecological network - most species use
# 2-3 of the 5 broad habitat categories, and each habitat is shared by
# several species, so the network is fairly dense rather than highly
# specialised at this coarse habitat resolution.

# ==========================================================
# 9. Centrality
# ==========================================================
# Degree, betweenness and closeness for each species and habitat
# (betweenness/closeness are computed on the bipartite graph, so a
# node is "central" if it sits on many species-habitat shortest paths)

network_centrality <- specieslevel(
  species_habitat_matrix,
  index = c("degree", "betweenness", "closeness"),
  level = "both"
)

species_centrality <- network_centrality[["lower level"]] %>%
  rownames_to_column("Species") %>%
  as_tibble() %>%
  arrange(desc(degree))

habitat_centrality <- network_centrality[["higher level"]] %>%
  rownames_to_column("Habitat") %>%
  as_tibble() %>%
  arrange(desc(degree))

species_centrality
habitat_centrality

# Interpretation:
# - Dicentrarchus labrax and Octopus vulgaris are the most generalist and
#   central species (degree 3, highest betweenness): they use the widest
#   range of habitats and sit on the shortest paths connecting otherwise
#   distant habitats.
# - Thunnus thynnus and Pinna nobilis are the most specialised/peripheral
#   species (degree 1, lowest closeness), each tied to a single habitat.
# - Rocky bottoms is by far the most central habitat (degree 10, highest
#   betweenness and closeness): most species pass "through" it, making it
#   the structural core of the network.
# - Maerl is the most peripheral habitat (degree 2), used only by Homarus
#   gammarus and Hippocampus hippocampus.

# ==========================================================
# 10. Modularity
# ==========================================================
# Group species and habitats into modules of tightly linked partners,
# then plot the network coloured by module

modules <- computeModules(species_habitat_matrix)

# Modularity score (Q): higher values indicate more distinct modules
modularity_Q <- modules@likelihood
modularity_Q

plotModuleWeb(modules)

# Network roles from within-module degree (z) and among-module
# connectivity (c), following Olesen et al. (2007):
# - Network hub: z > 2.5 and c > 0.62 (well-connected across the whole network)
# - Module hub:  z > 2.5 and c <= 0.62 (well-connected within its own module)
# - Connector:   z <= 2.5 and c > 0.62 (links several modules)
# - Peripheral:  z <= 2.5 and c <= 0.62 (mostly within-module links)

classify_role <- function(z, c) {
  case_when(
    z > 2.5 & c > 0.62 ~ "Network hub",
    z > 2.5 ~ "Module hub",
    c > 0.62 ~ "Connector",
    TRUE ~ "Peripheral"
  )
}

# z is undefined (NA) for modules where all nodes share the same
# within-module degree (e.g. single-node modules); treated here as 0
species_roles <- czvalues(modules, weighted = TRUE, level = "lower") %>%
  {tibble(Species = names(.$c), c = .$c, z = replace_na(.$z, 0))} %>%
  mutate(role = classify_role(z, c)) %>%
  arrange(desc(z), desc(c))

habitat_roles <- czvalues(modules, weighted = TRUE, level = "higher") %>%
  {tibble(Habitat = names(.$c), c = .$c, z = replace_na(.$z, 0))} %>%
  mutate(role = classify_role(z, c)) %>%
  arrange(desc(z), desc(c))

species_roles
habitat_roles

# Interpretation:
# - Modularity Q = 0.40 indicates a moderately strong modular structure:
#   despite the network's relatively high connectance, species and
#   habitats still cluster into distinct groups - e.g. a hard-bottom guild
#   (grouper, comber, scorpionfish, lobster on rocky/coralligenous ground)
#   versus a soft-bottom/Posidonia guild (seabream, cuttlefish, pen shell,
#   seahorse).
# - Most species and habitats are "Peripheral": their links stay mostly
#   within their own module, consistent with fairly distinct ecological
#   guilds.
# - Dicentrarchus labrax and Rocky bottoms are the only "Connector" nodes:
#   the seabass uses sandy, Posidonia AND rocky bottoms, and Rocky bottoms
#   is shared by species from several guilds, so both link modules that
#   would otherwise be separate.
# - No node reaches z > 2.5, so there are no strict "hub" species/habitats
#   here - expected for a network this small, where within-module degree
#   has little room to vary.
