############################################################
# Summer School: Network Analysis in R
# Case Study 2: Species-habitat bipartite network
# 2. Network visualization
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(tibble)     # Data frames
library(bipartite)  # Network analysis and visualization
library(ggplot2)    # Data visualization
library(viridis)    # Color scales
library(igraph)


# ==========================================================
# 2. Load interaction matrix
# ==========================================================

species_habitat_matrix = readRDS(
  "data/Processed/species_habitat_matrix.rds"
)


# ==========================================================
# 3. Plot the network as a bipartite graph
# ==========================================================

# Create an unweighted interaction matrix
species_habitat_matrix_binary = species_habitat_matrix
species_habitat_matrix_binary[
  species_habitat_matrix_binary > 0
] = 1


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
# 5. Plot the weighted bipartite network using ggplot2
# ==========================================================

# Order species by their total habitat affinity
species_nodes = species_habitat_long %>%
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
habitat_nodes = species_habitat_long %>%
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
network_edges = species_habitat_long %>%
  filter(Affinity > 0) %>%
  left_join(
    species_nodes %>%
      select(
        Species,
        x_species,
        y_species
      ),
    by = "Species"
  ) %>%
  left_join(
    habitat_nodes %>%
      select(
        Habitat,
        x_habitat,
        y_habitat
      ),
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
    aes(
      x = x_species,
      y = y_species,
      size = total_affinity
    ),
    colour = "darkorange2",
    show.legend = FALSE
  ) +
  geom_point(
    data = habitat_nodes,
    aes(
      x = x_habitat,
      y = y_habitat,
      size = total_affinity
    ),
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
# 6. Plot the network as an interaction matrix
# ==========================================================

ggplot(
  species_habitat_long,
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
# 7. Plot the bipartite network using an igraph layout
# ==========================================================

# Convert the interaction matrix to an igraph object
network_graph = graph_from_incidence_matrix(
  species_habitat_matrix,
  weighted = TRUE
)


# Calculate a force-directed layout
network_layout = layout_with_fr(
  network_graph,
  weights = E(network_graph)$weight
)


# Create node data
network_nodes = tibble(
  node = V(network_graph)$name,
  type = ifelse(
    V(network_graph)$type,
    "Habitat",
    "Species"
  ),
  x = network_layout[, 1],
  y = network_layout[, 2]
)


# Create edge data
network_edges = as_data_frame(
  network_graph,
  what = "edges"
) %>%
  left_join(
    network_nodes %>%
      select(
        from = node,
        x_from = x,
        y_from = y
      ),
    by = "from"
  ) %>%
  left_join(
    network_nodes %>%
      select(
        to = node,
        x_to = x,
        y_to = y
      ),
    by = "to"
  )


# Plot
ggplot() +
  geom_segment(
    data = network_edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to,
      linewidth = weight
    ),
    colour = "grey70",
    alpha = 0.7
  ) +
  geom_point(
    data = network_nodes,
    aes(
      x = x,
      y = y,
      colour = type
    ),
    size = 5
  ) +
  geom_text(
    data = network_nodes,
    aes(
      x = x,
      y = y,
      label = node
    ),
    nudge_y = 0.15,
    size = 3
  ) +
  scale_colour_manual(
    values = c(
      "Species" = "darkorange2",
      "Habitat" = "steelblue3"
    )
  ) +
  scale_linewidth_continuous(
    name = "Habitat affinity",
    range = c(0.2, 3)
  ) +
  coord_equal(
    clip = "off"
  ) +
  labs(
    colour = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "right"
  )

