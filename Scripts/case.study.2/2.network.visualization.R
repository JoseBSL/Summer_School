############################################################
# Summer School: Network Analysis in R
# Case Study 1: Plant-pollinator networks
# 2. Network visualization
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)      # Read CSV files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(tibble)     # Data frames
library(bipartite)  # Network analysis and visualization
library(ggplot2)    # Data visualization
library(viridis)    # Color scales


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
  filter(network_id == unique(network_id)[1])

# Convert the long-format data into an interaction matrix
example_matrix = example_network %>%
  select(
    plant_id,
    pollinator_species,
    visits
  ) %>%
  pivot_wider(
    names_from = pollinator_species,
    values_from = visits,
    values_fill = 0
  ) %>%
  column_to_rownames("plant_id") %>%
  as.matrix()


# ==========================================================
# 4. Plot the network as a bipartite graph
# ==========================================================

# Create an unweighted interaction matrix
example_matrix_binary = example_matrix
example_matrix_binary[example_matrix_binary > 0] = 1


# Unweighted network
plotweb(
  example_matrix_binary,
  lower_color = "forestgreen",
  higher_color = "steelblue",
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
  example_matrix,
  lower_color = "forestgreen",
  higher_color = "steelblue",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 1,
  text_size = 0.4,
  srt = 1,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1)
)


# ==========================================================
# 5. Plot the network as an interaction matrix
# ==========================================================

# ==========================================================
# Plot an ordered interaction matrix
# ==========================================================

# Calculate plant degree
plant_order = example_network_complete %>%
  group_by(plant_id) %>%
  summarise(
    degree = sum(visits > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(degree))


# Calculate pollinator degree
pollinator_order = example_network_complete %>%
  group_by(pollinator_species) %>%
  summarise(
    degree = sum(visits > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(degree))


# Order plants and pollinators by degree
example_network_ordered = example_network_complete %>%
  mutate(
    plant_id = factor(
      plant_id,
      levels = rev(plant_order$plant_id)
    ),
    pollinator_species = factor(
      pollinator_species,
      levels = pollinator_order$pollinator_species
    )
  )


# Plot the ordered interaction matrix
ggplot(
  example_network_ordered,
  aes(
    x = pollinator_species,
    y = plant_id,
    fill = visits
  )
) +
  geom_tile(
    colour = "grey70",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    na.value = "white",
    name = "Number\nof visits"
  ) +
  coord_equal() +
  labs(
    x = "Pollinator species",
    y = "Plant species"
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

# Order plants by their total number of visits
plant_nodes = example_network %>%
  group_by(plant_id) %>%
  summarise(
    total_visits = sum(visits),
    .groups = "drop"
  ) %>%
  arrange(desc(total_visits)) %>%
  mutate(
    x_plant = seq_along(plant_id),
    y_plant = 0
  )


# Order pollinators by their total number of visits
pollinator_nodes = example_network %>%
  group_by(pollinator_species) %>%
  summarise(
    total_visits = sum(visits),
    .groups = "drop"
  ) %>%
  arrange(desc(total_visits)) %>%
  mutate(
    x_pollinator = seq(
      1,
      nrow(plant_nodes),
      length.out = n()
    ),
    y_pollinator = 1
  )


# Add node coordinates to each interaction
network_edges = example_network %>%
  select(
    plant_id,
    pollinator_species,
    visits
  ) %>%
  left_join(
    plant_nodes %>%
      select(plant_id, x_plant, y_plant),
    by = "plant_id"
  ) %>%
  left_join(
    pollinator_nodes %>%
      select(
        pollinator_species,
        x_pollinator,
        y_pollinator
      ),
    by = "pollinator_species"
  )


# Plot the weighted bipartite network
ggplot() +
  geom_segment(
    data = network_edges,
    aes(
      x = x_plant,
      y = y_plant,
      xend = x_pollinator,
      yend = y_pollinator,
      linewidth = visits
    ),
    colour = "grey60",
    alpha = 0.7
  ) +
  geom_point(
    data = plant_nodes,
    aes(
      x = x_plant,
      y = y_plant,
      size = total_visits
    ),
    colour = "forestgreen",
    show.legend = FALSE
  ) +
  geom_point(
    data = pollinator_nodes,
    aes(
      x = x_pollinator,
      y = y_pollinator,
      size = total_visits
    ),
    colour = "steelblue",
    show.legend = FALSE
  ) +
  geom_text(
    data = plant_nodes,
    aes(
      x = x_plant,
      y = y_plant,
      label = plant_id
    ),
    angle = 90,
    hjust = 1.1,
    size = 3
  ) +
  geom_text(
    data = pollinator_nodes,
    aes(
      x = x_pollinator,
      y = y_pollinator,
      label = pollinator_species
    ),
    angle = 90,
    hjust = -0.1,
    size = 3
  ) +
  scale_linewidth_continuous(
    name = "Number of visits",
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
# 7. Plot the bipartite network using an igraph layout
# ==========================================================

# Convert the interaction matrix to an igraph object
network_graph = graph_from_incidence_matrix(
  example_matrix,
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
    "Pollinator",
    "Plant"
  ),
  x = network_layout[, 1],
  y = network_layout[, 2]
)


# Create edge data
network_edges_igraph = as_data_frame(
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


# Plot the network
ggplot() +
  geom_segment(
    data = network_edges_igraph,
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
      "Plant" = "forestgreen",
      "Pollinator" = "steelblue"
    )
  ) +
  scale_linewidth_continuous(
    name = "Number of visits",
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
