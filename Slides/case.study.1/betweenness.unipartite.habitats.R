# ==========================================================
# 8. Habitat projection
# ==========================================================

# Convert species-habitat matrix to presence-absence
species_habitat_binary =
  (species_habitat_matrix > 0) * 1


# ==========================================================
# Create habitat projection
# ==========================================================

# Habitats are connected if they share at least one species
habitat_projection_matrix =
  t(species_habitat_binary) %*%
  species_habitat_binary

# Remove self-links
diag(habitat_projection_matrix) = 0

# Convert to presence-absence
habitat_projection_matrix =
  (habitat_projection_matrix > 0) * 1


# ==========================================================
# Convert to igraph
# ==========================================================

habitat_graph = graph_from_adjacency_matrix(
  habitat_projection_matrix,
  mode = "undirected",
  diag = FALSE)


# ==========================================================
# Calculate betweenness centrality
# ==========================================================

habitat_betweenness = betweenness(
  habitat_graph,
  directed = FALSE,
  normalized = TRUE)


# ==========================================================
# Calculate layout
# ==========================================================

set.seed(123)

habitat_layout = layout_with_fr(
  habitat_graph)


# ==========================================================
# Create node data
# ==========================================================

habitat_nodes = tibble(
  habitat = V(habitat_graph)$name,
  betweenness = habitat_betweenness,
  x = habitat_layout[, 1],
  y = habitat_layout[, 2])


# ==========================================================
# Create edge data
# ==========================================================

habitat_edges = as_data_frame(
  habitat_graph,
  what = "edges") %>%
  left_join(
    habitat_nodes %>%
      select(
        from = habitat,
        x_from = x,
        y_from = y),
    by = "from") %>%
  left_join(
    habitat_nodes %>%
      select(
        to = habitat,
        x_to = x,
        y_to = y),
    by = "to")


# ==========================================================
# Plot habitat projection
# ==========================================================

habitat_centrality_plot = ggplot() +
  geom_segment(
    data = habitat_edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to),
    colour = "grey70",
    linewidth = 0.7,
    alpha = 0.6
  ) +
  geom_point(
    data = habitat_nodes,
    aes(
      x = x,
      y = y,
      colour = betweenness),
    size = 9
  ) +
  geom_text(
    data = habitat_nodes,
    aes(
      x = x,
      y = y,
      label = habitat),
    colour = "white",
    nudge_y = 0.15,
    size = 4
  ) +
  scale_colour_viridis_c(
    name = "Betweenness",
    option = "plasma"
  ) +
  coord_equal(
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(
      colour = "white"
    ),
    legend.title = element_text(
      colour = "white"
    ),
    plot.margin = margin(
      20, 20, 20, 20
    )
  )

habitat_centrality_plot


# ==========================================================
# Save as transparent PNG
# ==========================================================

ggsave(
  "habitat_betweenness_projection.png",
  plot = habitat_centrality_plot,
  width = 7,
  height = 6,
  dpi = 600,
  bg = "transparent")