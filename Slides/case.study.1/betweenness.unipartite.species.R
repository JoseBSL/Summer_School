# ==========================================================
# 7. Species projection
# ==========================================================

# Convert species-habitat matrix to presence-absence
species_habitat_binary =
  (species_habitat_matrix > 0) * 1


# ==========================================================
# Create species projection
# ==========================================================

# Species are connected if they share at least one habitat
species_projection_matrix =
  species_habitat_binary %*%
  t(species_habitat_binary)

# Remove self-links
diag(species_projection_matrix) = 0

# Convert to presence-absence
species_projection_matrix =
  (species_projection_matrix > 0) * 1


# ==========================================================
# Convert to igraph
# ==========================================================

species_graph = graph_from_adjacency_matrix(
  species_projection_matrix,
  mode = "undirected",
  diag = FALSE)


# ==========================================================
# Calculate betweenness centrality
# ==========================================================

species_betweenness = betweenness(
  species_graph,
  directed = FALSE,
  normalized = TRUE)


# ==========================================================
# Calculate layout
# ==========================================================

set.seed(123)

species_layout = layout_with_fr(
  species_graph)


# ==========================================================
# Create node data
# ==========================================================

species_nodes = tibble(
  species = V(species_graph)$name,
  betweenness = species_betweenness,
  x = species_layout[, 1],
  y = species_layout[, 2])


# ==========================================================
# Create edge data
# ==========================================================

species_edges = as_data_frame(
  species_graph,
  what = "edges") %>%
  left_join(
    species_nodes %>%
      select(
        from = species,
        x_from = x,
        y_from = y),
    by = "from") %>%
  left_join(
    species_nodes %>%
      select(
        to = species,
        x_to = x,
        y_to = y),
    by = "to")


# ==========================================================
# Plot species projection
# ==========================================================

centrality_plot = ggplot() +
  geom_segment(
    data = species_edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to),
    colour = "grey70",
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_point(
    data = species_nodes,
    aes(
      x = x,
      y = y,
      colour = betweenness),
    size = 7
  ) +
  geom_text(
    data = species_nodes,
    aes(
      x = x,
      y = y,
      label = species),
    colour = "white",
    nudge_y = 0.18,
    size = 3
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
    legend.position = "right",
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

centrality_plot


# ==========================================================
# Save as transparent PNG
# ==========================================================

ggsave(
  "species_betweenness_projection.png",
  plot = centrality_plot,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "transparent")