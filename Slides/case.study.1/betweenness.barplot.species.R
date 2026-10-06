# ==========================================================
# Plot species betweenness
# ==========================================================

species_betweenness_plot = tibble(
  Species = names(species_betweenness),
  Betweenness = as.numeric(species_betweenness)
) %>%
  ggplot(
    aes(
      x = reorder(Species, Betweenness),
      y = Betweenness
    )
  ) +
  geom_col(
    fill = "#8BCF5B",
    width = 0.7
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Betweenness"
  ) +
  theme_void() +
  theme(
    text = element_text(
      colour = "white"
    ),
    axis.text = element_text(
      colour = "white",
      size = 12
    ),
    axis.title.x = element_text(
      colour = "white",
      size = 12,
      margin = margin(t = 8)
    ),
    axis.ticks.x = element_line(
      colour = "white"
    ),
    axis.ticks.length = grid::unit(
      0.15,
      "cm"
    ),
    axis.line.x = element_line(
      colour = "white"
    ),
    plot.margin = margin(
      5, 5, 5, 5
    )
  )

species_betweenness_plot


# ==========================================================
# Save as transparent PNG
# ==========================================================

ggsave(
  "species_betweenness_barplot.png",
  plot = species_betweenness_plot,
  width = 6,
  height = 5,
  dpi = 600,
  bg = "transparent")