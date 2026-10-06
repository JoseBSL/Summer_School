# ==========================================================
# Species degree
# ==========================================================

# Species degree = number of habitats used
species_degree = rowSums(
  species_habitat_matrix > 0)

species_degree


# ==========================================================
# Plot species degree
# ==========================================================

species_degree_plot = species_degree %>%
  enframe(
    name = "Species",
    value = "Degree"
  ) %>%
  ggplot(
    aes(
      x = reorder(Species, Degree),
      y = Degree
    )
  ) +
  geom_col(
    fill = "#8BCF5B",
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = scales::breaks_width(1)
  ) +
  labs(
    x = NULL,
    y = "Degree"
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
    axis.ticks.length = unit(
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

species_degree_plot


# ==========================================================
# Save as transparent PNG
# ==========================================================

ggsave(
  "species_degree.png",
  plot = species_degree_plot,
  width = 6,
  height = 5,
  dpi = 600,
  bg = "transparent")