# ==========================================================
# Binary interaction matrix for connectance
# ==========================================================

connectance_matrix = species_habitat_long %>%
  mutate(
    Interaction = if_else(
      Affinity > 0,
      "Realised",
      "Not realised"
    )
  )


# ==========================================================
# Plot interaction matrix
# ==========================================================

connectance_plot = ggplot(
  connectance_matrix,
  aes(
    x = Habitat,
    y = Species,
    fill = Interaction
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = c(
      "Realised" = "#8BCF5B",
      "Not realised" = "#343434"
    ),
    name = NULL
  ) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(
      colour = "white",
      size = 11,
      angle = 45,
      hjust = 1,
      margin = margin(t = -30)
      
    ),
    axis.text.y = element_text(
      colour = "white",
      size = 10
    ),
    legend.position = "right",
    legend.text = element_text(
      colour = "white",
      size = 11
    ),
    legend.key = element_rect(
      fill = "transparent",
      colour = NA
    ),
    plot.margin = margin(
      35, 35, 35, 35
    )
  )

connectance_plot


# ==========================================================
# Save as transparent PNG
# ==========================================================

ggsave(
  "connectance_matrix.png",
  plot = connectance_plot,
  width = 7,
  height = 6,
  dpi = 600,
  bg = "transparent"
)
