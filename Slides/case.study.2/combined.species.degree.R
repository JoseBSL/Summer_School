# ==========================================================
# 10. Combine degree plots for slides
# ==========================================================

slide_theme = theme(
  panel.background = element_rect(
    fill = "transparent",
    colour = NA),
  plot.background = element_rect(
    fill = "transparent",
    colour = NA),
  panel.grid = element_blank(),
  plot.title = element_text(
    colour = "white",
    size = 16,
    face = "bold",
    hjust = 0.5),
  axis.text = element_text(
    colour = "white",
    size = 12),
  axis.title = element_text(
    colour = "white",
    size = 14),
  axis.line = element_line(
    colour = "white"),
  axis.ticks = element_line(
    colour = "white"),
  legend.position = "none"
)


# Plant degree
plant_degree_slide = plant_degree_boxplot +
  labs(
    x = NULL,
    y = "Mean degree",
    title = "Plants") +
  slide_theme


# Pollinator degree
pollinator_degree_slide = pollinator_degree_boxplot +
  labs(
    x = NULL,
    y = NULL,
    title = "Pollinators") +
  slide_theme


# Combine plots
degree_combined_slide =
  plant_degree_slide +
  pollinator_degree_slide &
  theme(
    plot.background = element_rect(
      fill = "transparent",
      colour = NA))


degree_combined_slide


# Save with transparent background
ggsave(
  "degree_treatment_combined.png",
  plot = degree_combined_slide,
  width = 10,
  height = 5,
  units = "in",
  dpi = 600,
  bg = NA)