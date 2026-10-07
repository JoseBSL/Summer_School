# ==========================================================
# 8. Compare normalised degree between treatments
# ==========================================================

degree_boxplot = ggplot(
  network_degree,
  aes(
    x = treatment,
    y = mean_ND)) +
  geom_boxplot(
    aes(fill = treatment),
    width = 0.5,
    colour = "white",
    outlier.shape = NA,
    alpha = 1) +
  geom_jitter(
    aes(fill = treatment),
    shape = 21,
    colour = "white",
    stroke = 0.7,
    width = 0.08,
    size = 3,
    alpha = 1) +
  scale_fill_manual(
    values = treatment_colours) +
  labs(
    x = "Treatment",
    y = "Mean normalised degree") +
  theme_bw() +
  theme(
    legend.position = "none")

degree_boxplot


# ==========================================================
# Save figure for slides
# ==========================================================

degree_boxplot_slide = degree_boxplot +
  theme(
    panel.background = element_rect(
      fill = "transparent",
      colour = NA),
    plot.background = element_rect(
      fill = "transparent",
      colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(
      colour = "white",
      size = 12),
    axis.title = element_text(
      colour = "white",
      size = 14),
    axis.line = element_line(
      colour = "white"),
    axis.ticks = element_line(
      colour = "white"))

ggsave(
  "mean_normalised_degree_treatment.png",
  plot = degree_boxplot_slide,
  width = 6,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "transparent")
