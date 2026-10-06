############################################################
# Summer School: Network Analysis in R
# Case Study 2: Species-habitat bipartite network
# 3. Network analysis
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(dplyr)
library(tidyr)
library(tibble)
library(bipartite)


# ==========================================================
# 2. Load interaction matrix
# ==========================================================

species_habitat_matrix = readRDS(
  "data/Processed/species_habitat_matrix.rds")


# ==========================================================
# 3. Degree
# ==========================================================

# Habitat degree = number of associated species
habitat_degree = colSums(species_habitat_matrix > 0)

habitat_degree
# ==========================================================
# Plot habitat degree
# ==========================================================

habitat_degree_plot = habitat_degree %>%
  enframe(
    name = "Habitat",
    value = "Degree"
  ) %>%
  ggplot(
    aes(
      x = reorder(Habitat, Degree),
      y = Degree
    )
  ) +
  geom_col(
    fill = "#8BCF5B",
    width = 0.7
  ) +
  coord_flip() +
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
  )+
  scale_y_continuous(
    breaks = scales::breaks_width(1)
  )

habitat_degree_plot


# ==========================================================
# Save as transparent PNG
# ==========================================================

ggsave(
  "habitat_degree.png",
  plot = habitat_degree_plot,
  width = 6,
  height = 4,
  dpi = 600,
  bg = "transparent")