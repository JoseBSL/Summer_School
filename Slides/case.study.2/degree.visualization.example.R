############################################################
# Summer School: Network Analysis in R
# Case Study 1: Plant-pollinator networks
# 3. Network analysis
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)      # Read CSV files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(tibble)     # Data frames
library(bipartite)  # Network analysis
library(ggplot2)    # Data visualization


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
  filter(
    network_id == unique(network_id)[1]
  )


# Convert the long-format data into an interaction matrix
example_matrix = example_network %>%
  group_by(
    plant_id,
    pollinator_species
  ) %>%
  summarise(
    visits = sum(visits),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = pollinator_species,
    values_from = visits,
    values_fill = 0
  ) %>%
  column_to_rownames("plant_id") %>%
  as.matrix()

example_matrix



# ==========================================================
# 4. Degree in the example network
# ==========================================================

# Plant degree = number of pollinator partners
plant_degree = rowSums(
  example_matrix > 0)

plant_degree


# Plot plant degree
plant_degree_example_plot = plant_degree %>%
  enframe(
    name = "Plant",
    value = "Degree"
  ) %>%
  ggplot(
    aes(
      x = reorder(Plant, Degree),
      y = Degree
    )
  ) +
  geom_col(
    fill = "#8BCF5B"
  ) +
  coord_flip() +
  labs(
    x = "Plant species",
    y = "Degree"
  ) +
  theme_bw()

plant_degree_example_plot


# Pollinator degree = number of plant partners
pollinator_degree = colSums(
  example_matrix > 0)

pollinator_degree


# Plot pollinator degree
pollinator_degree_example_plot = pollinator_degree %>%
  enframe(
    name = "Pollinator",
    value = "Degree"
  ) %>%
  ggplot(
    aes(
      x = reorder(Pollinator, Degree),
      y = Degree
    )
  ) +
  geom_col(
    fill = "#B7B5DD"
  ) +
  coord_flip() +
  labs(
    x = "Pollinator species",
    y = "Degree"
  ) +
  theme_bw()

pollinator_degree_example_plot


# The same calculation using bipartite
node_degree = specieslevel(
  example_matrix,
  index = "degree",
  level = "both")

node_degree

# ==========================================================
# 7. Export figures
# ==========================================================

# Transparent theme for dark slides
transparent_theme = theme(
  panel.background = element_rect(
    fill = "transparent",
    colour = NA
  ),
  plot.background = element_rect(
    fill = "transparent",
    colour = NA
  ),
  panel.grid = element_blank(),
  axis.text = element_text(
    colour = "white",
    size = 12
  ),
  axis.title = element_text(
    colour = "white",
    size = 14
  ),
  axis.line = element_line(
    colour = "white"
  ),
  axis.ticks = element_line(
    colour = "white"
  ),
  legend.background = element_rect(
    fill = "transparent",
    colour = NA
  ),
  legend.key = element_rect(
    fill = "transparent",
    colour = NA
  ),
  legend.text = element_text(
    colour = "white",
    size = 12
  )
)


# Example plant degree
ggsave(
  "plant_degree_example.png",
  plot = plant_degree_example_plot + transparent_theme,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "transparent"
)


# Example pollinator degree
ggsave(
  "pollinator_degree_example.png",
  plot = pollinator_degree_example_plot + transparent_theme,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "transparent"
)


# Plant degree distribution
ggsave(
  "plant_degree_distribution.png",
  plot = plant_degree_plot + transparent_theme,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "transparent"
)


# Pollinator degree distribution
ggsave(
  "pollinator_degree_distribution.png",
  plot = pollinator_degree_plot + transparent_theme,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "transparent"
)
