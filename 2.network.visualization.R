############################################################
# Summer School: Network Analysis in R
# Part 2: Visualise and explore data
############################################################

# ==========================================================
# 1. Load libraries
# ==========================================================
library(readr)  # Read CSV files
library(dplyr)  # Data manipulation
library(tidyr)  # Data reshaping
library(bipartite)  # Network analysis and visualization
library(ggplot2) # Data visualization
library(viridis) # Color scales for ggplot2
# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2("data/processed/visitation.networks.long.csv")

# ==========================================================
# 2. Prepare example network
# ==========================================================
# Check the available network IDs
unique(networks_long$`Network ID`)

# Select one network as an example
example_network = networks_long %>%
  filter(`Network ID` == unique(`Network ID`)[1])

# Convert the long-format data into an interaction matrix
example_matrix = example_network %>%
  select(
    `Plant species ID`,
    `Pollinator species`,
    `Number of visits`) %>%
  pivot_wider(
    names_from = `Pollinator species`,
    values_from = `Number of visits`,
    values_fill = 0) %>% 
  tibble::column_to_rownames("Plant species ID") %>%
  as.matrix()

# ==========================================================
# 2. Plot example network as a bipartite graph
#
# a) Unweighted network using bipartite
# b) Weighted network using bipartite
# c) Weighted network using ggplot2
# ==========================================================
graphics.off()

# Create an unweighted interaction matrix
example_matrix_binary <- example_matrix
example_matrix_binary[example_matrix_binary > 0] <- 1

# Unweighted network
plotweb(
  example_matrix_binary,
  lower_color = "forestgreen",
  higher_color = "steelblue",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 1,
  text_size = "auto",
  srt = 90,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1))

# Weighted network
plotweb(
  example_matrix,
  lower_color = "forestgreen",
  higher_color = "steelblue",
  link_color = "grey70",
  link_border = "grey70",
  link_alpha = 1,
  text_size = "auto",
  srt = 90,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1))

library(ggplot2)

ggplot(example_network,
       aes(x = `Pollinator species`,
           y = `Plant species ID`,
           fill = `Number of visits`)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_blank()
  )


# ==========================================================
# 3. Plot the network as an interaction matrix
# ==========================================================

# Complete the interaction matrix
example_network_complete <- example_network %>%
  complete(
    `Plant species ID`,
    `Pollinator species`,
    fill = list(`Number of visits` = 0)
  ) %>%
  mutate(
    `Number of visits` = na_if(`Number of visits`, 0))

# Plot the interaction matrix
ggplot(
  example_network_complete,
  aes(
    x = `Pollinator species`,
    y = `Plant species ID`,
    fill = `Number of visits`
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
# c) Weighted bipartite network using ggplot2
# ==========================================================

# Order plants by their total number of visits
plant_nodes <- example_network %>%
  group_by(`Plant species ID`) %>%
  summarise(
    total_visits = sum(`Number of visits`),
    .groups = "drop"
  ) %>%
  arrange(desc(total_visits)) %>%
  mutate(
    x_plant = seq_along(`Plant species ID`),
    y_plant = 0
  )

# Order pollinators by their total number of visits
pollinator_nodes <- example_network %>%
  group_by(`Pollinator species`) %>%
  summarise(
    total_visits = sum(`Number of visits`),
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
network_edges <- example_network %>%
  select(
    `Plant species ID`,
    `Pollinator species`,
    `Number of visits`
  ) %>%
  left_join(
    plant_nodes %>%
      select(`Plant species ID`, x_plant, y_plant),
    by = "Plant species ID"
  ) %>%
  left_join(
    pollinator_nodes %>%
      select(`Pollinator species`, x_pollinator, y_pollinator),
    by = "Pollinator species"
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
      linewidth = `Number of visits`
    ),
    colour = "grey60",
    alpha = 0.7
  ) +
  geom_point(
    data = plant_nodes,
    aes(x = x_plant, y = y_plant, size= total_visits),
    colour = "forestgreen",
    show.legend = FALSE
  ) +
  geom_point(
    data = pollinator_nodes,
    aes(x = x_pollinator, y = y_pollinator, size= total_visits),
    colour = "steelblue",
    show.legend = FALSE
  ) +
  geom_text(
    data = plant_nodes,
    aes(
      x = x_plant,
      y = y_plant,
      label = `Plant species ID`
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
      label = `Pollinator species`
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


