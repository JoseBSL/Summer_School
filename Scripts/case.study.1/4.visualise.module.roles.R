############################################################
# Summer School: Network Analysis in R
# Case Study 2: Species-habitat bipartite network
# Plot network modules using ggplot2
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(bipartite)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)


# ==========================================================
# 2. Load interaction matrix
# ==========================================================

species_habitat_matrix = readRDS(
  "data/Processed/species_habitat_matrix.rds"
)


# ==========================================================
# 3. Detect modules
# ==========================================================

modules = computeModules(
  species_habitat_matrix
)

# Modularity
modularity_Q = modules@likelihood

modularity_Q


# ==========================================================
# 4. Extract module information
# ==========================================================

module_info = listModuleInformation(modules)

# Extract detected modules
detected_modules = module_info[[2]]

# Number of modules
length(detected_modules)

# Show module composition
detected_modules


# ==========================================================
# 5. Create module tables
# ==========================================================

# Species and their assigned module
species_modules = lapply(
  seq_along(detected_modules),
  function(i) {
    tibble(
      Species = detected_modules[[i]][[1]],
      species_module = i
    )
  }
) %>%
  bind_rows()

species_modules


# Habitats and their assigned module
habitat_modules = lapply(
  seq_along(detected_modules),
  function(i) {
    tibble(
      Habitat = detected_modules[[i]][[2]],
      habitat_module = i
    )
  }
) %>%
  bind_rows()

habitat_modules


# ==========================================================
# 6. Create node positions
# ==========================================================

# Species
species_nodes = species_modules %>%
  arrange(species_module) %>%
  mutate(
    x_species = seq_along(Species),
    y_species = 0
  )


# Habitats
habitat_nodes = habitat_modules %>%
  arrange(habitat_module) %>%
  mutate(
    x_habitat = seq(
      1,
      nrow(species_nodes),
      length.out = n()
    ),
    y_habitat = 1
  )


# ==========================================================
# 7. Prepare links
# ==========================================================

# Convert the interaction matrix to long format
network_edges = species_habitat_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Species") %>%
  pivot_longer(
    cols = -Species,
    names_to = "Habitat",
    values_to = "Affinity"
  ) %>%
  filter(Affinity > 0) %>%
  left_join(
    species_nodes,
    by = "Species"
  ) %>%
  left_join(
    habitat_nodes,
    by = "Habitat"
  )


# ==========================================================
# 8. Plot modules
# ==========================================================

ggplot() +
  
  # Interactions
  geom_segment(
    data = network_edges,
    aes(
      x = x_species,
      y = y_species,
      xend = x_habitat,
      yend = y_habitat,
      linewidth = Affinity
    ),
    colour = "grey70",
    alpha = 0.7
  ) +
  
  # Species
  geom_point(
    data = species_nodes,
    aes(
      x = x_species,
      y = y_species,
      colour = factor(species_module)
    ),
    size = 4
  ) +
  
  # Habitats
  geom_point(
    data = habitat_nodes,
    aes(
      x = x_habitat,
      y = y_habitat,
      colour = factor(habitat_module)
    ),
    size = 6
  ) +
  
  # Species labels
  geom_text(
    data = species_nodes,
    aes(
      x = x_species,
      y = y_species,
      label = Species
    ),
    angle = 90,
    hjust = 1.15,
    size = 3
  ) +
  
  # Habitat labels
  geom_text(
    data = habitat_nodes,
    aes(
      x = x_habitat,
      y = y_habitat,
      label = Habitat
    ),
    angle = 90,
    hjust = -0.15,
    size = 3
  ) +
  
  # Interaction strength
  scale_linewidth_continuous(
    name = "Habitat affinity",
    range = c(0.3, 2.5)
  ) +
  
  labs(
    colour = "Module",
    x = NULL,
    y = NULL
  ) +
  
  coord_cartesian(
    ylim = c(-0.4, 1.4),
    clip = "off"
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "right",
    plot.margin = margin(80, 60, 80, 60)
  )