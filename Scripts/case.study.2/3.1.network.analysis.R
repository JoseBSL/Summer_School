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
# 4. Degree
# ==========================================================

# Plant degree = number of pollinator partners
plant_degree = rowSums(
  example_matrix > 0
)

plant_degree


# Plot plant degree
plant_degree %>%
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
    fill = "forestgreen"
  ) +
  coord_flip() +
  labs(
    x = "Plant species",
    y = "Degree"
  ) +
  theme_bw()


# Pollinator degree = number of plant partners
pollinator_degree = colSums(
  example_matrix > 0
)

pollinator_degree


# Plot pollinator degree
pollinator_degree %>%
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
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Pollinator species",
    y = "Degree"
  ) +
  theme_bw()


# Calculate degree using bipartite
node_degree = specieslevel(
  example_matrix,
  index = "degree",
  level = "both"
)

node_degree


# ==========================================================
# 5. Connectance
# ==========================================================

# Connectance is the proportion of realised interactions
# out of all possible interactions:
#
# C = L / (P * A)
#
# where:
# L = number of realised links
# P = number of plant species
# A = number of animal species


# Number of realised links
L = sum(
  example_matrix > 0
)

L


# Number of plant species
P = nrow(
  example_matrix
)

P


# Number of pollinator species
A = ncol(
  example_matrix
)

A


# Number of possible links
possible_links = P * A

possible_links


# Calculate connectance manually
connectance_manual = L / possible_links

connectance_manual


# Calculate connectance using bipartite
network_connectance = networklevel(
  example_matrix,
  index = "connectance"
)

network_connectance


# ==========================================================
# 6. Species-level descriptors
# ==========================================================

# Calculate degree, selectivity and betweenness centrality
# for plants and pollinators

node_metrics = specieslevel(
  example_matrix,
  index = c(
    "degree",
    "d",
    "betweenness"
  ),
  level = "both"
)

node_metrics


# Plant descriptors
plant_metrics = node_metrics[["lower level"]] %>%
  rownames_to_column("Species") %>%
  as_tibble() %>%
  arrange(desc(degree))

plant_metrics


# Pollinator descriptors
pollinator_metrics = node_metrics[["higher level"]] %>%
  rownames_to_column("Species") %>%
  as_tibble() %>%
  arrange(desc(degree))

pollinator_metrics


# ==========================================================
# 7. Selectivity
# ==========================================================

# d' ranges from 0 (low selectivity)
# to 1 (high selectivity)


# Plot plant selectivity
ggplot(
  plant_metrics,
  aes(
    x = reorder(Species, d),
    y = d
  )
) +
  geom_col(
    fill = "forestgreen"
  ) +
  coord_flip() +
  labs(
    x = "Plant species",
    y = "Selectivity (d')"
  ) +
  theme_bw()


# Plot pollinator selectivity
ggplot(
  pollinator_metrics,
  aes(
    x = reorder(Species, d),
    y = d
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Pollinator species",
    y = "Selectivity (d')"
  ) +
  theme_bw()


# ==========================================================
# 8. Betweenness centrality
# ==========================================================

# Plot plant betweenness
ggplot(
  plant_metrics,
  aes(
    x = reorder(Species, betweenness),
    y = betweenness
  )
) +
  geom_col(
    fill = "forestgreen"
  ) +
  coord_flip() +
  labs(
    x = "Plant species",
    y = "Betweenness centrality"
  ) +
  theme_bw()


# Plot pollinator betweenness
ggplot(
  pollinator_metrics,
  aes(
    x = reorder(Species, betweenness),
    y = betweenness
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Pollinator species",
    y = "Betweenness centrality"
  ) +
  theme_bw()


# ==========================================================
# 9. Nestedness
# ==========================================================

# Calculate nestedness
network_nestedness = networklevel(
  example_matrix,
  index = "nestedness"
)

network_nestedness


# ==========================================================
# 10. Modularity
# ==========================================================

# Identify groups of plants and pollinators that interact
# more strongly with each other than with the rest
# of the network

modules = computeModules(
  example_matrix
)


# Modularity score
modularity_Q = modules@likelihood

modularity_Q


# Visualise modules
plotModuleWeb(
  modules
)


# ==========================================================
# 11. Network roles
# ==========================================================

# Network roles are based on:
#
# z = within-module degree
# c = among-module connectivity
#
# Network hub: z > 2.5 and c > 0.62
# Module hub:  z > 2.5 and c <= 0.62
# Connector:   z <= 2.5 and c > 0.62
# Peripheral:  z <= 2.5 and c <= 0.62


# Function to classify network roles
classify_role = function(z, c) {
  
  case_when(
    z > 2.5 & c > 0.62 ~ "Network hub",
    z > 2.5 ~ "Module hub",
    c > 0.62 ~ "Connector",
    TRUE ~ "Peripheral"
  )
}


# Plant roles
plant_roles = czvalues(
  modules,
  weighted = TRUE,
  level = "lower"
) %>%
  {
    tibble(
      Species = names(.$c),
      c = .$c,
      z = replace_na(.$z, 0)
    )
  } %>%
  mutate(
    role = classify_role(z, c)
  ) %>%
  arrange(
    desc(z),
    desc(c)
  )

plant_roles


# Pollinator roles
pollinator_roles = czvalues(
  modules,
  weighted = TRUE,
  level = "higher"
) %>%
  {
    tibble(
      Species = names(.$c),
      c = .$c,
      z = replace_na(.$z, 0)
    )
  } %>%
  mutate(
    role = classify_role(z, c)
  ) %>%
  arrange(
    desc(z),
    desc(c)
  )

pollinator_roles

