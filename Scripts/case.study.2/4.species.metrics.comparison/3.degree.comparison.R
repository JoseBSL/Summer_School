############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 3. Network analysis
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(bipartite)
library(ggplot2)
library(nlme)


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")

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

# Select one monthly network as an example
example_network = networks_long %>%
  filter(
    network_id == unique(network_id)[1])

# Convert long-format data into an interaction matrix
example_matrix = example_network %>%
  group_by(
    plant_id,
    pollinator_species) %>%
  summarise(
    visits = sum(visits),
    .groups = "drop") %>%
  pivot_wider(
    names_from = pollinator_species,
    values_from = visits,
    values_fill = 0) %>%
  column_to_rownames("plant_id") %>%
  as.matrix()

example_matrix


# ==========================================================
# 4. Degree in the example network
# ==========================================================

# Degree = number of interaction partners

# Plant degree = number of pollinator partners
plant_degree = rowSums(
  example_matrix > 0)

plant_degree


# Plot plant degree
plant_degree_example_plot = plant_degree %>%
  enframe(
    name = "Plant",
    value = "Degree") %>%
  ggplot(
    aes(
      x = reorder(Plant, Degree),
      y = Degree)) +
  geom_col(
    fill = "forestgreen") +
  coord_flip() +
  labs(
    x = "Plant species",
    y = "Degree") +
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
    value = "Degree") %>%
  ggplot(
    aes(
      x = reorder(Pollinator, Degree),
      y = Degree)) +
  geom_col(
    fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Pollinator species",
    y = "Degree") +
  theme_bw()

pollinator_degree_example_plot


# The same calculation using bipartite
node_degree = specieslevel(
  example_matrix,
  index = "degree",
  level = "both")

node_degree


# ==========================================================
# 5. Degree across all 64 networks
# ==========================================================

# Calculate degree for every species
# in each monthly network

degree = networks_long %>%
  group_by(
    site,
    month,
    treatment,
    network_id) %>%
  group_modify(~ {
    
    # Create interaction matrix
    web = .x %>%
      group_by(
        plant_id,
        pollinator_species) %>%
      summarise(
        visits = sum(visits),
        .groups = "drop") %>%
      pivot_wider(
        names_from = pollinator_species,
        values_from = visits,
        values_fill = 0) %>%
      column_to_rownames("plant_id") %>%
      as.matrix()
    
    # Plant degree
    plant_degree = rowSums(
      web > 0)
    
    # Pollinator degree
    pollinator_degree = colSums(
      web > 0)
    
    # Combine plants and pollinators
    bind_rows(
      tibble(
        species = names(plant_degree),
        guild = "Plants",
        degree = as.numeric(plant_degree)),
      tibble(
        species = names(pollinator_degree),
        guild = "Pollinators",
        degree = as.numeric(pollinator_degree))
    )
  }) %>%
  ungroup()

degree


# ==========================================================
# 6. Visualise degree distributions
# ==========================================================

treatment_colours = c(
  "Restored" = "#3A923A",
  "Unrestored" = "#595959"
)


# ----------------------------------------------------------
# Plant degree
# ----------------------------------------------------------

plant_degree_plot = degree %>%
  filter(
    guild == "Plants") %>%
  ggplot(
    aes(
      x = degree,
      fill = treatment)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    colour = "white") +
  scale_x_continuous(
    breaks = scales::breaks_width(1)) +
  scale_fill_manual(
    values = treatment_colours) +
  labs(
    x = "Plant degree",
    y = "Frequency",
    fill = NULL) +
  theme_bw()

plant_degree_plot


# ----------------------------------------------------------
# Pollinator degree
# ----------------------------------------------------------

pollinator_degree_plot = degree %>%
  filter(
    guild == "Pollinators") %>%
  ggplot(
    aes(
      x = degree,
      fill = treatment)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    colour = "white") +
  scale_x_continuous(
    breaks = scales::breaks_width(1)) +
  scale_fill_manual(
    values = treatment_colours) +
  labs(
    x = "Pollinator degree",
    y = "Frequency",
    fill = NULL) +
  theme_bw()

pollinator_degree_plot


# ==========================================================
# 7. Mean degree across all 64 networks
# ==========================================================

# Calculate mean degree separately for plants and
# pollinators in each monthly network.

network_degree = degree %>%
  group_by(
    site,
    month,
    treatment,
    network_id,
    guild) %>%
  summarise(
    mean_degree = mean(degree),
    .groups = "drop")

network_degree


# ==========================================================
# 8. Compare mean plant degree between treatments
# ==========================================================

plant_network_degree = network_degree %>%
  filter(
    guild == "Plants")


# Plot mean plant degree
plant_degree_boxplot = ggplot(
  plant_network_degree,
  aes(
    x = treatment,
    y = mean_degree)) +
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
    y = "Mean plant degree") +
  theme_bw() +
  theme(
    legend.position = "none")

plant_degree_boxplot


# ----------------------------------------------------------
# Statistical analysis
# ----------------------------------------------------------

# Each observation represents one monthly network.
#
# Networks from the same site are sampled repeatedly
# through time, so we account for temporal autocorrelation
# using an AR(1) correlation structure.

plant_degree_model = gls(
  mean_degree ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = plant_network_degree,
  method = "REML")

summary(plant_degree_model)

anova(plant_degree_model)


# ==========================================================
# 9. Compare mean pollinator degree between treatments
# ==========================================================

pollinator_network_degree = network_degree %>%
  filter(
    guild == "Pollinators")


# Plot mean pollinator degree
pollinator_degree_boxplot = ggplot(
  pollinator_network_degree,
  aes(
    x = treatment,
    y = mean_degree)) +
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
    y = "Mean pollinator degree") +
  theme_bw() +
  theme(
    legend.position = "none")

pollinator_degree_boxplot


# ----------------------------------------------------------
# Statistical analysis
# ----------------------------------------------------------

pollinator_degree_model = gls(
  mean_degree ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = pollinator_network_degree,
  method = "REML")

summary(pollinator_degree_model)

anova(pollinator_degree_model)


