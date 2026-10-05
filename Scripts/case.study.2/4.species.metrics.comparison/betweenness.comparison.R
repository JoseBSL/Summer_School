############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 7. Comparison of betweenness centrality between treatments
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)      # Read CSV files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(tibble)     # Work with row names
library(ggplot2)    # Data visualisation
library(bipartite)  # Network descriptors
library(lmerTest)   # Linear mixed-effects models


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")


# ==========================================================
# 3. Calculate betweenness centrality
# ==========================================================

# Pool all months and create one network for each site.
#
# Betweenness centrality measures how often a species lies
# on the shortest paths connecting other species.
# Higher values indicate a more central connecting role.

site_betweenness = networks_long %>%
  group_by(
    site,
    treatment) %>%
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
    
    # Calculate betweenness for plants and pollinators
    metrics = specieslevel(
      web,
      index = "betweenness",
      level = "both")
    
    # Plants
    plants = metrics[["lower level"]] %>%
      as.data.frame() %>%
      rownames_to_column("species") %>%
      as_tibble() %>%
      transmute(
        species,
        betweenness = betweenness,
        guild = "Plant")
    
    # Pollinators
    pollinators = metrics[["higher level"]] %>%
      as.data.frame() %>%
      rownames_to_column("species") %>%
      as_tibble() %>%
      transmute(
        species,
        betweenness = betweenness,
        guild = "Pollinator")
    
    bind_rows(
      plants,
      pollinators)
  }) %>%
  ungroup()

site_betweenness


# ==========================================================
# 4. Compare plant betweenness between treatments
# ==========================================================

plant_betweenness = site_betweenness %>%
  filter(guild == "Plant")


# Summarise plant betweenness by treatment
plant_betweenness_summary = plant_betweenness %>%
  group_by(treatment) %>%
  summarise(
    mean_betweenness = mean(
      betweenness,
      na.rm = TRUE),
    sd_betweenness = sd(
      betweenness,
      na.rm = TRUE),
    .groups = "drop")

plant_betweenness_summary


# Plot plant betweenness
plot_plant_betweenness = ggplot(
  plant_betweenness,
  aes(
    x = treatment,
    y = betweenness)) +
  geom_boxplot(
    aes(fill = treatment),
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.4) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.08,
    size = 3,
    alpha = 0.4) +
  scale_fill_manual(
    values = c(
      "Restored" = "#3A923A",
      "Unrestored" = "#595959")) +
  scale_colour_manual(
    values = c(
      "Restored" = "#3A923A",
      "Unrestored" = "#595959")) +
  labs(
    x = "Treatment",
    y = "Plant betweenness centrality") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_plant_betweenness


# Test differences in plant betweenness
#
# Site accounts for species belonging to the same network.
# Species accounts for the same species occurring at
# multiple sites.

plant_betweenness_model = lmer(
  betweenness ~ treatment +
    (1 | site) +
    (1 | species),
  data = plant_betweenness)

summary(plant_betweenness_model)
anova(plant_betweenness_model)


# ==========================================================
# 5. Compare pollinator betweenness between treatments
# ==========================================================

pollinator_betweenness = site_betweenness %>%
  filter(guild == "Pollinator")


# Summarise pollinator betweenness by treatment
pollinator_betweenness_summary = pollinator_betweenness %>%
  group_by(treatment) %>%
  summarise(
    mean_betweenness = mean(
      betweenness,
      na.rm = TRUE),
    sd_betweenness = sd(
      betweenness,
      na.rm = TRUE),
    .groups = "drop")

pollinator_betweenness_summary


# Plot pollinator betweenness
plot_pollinator_betweenness = ggplot(
  pollinator_betweenness,
  aes(
    x = treatment,
    y = betweenness)) +
  geom_boxplot(
    aes(fill = treatment),
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.4) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.08,
    size = 3,
    alpha = 0.4) +
  scale_fill_manual(
    values = c(
      "Restored" = "#3A923A",
      "Unrestored" = "#595959")) +
  scale_colour_manual(
    values = c(
      "Restored" = "#3A923A",
      "Unrestored" = "#595959")) +
  labs(
    x = "Treatment",
    y = "Pollinator betweenness centrality") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_pollinator_betweenness


# Test differences in pollinator betweenness
pollinator_betweenness_model = lmer(
  betweenness ~ treatment +
    (1 | site) +
    (1 | species),
  data = pollinator_betweenness)

summary(pollinator_betweenness_model)
anova(pollinator_betweenness_model)

