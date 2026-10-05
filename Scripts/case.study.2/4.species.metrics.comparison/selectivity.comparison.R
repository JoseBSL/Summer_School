############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 6. Comparison of selectivity between treatments
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
# 3. Calculate selectivity (d')
# ==========================================================

# Pool all months and create one network for each site.
#
# d' measures species-level selectivity:
# 0 = low selectivity
# 1 = high selectivity

site_selectivity = networks_long %>%
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
    
    # Calculate d' for plants and pollinators
    metrics = specieslevel(
      web,
      index = "d",
      level = "both")
    
    # Plants
    plants = metrics[["lower level"]] %>%
      as.data.frame() %>%
      rownames_to_column("species") %>%
      as_tibble() %>%
      transmute(
        species,
        d = d,
        guild = "Plant")
    
    # Pollinators
    pollinators = metrics[["higher level"]] %>%
      as.data.frame() %>%
      rownames_to_column("species") %>%
      as_tibble() %>%
      transmute(
        species,
        d = d,
        guild = "Pollinator")
    
    bind_rows(
      plants,
      pollinators)
  }) %>%
  ungroup()

site_selectivity


# ==========================================================
# 4. Compare plant selectivity between treatments
# ==========================================================

plant_selectivity = site_selectivity %>%
  filter(guild == "Plant")


# Summarise plant selectivity by treatment
plant_selectivity_summary = plant_selectivity %>%
  group_by(treatment) %>%
  summarise(
    mean_d = mean(d, na.rm = TRUE),
    sd_d = sd(d, na.rm = TRUE),
    .groups = "drop")

plant_selectivity_summary


# Plot plant selectivity
plot_plant_selectivity = ggplot(
  plant_selectivity,
  aes(
    x = treatment,
    y = d)) +
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
    y = "Plant selectivity (d')") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_plant_selectivity


# Test differences in plant selectivity
#
# Site accounts for species belonging to the same network.
# Species accounts for the same species occurring at
# multiple sites.

plant_selectivity_model = lmer(
  d ~ treatment +
    (1 | site) +
    (1 | species),
  data = plant_selectivity)

summary(plant_selectivity_model)
anova(plant_selectivity_model)


# ==========================================================
# 5. Compare pollinator selectivity between treatments
# ==========================================================

pollinator_selectivity = site_selectivity %>%
  filter(guild == "Pollinator")


# Summarise pollinator selectivity by treatment
pollinator_selectivity_summary = pollinator_selectivity %>%
  group_by(treatment) %>%
  summarise(
    mean_d = mean(d, na.rm = TRUE),
    sd_d = sd(d, na.rm = TRUE),
    .groups = "drop")

pollinator_selectivity_summary


# Plot pollinator selectivity
plot_pollinator_selectivity = ggplot(
  pollinator_selectivity,
  aes(
    x = treatment,
    y = d)) +
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
    y = "Pollinator selectivity (d')") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_pollinator_selectivity


# Test differences in pollinator selectivity
pollinator_selectivity_model = lmer(
  d ~ treatment +
    (1 | site) +
    (1 | species),
  data = pollinator_selectivity)

summary(pollinator_selectivity_model)
anova(pollinator_selectivity_model)
