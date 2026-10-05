############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 5. Comparison of normalised degree between treatments
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)    # Read CSV files
library(dplyr)    # Data manipulation
library(tidyr)    # Data reshaping
library(tibble)   # Work with row names
library(ggplot2)  # Data visualisation
library(lmerTest)


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")


# ==========================================================
# 3. Calculate normalised degree
# ==========================================================

# Pool all months and create one network for each site.
# Normalised degree (ND) is the proportion of potential
# partners with which a species interacts.

site_degree = networks_long %>%
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
    
    # Plant normalised degree
    plants = tibble(
      species = rownames(web),
      ND = rowSums(web > 0) / ncol(web),
      guild = "Plant")
    
    # Pollinator normalised degree
    pollinators = tibble(
      species = colnames(web),
      ND = colSums(web > 0) / nrow(web),
      guild = "Pollinator")
    
    bind_rows(
      plants,
      pollinators)
  }) %>%
  ungroup()

site_degree



# ==========================================================
# 4. Compare plant normalised degree between treatments
# ==========================================================

plant_degree = site_degree %>%
  filter(guild == "Plant")


# Plot plant normalised degree
plot_plant_degree = ggplot(
  plant_degree,
  aes(
    x = treatment,
    y = ND)) +
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
    y = "Plant normalised degree (ND)") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_plant_degree


# Test differences in plant normalised degree
#
# Site is included as a random effect because multiple species
# are observed within each site.
#
# Species is also included as a random effect because the same
# species can occur in more than one site.
plant_degree_model = lmer(
  ND ~ treatment +
    (1 | site) +
    (1 | species),
  data = plant_degree)

summary(plant_degree_model)
anova(plant_degree_model)


# ==========================================================
# 5. Compare pollinator normalised degree between treatments
# ==========================================================

pollinator_degree = site_degree %>%
  filter(guild == "Pollinator")


# Plot pollinator normalised degree
plot_pollinator_degree = ggplot(
  pollinator_degree,
  aes(
    x = treatment,
    y = ND)) +
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
    y = "Pollinator normalised degree (ND)") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_pollinator_degree


# Test differences in pollinator normalised degree
pollinator_degree_model = lmer(
  ND ~ treatment +
    (1 | site) +
    (1 | species),
  data = pollinator_degree)

summary(pollinator_degree_model)
anova(pollinator_degree_model)
