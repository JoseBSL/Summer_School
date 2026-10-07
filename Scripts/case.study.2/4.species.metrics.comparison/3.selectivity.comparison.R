############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 6. Comparison of selectivity between treatments
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(bipartite)
library(nlme)


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")


# ==========================================================
# 3. Calculate mean selectivity per network
# ==========================================================

# d' measures species-level selectivity:
# 0 = low selectivity
# 1 = high selectivity
#
# Calculate d' for each species within each monthly network
# and then calculate the mean d' for each guild.

network_selectivity = networks_long %>%
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
    
    # Calculate d' for plants and pollinators
    metrics = specieslevel(
      web,
      index = "d",
      level = "both")
    
    # Mean d' per network
    tibble(
      plant_d = mean(
        metrics[["lower level"]][, "d"],
        na.rm = TRUE),
      pollinator_d = mean(
        metrics[["higher level"]][, "d"],
        na.rm = TRUE))
  }) %>%
  ungroup()

network_selectivity


# ==========================================================
# 4. Compare plant selectivity between treatments
# ==========================================================

treatment_colours = c(
  "Restored" = "#3A923A",
  "Unrestored" = "#595959"
)


# Plot mean plant selectivity
plot_plant_selectivity = ggplot(
  network_selectivity,
  aes(
    x = treatment,
    y = plant_d)) +
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
    y = "Mean plant selectivity (d')") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_plant_selectivity


# Test differences in mean plant selectivity
plant_selectivity_model = gls(
  plant_d ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = network_selectivity,
  method = "REML")

summary(plant_selectivity_model)

anova(plant_selectivity_model)


# ==========================================================
# 5. Compare pollinator selectivity between treatments
# ==========================================================

# Plot mean pollinator selectivity
plot_pollinator_selectivity = ggplot(
  network_selectivity,
  aes(
    x = treatment,
    y = pollinator_d)) +
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
    y = "Mean pollinator selectivity (d')") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_pollinator_selectivity


# Test differences in mean pollinator selectivity
pollinator_selectivity_model = gls(
  pollinator_d ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = network_selectivity,
  method = "REML")

summary(pollinator_selectivity_model)

anova(pollinator_selectivity_model)
