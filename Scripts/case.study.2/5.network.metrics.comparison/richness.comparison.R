############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 4. Comparison of species richness between treatments
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)    # Read CSV files
library(dplyr)    # Data manipulation
library(ggplot2)  # Data visualisation


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")


# ==========================================================
# 3. Calculate species richness
# ==========================================================

# Pool all months and calculate total plant and pollinator
# richness for each site
site_richness = networks_long %>%
  group_by(
    site,
    treatment) %>%
  summarise(
    plant_richness = n_distinct(plant_id),
    pollinator_richness = n_distinct(pollinator_species),
    .groups = "drop")

site_richness


# ==========================================================
# 4. Compare plant richness between treatments
# ==========================================================

# Summarise plant richness by treatment
plant_richness_summary = site_richness %>%
  group_by(treatment) %>%
  summarise(
    mean_richness = mean(plant_richness),
    sd_richness = sd(plant_richness),
    .groups = "drop")

plant_richness_summary


# Plot plant richness
plot_plant_richness = ggplot(
  site_richness,
  aes(
    x = treatment,
    y = plant_richness)) +
  geom_boxplot(
    aes(fill = treatment),
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.4) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.08,
    size = 4,
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
    y = "Plant richness") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_plant_richness


# Test differences in plant richness
plant_richness_model = lm(
  plant_richness ~ treatment,
  data = site_richness)

summary(plant_richness_model)
anova(plant_richness_model)


# ==========================================================
# 5. Compare pollinator richness between treatments
# ==========================================================

# Summarise pollinator richness by treatment
pollinator_richness_summary = site_richness %>%
  group_by(treatment) %>%
  summarise(
    mean_richness = mean(pollinator_richness),
    sd_richness = sd(pollinator_richness),
    .groups = "drop")

pollinator_richness_summary


# Plot pollinator richness
plot_pollinator_richness = ggplot(
  site_richness,
  aes(
    x = treatment,
    y = pollinator_richness)) +
  geom_boxplot(
    aes(fill = treatment),
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.4) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.08,
    size = 4,
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
    y = "Pollinator richness") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_pollinator_richness


# Test differences in pollinator richness
pollinator_richness_model = lm(
  pollinator_richness ~ treatment,
  data = site_richness)

summary(pollinator_richness_model)
anova(pollinator_richness_model)
