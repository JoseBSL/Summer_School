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

treatment_colours = c(
  "Restored" = "#3A923A",
  "Unrestored" = "#595959"
)


# Plot plant richness
plot_plant_richness = ggplot(
  site_richness,
  aes(
    x = treatment,
    y = plant_richness)) +
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
    size = 4,
    alpha = 1) +
  scale_fill_manual(
    values = treatment_colours) +
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

# Plot pollinator richness
plot_pollinator_richness = ggplot(
  site_richness,
  aes(
    x = treatment,
    y = pollinator_richness)) +
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
    size = 4,
    alpha = 1) +
  scale_fill_manual(
    values = treatment_colours) +
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

