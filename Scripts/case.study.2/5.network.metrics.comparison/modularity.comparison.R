############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# Comparison of network modularity between treatments
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)      # Read CSV files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(tibble)     # Work with row names
library(ggplot2)    # Data visualisation
library(bipartite)  # Network modularity
library(nlme)       # GLS models and temporal autocorrelation


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")


# ==========================================================
# 3. Calculate network modularity
# ==========================================================

# Calculate modularity separately for each monthly network.
#
# Higher modularity indicates that interactions are more
# strongly organised into distinct groups or modules.

network_modularity = networks_long %>%
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
    
    # Detect modules
    modules = computeModules(web)
    
    # Extract modularity
    modularity = slot(
      modules,
      "likelihood")
    
    tibble(
      modularity = modularity)
  }) %>%
  ungroup()

network_modularity


# ==========================================================
# 4. Summarise network modularity
# ==========================================================

modularity_summary = network_modularity %>%
  group_by(treatment) %>%
  summarise(
    mean_modularity = mean(modularity),
    sd_modularity = sd(modularity),
    .groups = "drop")

modularity_summary


# ==========================================================
# 5. Compare network modularity between treatments
# ==========================================================

plot_modularity = ggplot(
  network_modularity,
  aes(
    x = treatment,
    y = modularity)) +
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
    y = "Modularity") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_modularity


# ==========================================================
# 6. Explore temporal variation
# ==========================================================

plot_modularity_time = ggplot(
  network_modularity,
  aes(
    x = factor(month),
    y = modularity,
    fill = treatment)) +
  geom_boxplot(
    width = 0.7,
    outlier.shape = NA,
    alpha = 0.5,
    position = position_dodge(width = 0.8)) +
  geom_jitter(
    aes(colour = treatment),
    size = 2.5,
    alpha = 0.7,
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.8)) +
  scale_fill_manual(
    values = c(
      "Restored" = "#3A923A",
      "Unrestored" = "#595959")) +
  scale_colour_manual(
    values = c(
      "Restored" = "#3A923A",
      "Unrestored" = "#595959")) +
  labs(
    x = "Month",
    y = "Modularity",
    fill = "Treatment",
    colour = "Treatment") +
  theme_bw()

plot_modularity_time


# ==========================================================
# 7. Test differences in network modularity
# ==========================================================

# Networks from the same site were sampled repeatedly through
# time. We account for temporal autocorrelation using a
# first-order autoregressive correlation structure, AR(1).

modularity_model = gls(
  modularity ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = network_modularity,
  method = "REML")

summary(modularity_model)
anova(modularity_model)
