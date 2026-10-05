############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# Comparison of network connectance between treatments
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
library(nlme)       # GLS models and temporal autocorrelation


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")


# ==========================================================
# 3. Calculate network connectance
# ==========================================================

# Calculate connectance separately for each monthly network.
#
# Connectance is the proportion of all possible interactions
# that are observed in the network.
#
# 0 = no possible interactions are realised
# 1 = all possible interactions are realised

network_connectance = networks_long %>%
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
    
    # Calculate connectance
    connectance = networklevel(
      web,
      index = "connectance")
    
    tibble(
      connectance = as.numeric(connectance))
  }) %>%
  ungroup()

network_connectance


# ==========================================================
# 4. Summarise network connectance
# ==========================================================

connectance_summary = network_connectance %>%
  group_by(treatment) %>%
  summarise(
    mean_connectance = mean(connectance),
    sd_connectance = sd(connectance),
    .groups = "drop")

connectance_summary


# ==========================================================
# 5. Compare network connectance between treatments
# ==========================================================

plot_connectance = ggplot(
  network_connectance,
  aes(
    x = treatment,
    y = connectance)) +
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
    y = "Connectance") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_connectance


# ==========================================================
# 6. Explore temporal variation
# ==========================================================

plot_connectance_time = ggplot(
  network_connectance,
  aes(
    x = factor(month),
    y = connectance,
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
    y = "Connectance",
    fill = "Treatment",
    colour = "Treatment") +
  theme_bw()

plot_connectance_time


# ==========================================================
# 7. Test differences in network connectance
# ==========================================================

# Networks from the same site were sampled repeatedly through
# time. We account for temporal autocorrelation using a
# first-order autoregressive correlation structure, AR(1).

connectance_model = gls(
  connectance ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = network_connectance,
  method = "REML")

summary(connectance_model)
anova(connectance_model)
