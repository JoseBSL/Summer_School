############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 6. Comparison of network specialization between treatments
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
# 3. Calculate network specialization (H2')
# ==========================================================

# Calculate H2' separately for each monthly network.
#
# H2' ranges from 0 (low specialization)
# to 1 (high specialization).

network_specialization = networks_long %>%
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
    
    # Calculate H2'
    H2 = networklevel(
      web,
      index = "H2")
    
    tibble(
      H2 = as.numeric(H2))
  }) %>%
  ungroup()

network_specialization


# ==========================================================
# 4. Summarise network specialization
# ==========================================================

# Summarise H2' by treatment
H2_summary = network_specialization %>%
  group_by(treatment) %>%
  summarise(
    mean_H2 = mean(H2),
    sd_H2 = sd(H2),
    .groups = "drop")

H2_summary


# ==========================================================
# 5. Compare network specialization between treatments
# ==========================================================

plot_H2 = ggplot(
  network_specialization,
  aes(
    x = treatment,
    y = H2)) +
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
    y = "Network specialization (H2')") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_H2


# ==========================================================
# 6. Explore temporal variation
# ==========================================================

# Compare monthly variation in H2' between treatments

plot_H2_time = ggplot(
  network_specialization,
  aes(
    x = factor(month),
    y = H2,
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
    y = "Network specialization (H2')",
    fill = "Treatment",
    colour = "Treatment") +
  theme_bw()

plot_H2_time


# ==========================================================
# 7. Test differences in network specialization
# ==========================================================

# Networks from the same site were sampled repeatedly through
# time. Observations close together in time may therefore have
# correlated residuals. We account for this temporal
# autocorrelation using a first-order autoregressive
# correlation structure, AR(1).

H2_model = gls(
  H2 ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = network_specialization,
  method = "REML")

summary(H2_model)
anova(H2_model)
