############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# Comparison of network nestedness between treatments
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)      # Read CSV files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(tibble)     # Work with row names
library(ggplot2)    # Data visualisation
library(bipartite)  # Calculate NODF
library(maxnodf)    # Calculate corrected NODF (NODFc)
library(nlme)       # GLS models and temporal autocorrelation


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")


# ==========================================================
# 3. Calculate network nestedness
# ==========================================================

# Calculate nestedness separately for each monthly network.
#
# NODF measures observed nestedness.
# NODFc corrects nestedness to facilitate comparisons
# among networks with different structural properties.
# check https://doi.org/10.1111/1365-2656.12749
network_nestedness = networks_long %>%
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
    
    # Convert to binary interaction matrix
    web_binary = (web > 0) * 1
    
    # Calculate observed NODF
    NODF = networklevel(
      web_binary,
      index = "NODF")
    
    # Calculate corrected NODF
    NODFc_value = NODFc(
      web_binary,
      quality = 0)
    
    tibble(
      NODF = as.numeric(NODF),
      NODFc = as.numeric(NODFc_value))
  }) %>%
  ungroup()

network_nestedness


# ==========================================================
# 4. Summarise nestedness by treatment
# ==========================================================

nestedness_summary = network_nestedness %>%
  group_by(treatment) %>%
  summarise(
    mean_NODF = mean(NODF),
    sd_NODF = sd(NODF),
    mean_NODFc = mean(NODFc),
    sd_NODFc = sd(NODFc),
    .groups = "drop")

nestedness_summary


# ==========================================================
# 5. Compare observed NODF between treatments
# ==========================================================

plot_NODF = ggplot(
  network_nestedness,
  aes(
    x = treatment,
    y = NODF)) +
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
    y = "Nestedness (NODF)") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_NODF


# ==========================================================
# 6. Compare corrected NODF between treatments
# ==========================================================

plot_NODFc = ggplot(
  network_nestedness,
  aes(
    x = treatment,
    y = NODFc)) +
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
    y = "Corrected nestedness (NODFc)") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_NODFc


# ==========================================================
# 7. Explore temporal variation in NODF
# ==========================================================

plot_NODF_time = ggplot(
  network_nestedness,
  aes(
    x = factor(month),
    y = NODF,
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
    y = "Nestedness (NODF)",
    fill = "Treatment",
    colour = "Treatment") +
  theme_bw()

plot_NODF_time


# ==========================================================
# 8. Explore temporal variation in NODFc
# ==========================================================

plot_NODFc_time = ggplot(
  network_nestedness,
  aes(
    x = factor(month),
    y = NODFc,
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
    y = "Corrected nestedness (NODFc)",
    fill = "Treatment",
    colour = "Treatment") +
  theme_bw()

plot_NODFc_time


# ==========================================================
# 9. Test treatment differences in observed NODF
# ==========================================================

# Networks from the same site were sampled repeatedly through
# time. We account for temporal autocorrelation using AR(1).

NODF_model = gls(
  NODF ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = network_nestedness,
  method = "REML")

summary(NODF_model)
anova(NODF_model)


# ==========================================================
# 10. Test treatment differences in corrected NODF
# ==========================================================

NODFc_model = gls(
  NODFc ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = network_nestedness,
  method = "REML")

summary(NODFc_model)
anova(NODFc_model)

