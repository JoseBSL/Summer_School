############################################################
# Summer School: Network Analysis in R
# Case Study 2: Plant-pollinator networks
# 3. Comparison of visitation activity between treatments
############################################################

# ==========================================================
# 1. Load libraries
# ==========================================================

library(readr)    # Read CSV files
library(dplyr)    # Data manipulation
library(ggplot2)  # Data visualisation
library(nlme)     # GLS models and temporal autocorrelation

# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv")

# ==========================================================
# 3. Number of visits
# ==========================================================

# Calculate the total number of visits in each monthly network
visits = networks_long %>%
  group_by(
    site,
    month,
    treatment,
    network_id) %>%
  summarise(
    visits = sum(visits),
    .groups = "drop")

visits

# Summarise number of visits by treatment
visits_summary = visits %>%
  group_by(treatment) %>%
  summarise(
    mean_visits = mean(visits),
    sd_visits = sd(visits),
    .groups = "drop")

visits_summary

# Compare number of visits between treatments
plot_visits = ggplot(
  visits,
  aes(
    x = treatment,
    y = visits)) +
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
    y = "Number of visits") +
  theme_bw() +
  theme(
    legend.position = "none")

plot_visits

# ==========================================================
# 4. Explore temporal variation
# ==========================================================

# Plot monthly variation in visits between treatments
plot_visits_time = ggplot(
  visits,
  aes(
    x = factor(month),
    y = visits,
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
    y = "Number of visits",
    fill = "Treatment",
    colour = "Treatment") +
  theme_bw()

plot_visits_time

# ==========================================================
# 5. Test differences in number of visits
# ==========================================================

# Networks from the same site were sampled repeatedly through
# time. Observations close together in time may therefore have
# correlated residuals. We account for this temporal
# autocorrelation using a first-order autoregressive
# correlation structure, AR(1).

visits_model = gls(
  visits ~ treatment,
  correlation = corAR1(
    form = ~ month | site),
  data = visits,
  method = "REML")

summary(visits_model)
anova(visits_model)

