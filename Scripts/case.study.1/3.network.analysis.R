############################################################
# Summer School: Network Analysis in R
# Case Study 1: Plant-pollinator networks
# 3. Network analysis
############################################################


# ==========================================================
# 1. Load libraries
# ==========================================================

library(tidyverse)
library(bipartite)


# ==========================================================
# 2. Load data
# ==========================================================

networks_long = read_csv2(
  "Data/Processed/visitation.networks.long.csv"
)


# ==========================================================
# 3. Calculate number of visits
# ==========================================================

# Calculate the total number of visits in each network
visits = networks_long %>%
  group_by(
    site,
    month,
    treatment,
    network_id
  ) %>%
  summarise(
    visits = sum(visits),
    .groups = "drop"
  )

visits


# Summarise number of visits by treatment
visits_summary = visits %>%
  group_by(treatment) %>%
  summarise(
    mean_visits = mean(visits),
    sd_visits = sd(visits),
    .groups = "drop"
  )

visits_summary


# Compare number of visits between treatments
ggplot(
  visits,
  aes(
    x = treatment,
    y = visits,
    fill = treatment
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.1,
    size = 3.5,
    shape = 21,
    fill = "white",
    stroke = 1
  ) +
  scale_fill_manual(
    values = c(
      "Restored" = "#56B4E9",
      "Unrestored" = "#E69F00"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Restored" = "#0072B2",
      "Unrestored" = "#D55E00"
    )
  ) +
  labs(
    x = "Treatment",
    y = "Number of visits"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )


# ==========================================================
# 4. Compare number of visits between treatments
# ==========================================================

# For simplicity, we use a linear model here.
# In a full analysis, the assumptions of the model and the
# spatial and temporal structure of the data should be evaluated.

visits_model = lm(
  visits ~ treatment,
  data = visits
)

summary(visits_model)
anova(visits_model)


# ==========================================================
# 5. Calculate species-level descriptors
# ==========================================================

# Calculate degree, d' and betweenness for all species
# separately within each network

species_metrics = networks_long %>%
  group_by(
    site,
    month,
    treatment,
    network_id
  ) %>%
  group_modify(~ {
    
    # Create the interaction matrix
    web = .x %>%
      group_by(
        plant_id,
        pollinator_species
      ) %>%
      summarise(
        visits = sum(visits),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = pollinator_species,
        values_from = visits,
        values_fill = 0
      ) %>%
      column_to_rownames("plant_id") %>%
      as.matrix()
    
    # Calculate species-level descriptors
    metrics = specieslevel(
      web,
      index = c(
        "degree",
        "d",
        "betweenness"
      ),
      level = "both"
    )
    
    # Plant metrics
    plants = metrics[["lower level"]] %>%
      as.data.frame() %>%
      rownames_to_column("species") %>%
      as_tibble() %>%
      mutate(
        level = "Plant"
      )
    
    # Pollinator metrics
    pollinators = metrics[["higher level"]] %>%
      as.data.frame() %>%
      rownames_to_column("species") %>%
      as_tibble() %>%
      mutate(
        level = "Pollinator"
      )
    
    # Combine plants and pollinators
    bind_rows(
      plants,
      pollinators
    )
  }) %>%
  ungroup()

species_metrics


# ==========================================================
# 6. Compare degree between treatments
# ==========================================================

ggplot(
  species_metrics,
  aes(
    x = treatment,
    y = degree,
    fill = treatment
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.1,
    size = 2.5,
    shape = 21,
    fill = "white",
    stroke = 0.8,
    alpha = 0.7
  ) +
  facet_wrap(
    ~ level,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Restored" = "#56B4E9",
      "Unrestored" = "#E69F00"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Restored" = "#0072B2",
      "Unrestored" = "#D55E00"
    )
  ) +
  labs(
    x = "Treatment",
    y = "Degree"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )


# ==========================================================
# 7. Compare selectivity (d') between treatments
# ==========================================================

# d' ranges from 0 (low selectivity) to 1 (high selectivity)

ggplot(
  species_metrics,
  aes(
    x = treatment,
    y = d,
    fill = treatment
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.1,
    size = 2.5,
    shape = 21,
    fill = "white",
    stroke = 0.8,
    alpha = 0.7
  ) +
  facet_wrap(
    ~ level
  ) +
  scale_fill_manual(
    values = c(
      "Restored" = "#56B4E9",
      "Unrestored" = "#E69F00"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Restored" = "#0072B2",
      "Unrestored" = "#D55E00"
    )
  ) +
  labs(
    x = "Treatment",
    y = "Selectivity (d')"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )


# ==========================================================
# 8. Compare betweenness between treatments
# ==========================================================

ggplot(
  species_metrics,
  aes(
    x = treatment,
    y = betweenness,
    fill = treatment
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.1,
    size = 2.5,
    shape = 21,
    fill = "white",
    stroke = 0.8,
    alpha = 0.7
  ) +
  facet_wrap(
    ~ level,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Restored" = "#56B4E9",
      "Unrestored" = "#E69F00"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Restored" = "#0072B2",
      "Unrestored" = "#D55E00"
    )
  ) +
  labs(
    x = "Treatment",
    y = "Betweenness centrality"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )


# ==========================================================
# 9. Calculate network connectance
# ==========================================================

# Calculate one connectance value for each network

connectance = networks_long %>%
  group_by(
    site,
    month,
    treatment,
    network_id
  ) %>%
  group_modify(~ {
    
    # Create the interaction matrix
    web = .x %>%
      group_by(
        plant_id,
        pollinator_species
      ) %>%
      summarise(
        visits = sum(visits),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = pollinator_species,
        values_from = visits,
        values_fill = 0
      ) %>%
      column_to_rownames("plant_id") %>%
      as.matrix()
    
    # Calculate connectance
    tibble(
      connectance = networklevel(
        web,
        index = "connectance"
      )
    )
  }) %>%
  ungroup()

connectance


# ==========================================================
# 10. Compare connectance between treatments
# ==========================================================

ggplot(
  connectance,
  aes(
    x = treatment,
    y = connectance,
    fill = treatment
  )
) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    aes(colour = treatment),
    width = 0.1,
    size = 3.5,
    shape = 21,
    fill = "white",
    stroke = 1
  ) +
  scale_fill_manual(
    values = c(
      "Restored" = "#56B4E9",
      "Unrestored" = "#E69F00"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Restored" = "#0072B2",
      "Unrestored" = "#D55E00"
    )
  ) +
  labs(
    x = "Treatment",
    y = "Connectance"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )
