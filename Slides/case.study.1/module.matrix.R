# ==========================================================
# 6. Modularity
# ==========================================================

# Identify groups of species and habitats that interact more
# strongly with each other than with the rest of the network

modules = computeModules(
  species_habitat_matrix)


# Modularity score
modularity_Q = modules@likelihood

modularity_Q


# ==========================================================
# Extract module information
# ==========================================================

module_info = listModuleInformation(
  modules)[[2]]


# ==========================================================
# Species module membership
# ==========================================================

species_modules = bind_rows(
  lapply(
    seq_along(module_info),
    function(i) {
      
      tibble(
        Species = module_info[[i]][[1]],
        Module = paste0("Module ", i)
      )
    }
  )
)


# ==========================================================
# Habitat module membership
# ==========================================================

habitat_modules = bind_rows(
  lapply(
    seq_along(module_info),
    function(i) {
      
      tibble(
        Habitat = module_info[[i]][[2]],
        Module = paste0("Module ", i)
      )
    }
  )
)


species_modules

habitat_modules


# ==========================================================
# Convert interaction matrix to long format
# ==========================================================

module_long = species_habitat_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Species") %>%
  pivot_longer(
    cols = -Species,
    names_to = "Habitat",
    values_to = "Affinity"
  )


# ==========================================================
# Add module membership
# ==========================================================

module_long = module_long %>%
  left_join(
    species_modules %>%
      rename(
        species_module = Module
      ),
    by = "Species"
  ) %>%
  left_join(
    habitat_modules %>%
      rename(
        habitat_module = Module
      ),
    by = "Habitat"
  )


# ==========================================================
# Classify interactions
# ==========================================================

module_long = module_long %>%
  mutate(
    interaction_module = case_when(
      
      Affinity == 0 ~
        "No interaction",
      
      species_module == habitat_module ~
        species_module,
      
      TRUE ~
        "Between modules"
    )
  )


# ==========================================================
# Order species and habitats by module
# ==========================================================

species_order = species_modules %>%
  arrange(Module) %>%
  pull(Species)


habitat_order = habitat_modules %>%
  arrange(Module) %>%
  pull(Habitat)


module_long = module_long %>%
  mutate(
    Species = factor(
      Species,
      levels = rev(species_order)
    ),
    Habitat = factor(
      Habitat,
      levels = habitat_order
    )
  )


# ==========================================================
# Module colours
# ==========================================================

module_colours = c(
  "Module 1" = "#8BCF5B",
  "Module 2" = "#B7B5DD",
  "Module 3" = "#E69F5B",
  "Module 4" = "#5DA5DA",
  "Between modules" = "#777777",
  "No interaction" = "#343434"
)


# ==========================================================
# Plot modular structure
# ==========================================================

modularity_plot = ggplot(
  module_long,
  aes(
    x = Habitat,
    y = Species,
    fill = interaction_module
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = module_colours,
    name = NULL,
    breaks = c(
      "Module 1",
      "Module 2",
      "Module 3",
      "Module 4",
      "Between modules"
    )
  ) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(
      colour = "white",
      size = 11,
      angle = 45,
      hjust = 1,
      margin = margin(t = -25)
    ),
    axis.text.y = element_text(
      colour = "white",
      size = 10
    ),
    legend.position = "right",
    legend.text = element_text(
      colour = "white",
      size = 11
    ),
    legend.key = element_rect(
      fill = "transparent",
      colour = NA
    ),
    plot.margin = margin(
      35, 35, 35, 35
    )
  )

modularity_plot


# ==========================================================
# Save as transparent PNG
# ==========================================================

ggsave(
  "modularity_matrix.png",
  plot = modularity_plot,
  width = 7,
  height = 6,
  dpi = 600,
  bg = "transparent")