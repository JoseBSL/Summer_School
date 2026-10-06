# ==========================================================
# SPECIES COLOURS
# ==========================================================

# Number of habitats associated with each species
species_degree = rowSums(
  species_habitat_matrix_binary > 0)

# Colour according to number of habitat associations
species_colours = ifelse(
  species_degree == 1,
  "#E76F51",   # One habitat
  "#B7B5DD")   # More than one habitat

names(species_colours) = rownames(
  species_habitat_matrix_binary)


# ==========================================================
# SAVE NETWORK AS TRANSPARENT PNG
# ==========================================================

png(
  "species_habitat_network.png",
  width = 2400,
  height = 1600,
  res = 600,
  bg = "transparent")

# Label colour
par(col = "#B7B5DD")

plotweb(
  species_habitat_matrix_binary,
  sorting = "normal",
  
  # Species coloured by number of habitat associations
  lower_color = species_colours,
  lower_border = "same",
  
  # Habitats remain white
  higher_color = "white",
  higher_border = "white",
  
  # Links inherit the colour of the species
  link_color = "lower",
  link_border = "same",
  link_alpha = 0.85,
  
  # Labels
  text_size = 0.5,
  srt = 0,
  lab_distance = 0.01,
  
  # Margins
  mar = c(1, 1, 1, 1))

dev.off()