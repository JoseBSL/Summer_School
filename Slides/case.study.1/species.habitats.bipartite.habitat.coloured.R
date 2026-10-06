# ==========================================================
# HABITAT COLOURS
# ==========================================================

habitat_colours = c(
  "#8C88D8",  # Rocky bottoms
  "#66B89A",  # Coralligenous
  "#65B5D8",  # Posidonia oceanica
  "#E3A15A",  # Sandy bottoms
  "#C66B9A"   # Maerl
)

# Assign actual habitat names from the matrix
names(habitat_colours) = colnames(
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

# Default label colour
par(col = "#B7B5DD")

plotweb(
  species_habitat_matrix_binary,
  sorting = "normal",
  
  # Species
  lower_color = "white",
  lower_border = "white",
  
  # Habitats
  higher_color = habitat_colours,
  higher_border = "same",
  
  # Links inherit habitat colour
  link_color = "higher",
  link_border = "same",
  link_alpha = 0.85,
  
  # Labels
  text_size = 0.45,
  srt = 0,
  lab_distance = 0.01,
  
  # Margins
  mar = c(1, 1, 1, 1))

dev.off()
