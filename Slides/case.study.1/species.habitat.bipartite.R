# ==========================================================
# Fix plotweb text-colour bug
# ==========================================================

plotweb_fixed = bipartite::plotweb

plotweb_code = paste(
  deparse(plotweb_fixed),
  collapse = "\n")

# Add colour to lower-level rotated labels
plotweb_code = sub(
  "font = font, family = family\\)\\n            text\\(c_tx",
  "font = font, family = family, col = lower_text_color)\\n            text(c_tx",
  plotweb_code)

# Add colour to higher-level rotated labels
plotweb_code = sub(
  "font = font, family = family\\)\\n        \\}\\n    \\}\\n    if \\(!is.null\\(add_lower_abundances\\)",
  "font = font, family = family, col = higher_text_color)\\n        }\\n    }\\n    if (!is.null(add_lower_abundances)",
  plotweb_code)



# ==========================================================
# Save network as transparent PNG
# ==========================================================

# Save network as transparent PNG
png(
  "species_habitat_network.png",
  width = 2400,
  height = 1600,
  res = 600,
  bg = "transparent")

# Set default text colour
par(col = "#B7B5DD")

plotweb(
  species_habitat_matrix_binary,
  lower_color = "white",
  higher_color = "white",
  lower_border = "white",
  higher_border = "white",
  link_color = "#B7B5DD",
  link_border = "#B7B5DD",
  link_alpha = 1,
  text_size = 0.5,
  srt = 0,
  lab_distance = 0.01,
  mar = c(1, 1, 1, 1))

dev.off()
