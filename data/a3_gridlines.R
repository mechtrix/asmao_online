library(ggplot2)
library(grid)

# A3-Maße in mm (420 × 297 mm)
a3_width <- 420
a3_height <- 297

a3_height <- 420
a3_width <- 297


# Linienparameter
line_spacing <- 100  # Abstand in mm
line_color <- "black"
line_width <- 0.5   # Linienbreite in mm

# Berechne Positionen der Linien (von oben nach unten)
line_positions <- seq(0, a3_height, by = line_spacing)
line_positions <- line_positions[line_positions <= a3_height]  # Nur innerhalb der Seite

# Erstelle leeren Plot mit A3-Größe
p <- ggplot() +
  coord_fixed(xlim = c(0, a3_width), ylim = c(0, a3_height)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Füge horizontale Linien hinzu
for (y in line_positions) {
  p <- p + geom_hline(yintercept = y, color = line_color, linewidth = line_width)
}

# Speichere als PDF (A3-Größe)
ggsave(
  filename = "data/linienraster_a3.pdf",
  plot = p,
  width = a3_width / 25.4,  # Umrechnung mm → Zoll (1 Zoll = 25.4 mm)
  height = a3_height / 25.4,
  units = "in",
  dpi = 300
)

print(paste("PDF mit", length(line_positions), "Linien gespeichert unter: linienraster_a3.pdf"))
