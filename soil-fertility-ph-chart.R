# Wissenschaftliche Grafik: pH-Einfluss auf Nährstoffverfügbarkeit, Metallmobilität und Bodenbiologie

library(ggplot2)
library(dplyr)
library(tidyr)

# pH-Bereich
ph <- seq(3, 9, by = 0.01)

# Hilfsfunktion: Gauß-Kurve (0–1 skaliert)
gauss <- function(x, mu, sigma, max = 1) {
  max * exp(-0.5 * ((x - mu) / sigma)^2)
}

# Daten generieren: relative Verfügbarkeit/Mobilität (0–1) nach pH
df <- data.frame(
  pH = ph,
  
  # Makronährstoffe (Maximum ~ pH 6–7)
  N  = gauss(ph, 6.8, 0.8),
  P  = gauss(ph, 6.5, 0.6),
  K  = gauss(ph, 6.5, 0.9),
  Ca = gauss(ph, 7.0, 0.8),
  Mg = gauss(ph, 7.0, 0.8),
  S  = gauss(ph, 6.5, 0.9),
  
  # Mikronährstoffe (stärker verfügbar bei niedrigerem pH)
  Fe = gauss(ph, 4.5, 0.7),
  Mn = gauss(ph, 4.5, 0.8),
  Zn = gauss(ph, 5.0, 0.8),
  Cu = gauss(ph, 5.0, 0.8),
  B  = gauss(ph, 5.5, 0.7),
  
  # Molybdän – stärker verfügbar bei hohem pH
  Mo = gauss(ph, 8.0, 0.7),
  
  # Toxisches Aluminium – stark mobil bei pH < 5
  Al = gauss(ph, 4.2, 0.4, max = 1),
  
  # Biologie – Maximum bei ~6.5
  Mikrobielle_Aktivitaet = gauss(ph, 6.5, 0.7)
)

# In langes Format überführen
long <- df %>%
  pivot_longer(
    cols      = -pH,
    names_to  = "Stoff",
    values_to = "Verfuegbarkeit"
  ) %>%
  mutate(
    Gruppe = case_when(
      Stoff %in% c("N", "P", "K", "Ca", "Mg", "S") ~ "Makronährstoffe",
      Stoff %in% c("Fe", "Mn", "Zn", "Cu", "B", "Mo") ~ "Mikronährstoffe",
      Stoff == "Al" ~ "Toxische Elemente",
      Stoff == "Mikrobielle_Aktivitaet" ~ "Biologie",
      TRUE ~ "Sonstige"
    ),
    Stoff = dplyr::recode(
      Stoff,
      N  = "Stickstoff (N)",
      P  = "Phosphor (P)",
      K  = "Kalium (K)",
      Ca = "Calcium (Ca)",
      Mg = "Magnesium (Mg)",
      S  = "Schwefel (S)",
      Fe = "Eisen (Fe)",
      Mn = "Mangan (Mn)",
      Zn = "Zink (Zn)",
      Cu = "Kupfer (Cu)",
      B  = "Bor (B)",
      Mo = "Molybdän (Mo)",
      Al = "Aluminium (Al)",
      Mikrobielle_Aktivitaet = "Mikrobielle Aktivität"
    )
  )

# Farbpalette (optional anpassbar)
stoff_levels <- c(
  "Stickstoff (N)", "Phosphor (P)", "Kalium (K)",
  "Calcium (Ca)", "Magnesium (Mg)", "Schwefel (S)",
  "Eisen (Fe)", "Mangan (Mn)", "Zink (Zn)",
  "Kupfer (Cu)", "Bor (B)", "Molybdän (Mo)",
  "Aluminium (Al)", "Mikrobielle Aktivität"
)

long$Stoff <- factor(long$Stoff, levels = stoff_levels)

farben <- c(
  "Stickstoff (N)"        = "#1b9e77",
  "Phosphor (P)"          = "#d95f02",
  "Kalium (K)"            = "#7570b3",
  "Calcium (Ca)"          = "#e7298a",
  "Magnesium (Mg)"        = "#66a61e",
  "Schwefel (S)"          = "#e6ab02",
  "Eisen (Fe)"            = "#a6761d",
  "Mangan (Mn)"           = "#666666",
  "Zink (Zn)"             = "#1f78b4",
  "Kupfer (Cu)"           = "#b2df8a",
  "Bor (B)"               = "#fb9a99",
  "Molybdän (Mo)"         = "#cab2d6",
  "Aluminium (Al)"        = "#e31a1c",
  "Mikrobielle Aktivität" = "#33a02c"
)

# Grafik erstellen – jede Größe in eigenem horizontalen Panel
# Grafik mit Überlagerung, gruppiert nach Makro/Mikro/… und besser lesbar

p <- ggplot(long, aes(x = pH, y = Verfuegbarkeit, group = Stoff)) +
  # halbtransparente, gefüllte Flächen
  geom_area(aes(fill = Stoff),
            alpha = 0.25,          # Transparenz, damit Überlagerung sichtbar bleibt
            colour = NA,
            position = "identity") +
  # klare Linien oben drauf
  geom_line(aes(colour = Stoff),
            size = 0.6) +
  scale_fill_manual(values = farben, guide = "none") +   # gleiche Farben wie zuvor
  scale_colour_manual(values = farben, name = "Größe / Prozess") +
  scale_x_continuous(
    name   = "pH-Wert des Bodens",
    limits = c(3, 9),
    breaks = 3:9
  ) +
  scale_y_continuous(
    name   = "Relative Verfügbarkeit / Mobilität",
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  facet_grid(Gruppe ~ ., scales = "fixed") +             # Gruppenzusammenfassung bleibt
  labs(
    title = "Einfluss des pH-Werts auf Nährstoffverfügbarkeit, Metallmobilität und Bodenbiologie"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "right",
    panel.spacing.y   = unit(0.8, "lines"),
    strip.text.y      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", hjust = 0.5),
    axis.title.x      = element_text(margin = margin(t = 8)),
    axis.title.y      = element_text(margin = margin(r = 8))
  )
p
ggsave(
  filename = "ph_boden_naehrstoffverfuegbarkeit.png",
  plot     = p,
  width    = 12,
  height   = 8,
  dpi      = 300,
  units    = "in"
)