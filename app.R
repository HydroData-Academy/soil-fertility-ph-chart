library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------- Daten vorbereiten (wie in deinem Script) ----------

ph <- seq(3, 9, by = 0.01)
gauss <- function(x, mu, sigma, max = 1) {
  max * exp(-0.5 * ((x - mu) / sigma)^2)
}

df <- data.frame(
  pH = ph,
  N  = gauss(ph, 6.8, 0.8),
  P  = gauss(ph, 6.5, 0.6),
  K  = gauss(ph, 6.5, 0.9),
  Ca = gauss(ph, 7.0, 0.8),
  Mg = gauss(ph, 7.0, 0.8),
  S  = gauss(ph, 6.5, 0.9),
  Fe = gauss(ph, 4.5, 0.7),
  Mn = gauss(ph, 4.5, 0.8),
  Zn = gauss(ph, 5.0, 0.8),
  Cu = gauss(ph, 5.0, 0.8),
  B  = gauss(ph, 5.5, 0.7),
  Mo = gauss(ph, 8.0, 0.7),
  Al = gauss(ph, 4.2, 0.4, max = 1),
  Mikrobielle_Aktivitaet = gauss(ph, 6.5, 0.7)
)

long <- df %>%
  pivot_longer(-pH, names_to = "Stoff", values_to = "Verfuegbarkeit") %>%
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

# ---------- Shiny UI ----------

ui <- fluidPage(
  titlePanel("Einfluss des pH-Werts auf Nährstoffverfügbarkeit"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gruppe", "Gruppe auswählen:",
                  choices = unique(long$Gruppe),
                  selected = "Makronährstoffe"),
      
      uiOutput("stoff_ui"), # dynamische Stoffauswahl
      
      numericInput("ph_input", "Gemessener pH-Wert:", value = 6.5, min = 3, max = 9, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("ph_plot", height = "700px")
    )
  )
)

# ---------- Shiny Server ----------

server <- function(input, output, session) {
  
  # dynamische Stoffliste je nach Gruppe
  output$stoff_ui <- renderUI({
    stoffe <- long %>%
      filter(Gruppe == input$gruppe) %>%
      pull(Stoff) %>%
      unique()
    
    selectInput("stoff", "Stoff(e) auswählen:", choices = stoffe,
                selected = stoffe[1], multiple = TRUE)
  })
  
  output$ph_plot <- renderPlot({
    req(input$stoff)
    
    plot_data <- long %>%
      filter(Gruppe == input$gruppe, Stoff %in% input$stoff)
    
    ggplot(plot_data, aes(x = pH, y = Verfuegbarkeit, group = Stoff)) +
      geom_area(aes(fill = Stoff), alpha = 0.25, colour = NA, position = "identity") +
      geom_line(aes(colour = Stoff), size = 0.8) +
      geom_vline(xintercept = input$ph_input, colour = "red", size = 1, linetype = "dashed") +
      scale_fill_manual(values = farben, guide = "none") +
      scale_colour_manual(values = farben, name = "Stoff / Prozess") +
      scale_x_continuous(name = "pH-Wert des Bodens", limits = c(3, 9), breaks = 3:9) +
      scale_y_continuous(name = "Relative Verfügbarkeit / Mobilität", limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
      facet_grid(Gruppe ~ ., scales = "fixed") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "right",
        panel.spacing.y = unit(0.8, "lines"),
        strip.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)
      ) +
      labs(title = "Einfluss des pH-Werts auf Nährstoffverfügbarkeit, Metallmobilität und Bodenbiologie")
  })
}

# ---------- Run App ----------
shinyApp(ui = ui, server = server)