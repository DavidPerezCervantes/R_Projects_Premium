# =========================================================================
# DASHBOARD DE DISTRIBUCIONES DISCRETAS (FASE 4 - CÓDIGO BASE)
# =========================================================================

library(shiny)
library(ggplot2)

# --- 1. INTERFAZ DE USUARIO (UI) ---
ui <- fluidPage(
  
  # Título de la aplicación
  titlePanel("Visualizador Interactivo de Probabilidades Discretas"),
  
  sidebarLayout(
    
    # Panel Lateral: Controles e Inputs
    sidebarPanel(
      # Menú de pestañas para cambiar de distribución
      tabsetPanel(id = "dist_tab",
                  
                  # Pestaña 1: Aproximación Poisson
                  tabPanel("Aprox. Poisson", value = "poisson",
                           br(),
                           numericInput("n_pois", "Número de ensayos (n):", value = 100, min = 1),
                           numericInput("p_pois", "Probabilidad de éxito (p):", value = 0.05, min = 0, max = 1, step = 0.01),
                           numericInput("k_pois", "Valor a evaluar (k):", value = 2, min = 0)
                  ),
                  
                  # Pestaña 2: Geométrica
                  tabPanel("Geométrica", value = "geom",
                           br(),
                           helpText("Ensayo en el que ocurre el 1er éxito."),
                           numericInput("p_geom", "Probabilidad de éxito (p):", value = 0.2, min = 0, max = 1, step = 0.01),
                           numericInput("k_geom", "Ensayo objetivo (k):", value = 3, min = 1)
                  ),
                  
                  # Pestaña 3: Binomial Negativa
                  tabPanel("Binomial Negativa", value = "nbinom",
                           br(),
                           helpText("Ensayo en el que ocurre el r-ésimo éxito."),
                           numericInput("r_nbinom", "Número de éxitos objetivo (r):", value = 3, min = 1),
                           numericInput("p_nbinom", "Probabilidad de éxito (p):", value = 0.3, min = 0, max = 1, step = 0.01),
                           numericInput("k_nbinom", "Ensayo objetivo (k):", value = 5, min = 1)
                  )
      ),
      
      hr(), # Línea divisoria
      
      # Control global de Condición (aplica para cualquier pestaña)
      selectInput("condicion", "Condición de la Probabilidad:",
                  choices = c("Exactamente igual (=)" = "eq",
                              "Menor o igual (\u2264)" = "le",
                              "Mayor o igual (\u2265)" = "ge"))
    ),
    
    # Panel Principal: Resultados y Gráficas
    mainPanel(
      # Tarjeta destacada para el resultado numérico
      div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; border: 1px solid #dee2e6; text-align: center; margin-bottom: 20px;",
          h4("Probabilidad Calculada:"),
          h2(textOutput("resultado_num"), style = "color: #2c3e50; font-weight: bold;")
      ),
      # Gráfico de la distribución
      plotOutput("grafico_dist", height = "400px")
    )
  )
)

# --- 2. LÓGICA DEL SERVIDOR (SERVER) ---
server <- function(input, output, session) {
  
  # Función reactiva para consolidar los parámetros según la pestaña activa
  datos_reactivos <- reactive({
    tab <- input$dist_tab
    cond <- input$condicion
    
    if (tab == "poisson") {
      lambda <- input$n_pois * input$p_pois
      k <- input$k_pois
      # Rango para el gráfico de Poisson
      x_vals <- 0:max(10, ceiling(lambda + 4 * sqrt(lambda)))
      
      # Cálculo de probabilidad
      prob <- switch(cond,
                     "eq" = dpois(k, lambda),
                     "le" = ppois(k, lambda),
                     "ge" = ppois(k - 1, lambda, lower.tail = FALSE))
      
      # Definir qué barras resaltar
      highlight <- switch(cond,
                          "eq" = (x_vals == k),
                          "le" = (x_vals <= k),
                          "ge" = (x_vals >= k))
      
      return(list(x = x_vals, y = dpois(x_vals, lambda), prob = prob, highlight = highlight, title = paste("Poisson (\u03BB =", round(lambda, 2), ")")))
      
    } else if (tab == "geom") {
      p <- input$p_geom
      k <- input$k_geom
      x_vals <- 1:max(10, ceiling(1/p + 3*sqrt((1-p)/(p^2))))
      
      # R cuenta fracasos. k ensayos totales = k-1 fracasos.
      prob <- switch(cond,
                     "eq" = dgeom(k - 1, p),
                     "le" = pgeom(k - 1, p),
                     "ge" = pgeom(k - 2, p, lower.tail = FALSE))
      
      highlight <- switch(cond,
                          "eq" = (x_vals == k),
                          "le" = (x_vals <= k),
                          "ge" = (x_vals >= k))
      
      return(list(x = x_vals, y = dgeom(x_vals - 1, p), prob = prob, highlight = highlight, title = "Distribución Geométrica"))
      
    } else if (tab == "nbinom") {
      r <- input$r_nbinom
      p <- input$p_nbinom
      k <- input$k_nbinom
      mu <- r/p
      x_vals <- r:max(r + 10, ceiling(mu + 3*sqrt(r*(1-p)/(p^2))))
      
      # R cuenta fracasos antes del r-ésimo éxito. Fracasos = k - r
      prob <- switch(cond,
                     "eq" = dnbinom(k - r, r, p),
                     "le" = pnbinom(k - r, r, p),
                     "ge" = pnbinom(k - r - 1, r, p, lower.tail = FALSE))
      
      highlight <- switch(cond,
                          "eq" = (x_vals == k),
                          "le" = (x_vals <= k),
                          "ge" = (x_vals >= k))
      
      return(list(x = x_vals, y = dnbinom(x_vals - r, r, p), prob = prob, highlight = highlight, title = "Distribución Binomial Negativa"))
    }
  })
  
  # Renderizar el resultado numérico
  output$resultado_num <- renderText({
    res <- datos_reactivos()$prob
    paste(round(res, 6), " (", round(res * 100, 2), "%)", sep = "")
  })
  
  # Renderizar el gráfico de barras
  output$grafico_dist <- renderPlot({
    datos <- datos_reactivos()
    df <- data.frame(x = datos$x, y = datos$y, Destacado = datos$highlight)
    
    ggplot(df, aes(x = as.factor(x), y = y, fill = Destacado)) +
      geom_bar(stat = "identity", color = "black", alpha = 0.8) +
      scale_fill_manual(values = c("FALSE" = "#bdc3c7", "TRUE" = "#e67e22")) +
      labs(title = datos$title, x = "Número de Ensayos (k)", y = "Probabilidad P(X = k)") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)