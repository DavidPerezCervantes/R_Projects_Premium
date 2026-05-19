# -------------------------------------------------------------------------
# Dashboard: Probabilidad Binomial (Temas 7.2 y 7.3 - M. Finan)
# Objetivo: Herramienta pedagógica interactiva para estudiantes de Actuaría
# -------------------------------------------------------------------------

if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# --- INTERFAZ DE USUARIO (UI) ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Probabilidad Binomial"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculadora Actuarial", tabName = "dashboard", icon = icon("calculator")),
      div(style = "padding: 20px;",
          numericInput("n", "Ensayos (n):", value = 10, min = 1, max = 500),
          sliderInput("p", "Probabilidad de éxito (p):", min = 0, max = 1, value = 0.5, step = 0.01),
          hr(),
          selectInput("tipo_calc", "Tipo de Probabilidad:",
                      choices = list("Exactamente X = k" = "igual",
                                     "A lo sumo X ≤ k" = "menor_igual",
                                     "Al menos X ≥ k" = "mayor_igual",
                                     "Rango a ≤ X ≤ b" = "rango")),
          uiOutput("controles_k"),
          hr(),
          radioButtons("tipo_grafico", "Tipo de Visualización:",
                       choices = list("Masa (PMF)" = "pmf", "Acumulada (CDF)" = "cdf"))
      )
    )
  ),
  
  dashboardBody(
    withMathJax(), # Soporte para fórmulas LaTeX
    tags$head(tags$style(HTML("
      .small-box {height: 110px;} 
      .math-box { font-size: 16px; background-color: #f9f9f9; padding: 15px; border-radius: 5px; border-left: 5px solid #f39c12; }
    "))),
    
    fluidRow(
      valueBoxOutput("prob_box", width = 4),
      valueBoxOutput("esp_box", width = 4),
      valueBoxOutput("var_box", width = 4)
    ),
    
    fluidRow(
      box(title = "Visualización de la Distribución", status = "primary", solidHeader = TRUE, width = 8,
          plotOutput("plot_binomial", height = "400px")),
      
      box(title = "Desglose de Cálculos (Paso a Paso)", status = "warning", solidHeader = TRUE, width = 4,
          uiOutput("procedimiento"))
    )
  )
)

# --- LÓGICA DEL SERVIDOR (SERVER) ---
server <- function(input, output) {
  
  # Controles dinámicos para k según el tipo de cálculo
  output$controles_k <- renderUI({
    if(input$tipo_calc == "rango"){
      tagList(
        numericInput("k_a", "Desde (a):", value = 2, min = 0, max = input$n),
        numericInput("k_b", "Hasta (b):", value = 5, min = 0, max = input$n)
      )
    } else {
      numericInput("k", "Valor k:", value = 3, min = 0, max = input$n)
    }
  })
  
  # Cálculo reactivo de la probabilidad
  calc_prob <- reactive({
    n <- input$n; p <- input$p
    if(input$tipo_calc == "igual") return(dbinom(input$k, n, p))
    if(input$tipo_calc == "menor_igual") return(pbinom(input$k, n, p))
    if(input$tipo_calc == "mayor_igual") return(1 - pbinom(input$k - 1, n, p))
    if(input$tipo_calc == "rango") return(pbinom(input$k_b, n, p) - pbinom(input$k_a - 1, n, p))
  })
  
  # Outputs de cajas informativas (ValueBoxes)
  output$prob_box <- renderValueBox({
    valueBox(paste0(round(calc_prob() * 100, 2), "%"), "Probabilidad Final", icon = icon("percent"), color = "purple")
  })
  
  output$esp_box <- renderValueBox({
    valueBox(round(input$n * input$p, 4), "Media E[X]", icon = icon("chart-line"), color = "blue")
  })
  
  output$var_box <- renderValueBox({
    valueBox(round(input$n * input$p * (1 - input$p), 4), "Varianza Var(X)", icon = icon("balance-scale"), color = "navy")
  })
  
  # Renderizado del gráfico con resaltado de área
  output$plot_binomial <- renderPlot({
    n <- input$n; p <- input$p
    df <- data.frame(x = 0:n, prob = dbinom(0:n, n, p), cum = pbinom(0:n, n, p))
    
    # Lógica de coloreado reactivo
    df$highlight <- FALSE
    if(input$tipo_calc == "igual") df$highlight[df$x == input$k] <- TRUE
    if(input$tipo_calc == "menor_igual") df$highlight[df$x <= input$k] <- TRUE
    if(input$tipo_calc == "mayor_igual") df$highlight[df$x >= input$k] <- TRUE
    if(input$tipo_calc == "rango") df$highlight[df$x >= input$k_a & df$x <= input$k_b] <- TRUE
    
    if(input$tipo_grafico == "pmf"){
      ggplot(df, aes(x = x, y = prob, fill = highlight)) +
        geom_bar(stat = "identity", color = "black", alpha = 0.8) +
        scale_fill_manual(values = c("TRUE" = "#3c8dbc", "FALSE" = "#d2d6de"), guide = "none") +
        labs(title = "Función de Masa de Probabilidad (PMF)", y = "P(X = x)", x = "Éxitos (x)") + 
        theme_minimal()
    } else {
      ggplot(df, aes(x = x, y = cum)) +
        geom_step(color = "#3c8dbc", size = 1) +
        geom_point(aes(color = highlight), size = 3) +
        scale_color_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#3c8dbc"), guide = "none") +
        labs(title = "Función de Distribución Acumulada (CDF)", y = "F(x) = P(X <= x)", x = "x") + 
        theme_minimal()
    }
  })
  
  # Desglose matemático interactivo (Fase 5 - Corregida)
  output$procedimiento <- renderUI({
    n <- input$n; p <- input$p; q <- 1-p
    
    # Texto de la fórmula según el caso
    formula_latex <- switch(input$tipo_calc,
                            "igual" = sprintf("P(X = %d)", input$k),
                            "menor_igual" = sprintf("P(X \\leq %d) = \\sum_{i=0}^{%d} P(X=i)", input$k, input$k),
                            "mayor_igual" = sprintf("P(X \\geq %d) = \\sum_{i=%d}^{%d} P(X=i)", input$k, input$k, n),
                            "rango" = sprintf("P(%d \\leq X \\leq %d) = \\sum_{i=%d}^{%d} P(X=i)", input$k_a, input$k_b, input$k_a, input$k_b)
    )
    
    div(class = "math-box",
        withMathJax(
          h5(strong("Estadísticos (7.3):")),
          p(sprintf("$$E[X] = np = %.2f$$", n*p)),
          p(sprintf("$$\\text{Var}(X) = npq = %.4f$$", n*p*q)),
          p(sprintf("$$\\sigma = \\sqrt{npq} = %.4f$$", sqrt(n*p*q))),
          hr(),
          h5(strong("Planteamiento (7.2):")),
          p(sprintf("$$%s$$", formula_latex)),
          if(input$tipo_calc == "igual") {
            tagList(
              p("Aplicando fórmula puntual:"),
              p(sprintf("$$P(X=%d) = \\binom{%d}{%d} (%.2f)^{%d} (%.2f)^{%d}$$", 
                        input$k, n, input$k, p, input$k, q, n - input$k)),
              p(sprintf("$$\\text{Coeficiente } \\binom{n}{k} = %.0f$$", choose(n, input$k)))
            )
          } else {
            p(em("El resultado es la suma de las probabilidades individuales de los eventos sombreados en azul."))
          }
        )
    )
  })
}

# Ejecutar la App
shinyApp(ui, server)