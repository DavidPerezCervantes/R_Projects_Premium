library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# ==========================================
# 1. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- navbarPage(
  title = "🚀 Solucionario: Ejercicios Medium (Cap. 4)",
  theme = bs_theme(bootswatch = "flatly", primary = "#2c3e50"),
  
  tabPanel("Módulo 1: 4M1 y 4M2",
           sidebarLayout(
             sidebarPanel(
               h4("Control de Simulación (4M1)"),
               p("Simularemos estaturas basadas únicamente en los Priors."),
               sliderInput("n_sim", "Número de muestras:", 
                           min = 1000, max = 20000, value = 10000, step = 1000),
               
               hr(),
               h4("Parámetros del Prior"),
               helpText("\u03bc ~ Normal(0, 10)"),
               helpText("\u03c3 ~ Exponencial(1)"),
               
               hr(),
               div(style = "background-color: #d5f5e3; padding: 10px; border-radius: 5px;",
                   p("💡 En 4M2, el reto es escribir la función de optimización que encontraría el MAP.")
               )
             ),
             
             mainPanel(
               h3("4M1: Distribución Predictiva Prior"),
               p("Esta gráfica muestra qué alturas 'espera' ver el modelo antes de conocer a los humanos reales. Nota que al estar centrado en 0, ¡el modelo cree que existen alturas negativas!"),
               plotOutput("plot_4m1", height = "400px"),
               
               hr(),
               h3("4M2: Traducción a Código (Aproximación Cuadrática)"),
               p("Para resolver el 4M2 sin la librería 'rethinking', la estructura de optimización en R sería esta:"),
               verbatimTextOutput("code_4m2")
             )
           )
  )
)

# ==========================================
# 2. LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # --- LÓGICA 4M1: SIMULACIÓN ---
  data_sim <- reactive({
    set.seed(42)
    # Paso 1: Muestrear parámetros de los Priors
    mu_samples <- rnorm(input$n_sim, 0, 10)
    sigma_samples <- rexp(input$n_sim, 1)
    
    # Paso 2: Generar estaturas h_i basadas en esos parámetros
    h_sim <- rnorm(input$n_sim, mean = mu_samples, sd = sigma_samples)
    
    data.frame(h = h_sim)
  })
  
  output$plot_4m1 <- renderPlot({
    ggplot(data_sim(), aes(x = h)) +
      geom_density(fill = "#3498db", alpha = 0.6, color = "#2980b9", linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Alturas simuladas desde el Prior",
           subtitle = "Nota la gran cantidad de valores por debajo de 0",
           x = "Altura (h)", y = "Densidad") +
      theme_minimal(base_size = 14)
  })
  
  # --- LÓGICA 4M2: EXPLICACIÓN DE CÓDIGO ---
  output$code_4m2 <- renderText({
    "
    # Receta para Aproximación Cuadrática (4M2):
    
    log_posterior <- function(pars, data_y) {
      mu <- pars[1]
      sigma <- exp(pars[2]) # Aseguramos sigma positivo
      
      # 1. Likelihood
      log_lik <- sum(dnorm(data_y, mean = mu, sd = sigma, log = TRUE))
      
      # 2. Priors
      log_prior_mu <- dnorm(mu, 0, 10, log = TRUE)
      log_prior_sigma <- dexp(sigma, 1, log = TRUE)
      
      # Retornamos el Negativo para minimizar con optim()
      return(-(log_lik + log_prior_mu + log_prior_sigma))
    }
    
    # El comando optim() encontraría el pico (MAP) y la curvatura (Hessiana)
    "
  })
}

shinyApp(ui = ui, server = server)
