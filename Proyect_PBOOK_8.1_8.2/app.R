library(shiny)
library(ggplot2)
library(rhandsontable)
library(dplyr)

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  withMathJax(),
  tags$head(tags$style(HTML("
    .math-box { background: #f9f9f9; border-left: 5px solid #2c3e50; padding: 15px; margin-top: 20px; }
    .result-val { color: #e74c3c; font-weight: bold; }
  "))),
  
  titlePanel("Arquitecto de Dashboards: Probabilidad Actuarial"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 radioButtons("tipo_var", "1. Tipo de Variable:", 
                              choices = c("Discreta" = "disc", "Continua" = "cont")),
                 
                 conditionalPanel(
                   condition = "input.tipo_var == 'disc'",
                   selectInput("tipo_input_disc", "Datos de entrada:",
                               choices = c("Masa (PMF) P(X=x)" = "pmf", "Acumulada (CDF) F(x)" = "cdf")),
                   rHandsontableOutput("tabla_discreta")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tipo_var == 'cont'",
                   textInput("func_cont", "Función f(x):", value = "2*x"),
                   sliderInput("rango_cont", "Dominio de X:", min = -10, max = 10, value = c(0, 1), step = 0.1),
                   helpText("Usa 'x' como variable (ej: 3*x^2, exp(-x), 1/2).")
                 ),
                 
                 hr(),
                 h4("2. Calcular Probabilidad"),
                 numericInput("lim_a", "Valor inferior (a):", value = 0),
                 numericInput("lim_b", "Valor superior (b):", value = 1),
                 p(em("Calcula P(a < X <= b)"))
    ),
    
    mainPanel(width = 8,
              fluidRow(
                column(6, plotOutput("plot_pmf")),
                column(6, plotOutput("plot_cdf"))
              ),
              uiOutput("area_matematica")
    )
  )
)

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- LÓGICA DISCRETA ---
  rv <- reactiveValues(data = data.frame(x = 0:4, Valor = c(1, 4, 6, 4, 1)/16))
  
  output$tabla_discreta <- renderRHandsontable({
    rhandsontable(rv$data, rowHeaders = FALSE) %>% hot_col("x", type = "numeric")
  })
  
  observeEvent(input$tabla_discreta, { rv$data <- hot_to_r(input$tabla_discreta) })
  
  # --- MOTOR DE CÁLCULO REACTIVO ---
  res_calc <- reactive({
    if(input$tipo_var == "disc") {
      df <- rv$data %>% arrange(x)
      if(input$tipo_input_disc == "pmf") {
        df$PMF <- df$Valor; df$CDF <- cumsum(df$PMF)
      } else {
        df$CDF <- df$Valor; df$PMF <- c(df$CDF[1], diff(df$CDF))
      }
      prob_rango <- sum(df$PMF[df$x > input$lim_a & df$x <= input$lim_b])
      return(list(df = df, prob = prob_rango, ex = sum(df$x * df$PMF)))
    } else {
      # Lógica Continua con Integración Numérica
      f <- function(x) { eval(parse(text = input.func_cont), list(x = x)) }
      # Intentar evaluar la función para validar
      func_str <- input$func_cont
      f_aux <- function(x) { 
        safe_env <- new.env()
        assign("x", x, envir = safe_env)
        eval(parse(text = func_str), envir = safe_env)
      }
      
      e_x <- tryCatch(integrate(function(x) x * f_aux(x), input$rango_cont[1], input$rango_cont[2])$value, error = function(e) 0)
      p_rango <- tryCatch(integrate(f_aux, input$lim_a, input$lim_b)$value, error = function(e) 0)
      
      return(list(f = f_aux, prob = p_rango, ex = e_x))
    }
  })
  
  # --- GRÁFICOS ---
  output$plot_pmf <- renderPlot({
    res <- res_calc()
    if(input$tipo_var == "disc") {
      ggplot(res$df, aes(x, PMF)) + geom_segment(aes(xend=x, yend=0), color="steelblue", size=1.2) +
        geom_point(size=4) + theme_minimal() + labs(title="PMF: Probabilidad Puntual")
    } else {
      ggplot(data.frame(x = input$rango_cont), aes(x)) + 
        stat_function(fun = res$f, color="steelblue", size=1.2) +
        geom_area(stat = "function", fun = res$f, fill="steelblue", alpha=0.3, xlim=c(input$lim_a, input$lim_b)) +
        theme_minimal() + labs(title="PDF: Función de Densidad")
    }
  })
  
  output$plot_cdf <- renderPlot({
    res <- res_calc()
    if(input$tipo_var == "disc") {
      ggplot(res$df, aes(x, CDF)) + geom_step(direction="hv", color="darkred") +
        geom_point(color="darkred") + theme_minimal() + labs(title="CDF: Probabilidad Acumulada")
    } else {
      F_cont <- function(val) { sapply(val, function(v) integrate(res$f, input$rango_cont[1], v)$value) }
      ggplot(data.frame(x = input$rango_cont), aes(x)) + 
        stat_function(fun = F_cont, color="darkred", size=1.2) +
        theme_minimal() + labs(title="CDF: Función Acumulada")
    }
  })
  
  # --- SALIDA LATEX ---
  output$area_matematica <- renderUI({
    res <- res_calc()
    div(class="math-box",
        if(input$tipo_var == "disc") {
          withMathJax(sprintf("$$\\text{Probabilidad en rango: } P(%g < X \\le %g) = %.4f$$
                             $$\\text{Esperanza: } E[X] = \\sum x \\cdot p(x) = %.4f$$", 
                              input$lim_a, input$lim_b, res$prob, res$ex))
        } else {
          withMathJax(sprintf("$$\\text{Probabilidad (Área): } P(%g < X < %g) = \\int_{%g}^{%g} f(x)dx = %.4f$$
                             $$\\text{Esperanza: } E[X] = \\int_{-\\infty}^{\\infty} x \\cdot f(x)dx \\approx %.4f$$", 
                              input$lim_a, input$lim_b, input$lim_a, input$lim_b, res$prob, res$ex))
        }
    )
  })
}

shinyApp(ui, server)