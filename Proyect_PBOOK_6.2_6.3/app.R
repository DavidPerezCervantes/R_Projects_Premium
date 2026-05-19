library(shiny)
library(rhandsontable)
library(plotly)
library(dplyr)

# Función para convertir texto (decimales o fracciones) a número
parse_math <- function(x) {
  sapply(x, function(val) {
    if (is.na(val) || val == "") return(NA)
    res <- tryCatch(eval(parse(text = val)), error = function(e) NA)
    return(as.numeric(res))
  })
}

ui <- fluidPage(
  withMathJax(),
  titlePanel("ActuaryApp: Solver Actuarial Completo"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Ingresa datos"),
      rHandsontableOutput("hot"),
      br(),
      helpText("Escribe fracciones (1/15) o decimales."),
      hr(),
      h4("2. Calcular P(X <= k)"),
      numericInput("k_val", "Ingresa el valor de k:", value = 3, step = 1),
      hr(),
      uiOutput("validation_msg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualización", plotlyOutput("pmf_plot"), plotlyOutput("cdf_plot")),
        tabPanel("Tutor: Explicación", uiOutput("tutor_explanation"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(data = data.frame(
    x = c(1, 2, 3, 4, 5), 
    p = c("1/15", "2/15", "3/15", "4/15", "5/15"), 
    stringsAsFactors = FALSE
  ))
  
  output$hot <- renderRHandsontable({
    rhandsontable(values$data, allowInvalid = TRUE) %>% 
      hot_col("x", format = "0") %>% 
      hot_col("p", type = "text")
  })
  
  get_clean_data <- reactive({
    if (is.null(input$hot)) return(values$data)
    df <- hot_to_r(input$hot)
    df$x <- as.numeric(as.character(df$x))
    df$p <- parse_math(as.character(df$p))
    df <- df[!is.na(df$x) & !is.na(df$p), ] %>% arrange(x)
    return(df)
  })
  
  output$validation_msg <- renderUI({
    df <- get_clean_data()
    if(nrow(df) == 0) return(div(style="color:orange;", "Tabla vacía"))
    sum_p <- sum(df$p)
    if (abs(sum_p - 1) < 0.005) { div(style="color:green; font-weight:bold;", "Suma = 1 (Válida)") }
    else { div(style="color:red; font-weight:bold;", paste("Suma =", round(sum_p, 4), ". Debe ser 1.")) }
  })
  
  output$pmf_plot <- renderPlotly({
    plot_ly(get_clean_data(), x = ~x, y = ~p, type = 'bar', name = 'PMF')
  })
  
  output$cdf_plot <- renderPlotly({
    df <- get_clean_data() %>% mutate(cdf = cumsum(p))
    plot_ly(df, x = ~x, y = ~cdf, type = 'scatter', mode = 'lines+markers', line = list(shape = 'hv'))
  })
  
  output$tutor_explanation <- renderUI({
    df <- get_clean_data()
    if(nrow(df) == 0) return(p("Ingresa datos para ver la explicación."))
    
    # 1. Cálculo específico para P(X <= k)
    k <- input$k_val
    sub_df <- df %>% filter(x <= k)
    suma_acumulada <- sum(sub_df$p)
    
    # 2. Cálculos estadísticos
    ex <- sum(df$x * df$p)
    ex2 <- sum((df$x^2) * df$p)
    var_x <- ex2 - (ex^2)
    sd_x <- sqrt(var_x)
    
    # Cadenas para suma expandida
    sum_ex <- paste(paste0("(", df$x, "\\cdot", round(df$p, 4), ")"), collapse = " + ")
    sum_ex2 <- paste(paste0("(", df$x, "^2 \\cdot", round(df$p, 4), ")"), collapse = " + ")
    
    withMathJax(
      h4("Cálculo de Probabilidad Acumulada P(X <= k)"),
      p(paste0("$$P(X \\le ", k, ") = \\sum_{x \\le ", k, "} P(x) = ", 
               paste(paste0("P(", sub_df$x, ")"), collapse = " + "), 
               " = ", round(suma_acumulada, 4), "$$")),
      hr(),
      h4("Cálculos Estadísticos (Sección 6.3)"),
      p(paste0("$$E[X] = \\sum x \\cdot P(x) = ", sum_ex, " = ", round(ex, 4), "$$")),
      p(paste0("$$E[X^2] = \\sum x^2 \\cdot P(x) = ", sum_ex2, " = ", round(ex2, 4), "$$")),
      p(paste0("$$Var(X) = E[X^2] - (E[X])^2 = ", round(ex2, 4), " - (", round(ex, 4), ")^2 = ", round(var_x, 4), "$$")),
      p(paste0("$$\\sigma_X = \\sqrt{Var(X)} = \\sqrt{", round(var_x, 4), "} = ", round(sd_x, 4), "$$"))
    )
  })
}

shinyApp(ui = ui, server = server)