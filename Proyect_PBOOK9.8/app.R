
#DAVID PEREZ CERVANTES  

library(shiny)
library(bslib)

# ==========================================
# 1. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- navbarPage(
  title = "📊 Laboratorio Actuarial: Distribución Exponencial",
  theme = bs_theme(bootswatch = "lumen", primary = "#2c3e50"),
  
  # --- PESTAÑA 1: TEORÍA ---
  tabPanel("1. Teoría y Gráficas",
           sidebarLayout(
             sidebarPanel(
               h4("Parámetro del Modelo"),
               p("La distribución exponencial modela tiempos de espera o fallas. El único parámetro necesario es la tasa de ocurrencia (\u03bb)."),
               sliderInput("lambda_teoria", "Tasa (\u03bb):", 
                           min = 0.05, max = 2, value = 0.5, step = 0.05),
               hr(),
               h4("Estadísticas Teóricas"),
               div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                   htmlOutput("stats_teoria")
               )
             ),
             mainPanel(
               h4("Función de Densidad de Probabilidad (pdf)"),
               plotOutput("plot_pdf", height = "250px"),
               br(),
               h4("Función de Distribución Acumulada (cdf)"),
               plotOutput("plot_cdf", height = "250px")
             )
           )
  ),
  
  # --- PESTAÑA 2: EFECTO SIN MEMORIA (NUEVA) ---
  tabPanel("2. Efecto Sin Memoria",
           sidebarLayout(
             sidebarPanel(
               h4("1. Configura el Escenario"),
               sliderInput("mem_lambda", "Tasa (\u03bb):", min = 0.05, max = 2, value = 0.2, step = 0.05),
               numericInput("mem_s", "Tiempo ya transcurrido (s):", value = 5, min = 0, step = 1),
               numericInput("mem_t", "Tiempo adicional (t):", value = 3, min = 0, step = 1),
               hr(),
               h4("2. La Matemática"),
               p("La probabilidad de esperar un tiempo 't' adicional, dado que ya esperamos 's' tiempo, es igual a la probabilidad inicial de esperar 't'."),
               HTML("<div style='background-color: #ecf0f1; padding: 10px; border-radius: 5px; text-align: center;'>
                    <b>P(X > s+t | X > s) = P(X > t)</b>
                    </div>")
             ),
             mainPanel(
               fluidRow(
                 column(6, 
                        h4("Escenario A: Desde Cero"),
                        p("Probabilidad original de esperar más de 't' tiempo. P(X > t)"),
                        plotOutput("plot_mem_orig", height = "280px")
                 ),
                 column(6, 
                        h4("Escenario B: Condicionado"),
                        p(HTML("Probabilidad de esperar 't' adicional. P(X > s+t | X > s)")),
                        plotOutput("plot_mem_cond", height = "280px")
                 )
               ),
               br(),
               div(style = "background-color: #fcf3cf; padding: 20px; border-radius: 5px; text-align: center; font-size: 18px;",
                   htmlOutput("texto_memoria")
               )
             )
           )
  ),
  
  # --- PESTAÑA 3: CALCULADORA UNIVERSAL ---
  tabPanel("3. Calculadora Actuarial",
           sidebarLayout(
             sidebarPanel(
               h4("1. Define el Parámetro"),
               radioButtons("tipo_input", "Ingresar el parámetro como:",
                            choices = list("Tasa (\u03bb)" = "lambda",
                                           "Media Original E(X)" = "media",
                                           "Mediana Original" = "mediana")),
               numericInput("valor_param", "Valor:", value = 2, min = 0.0001),
               
               hr(),
               h4("2. Tipo de Cálculo"),
               selectInput("tipo_calculo", "Selecciona la operación:",
                           choices = list(
                             "P(X < x) - Prob. Acumulada" = "menor",
                             "P(X > x) - Prob. de Supervivencia" = "mayor",
                             "P(a < X < b) - Prob. en Intervalo" = "intervalo",
                             "P(X > s+t | X > s) - Prob. Condicional" = "condicional",
                             "Calcular Cuantil / Percentil" = "percentil",
                             "MÓDULO DE SEGUROS (Deducibles y Topes)" = "seguros"
                           )),
               
               conditionalPanel(
                 condition = "input.tipo_calculo == 'menor' || input.tipo_calculo == 'mayor'",
                 numericInput("val_x", "Valor de x:", value = 5, min = 0)
               ),
               conditionalPanel(
                 condition = "input.tipo_calculo == 'intervalo'",
                 numericInput("val_a", "Límite inferior (a):", value = 2, min = 0),
                 numericInput("val_b", "Límite superior (b):", value = 4, min = 0)
               ),
               conditionalPanel(
                 condition = "input.tipo_calculo == 'condicional'",
                 numericInput("val_s", "Tiempo ya esperado (s):", value = 2, min = 0),
                 numericInput("val_t", "Tiempo adicional (t):", value = 3, min = 0)
               ),
               conditionalPanel(
                 condition = "input.tipo_calculo == 'percentil'",
                 numericInput("val_p", "Probabilidad Acumulada (p):", value = 0.5, min = 0, max = 0.9999, step = 0.01)
               ),
               conditionalPanel(
                 condition = "input.tipo_calculo == 'seguros'",
                 numericInput("val_d", "Deducible (d):", value = 1, min = 0),
                 radioButtons("tiene_tope", "¿La póliza tiene un tope de pago máximo (m)?", 
                              choices = c("No (Pago ilimitado)", "Sí (Tiene límite)"), selected = "Sí (Tiene límite)"),
                 conditionalPanel(
                   condition = "input.tiene_tope == 'Sí (Tiene límite)'",
                   numericInput("val_m", "Pago Máximo / Tope (m):", value = 5, min = 0)
                 )
               )
             ),
             
             mainPanel(
               h3("Resultado del Cálculo"),
               div(style = "background-color: #e8f4f8; padding: 20px; border-radius: 8px; font-size: 16px;",
                   htmlOutput("resultado_calc")
               ),
               br(),
               h4("Visualización del Área"),
               plotOutput("plot_calc", height = "350px")
             )
           )
  )
)

# ==========================================
# 2. LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # --- LOGICA PESTAÑA 1 ---
  output$stats_teoria <- renderUI({
    lam <- input$lambda_teoria
    HTML(paste0("<b>E(X) = </b>", round(1/lam, 4), " (Media)<br><b>Var(X) = </b>", round(1/(lam^2), 4), " (Varianza)"))
  })
  
  output$plot_pdf <- renderPlot({
    lam <- input$lambda_teoria; x_max <- qexp(0.999, rate = lam)
    par(mar = c(4, 4, 1, 1), bg = "white")
    curve(dexp(x, rate = lam), from = 0, to = x_max, col = "#c0392b", lwd = 3, ylab = "f(x)", xlab = "x")
    grid()
  })
  
  output$plot_cdf <- renderPlot({
    lam <- input$lambda_teoria; x_max <- qexp(0.999, rate = lam)
    par(mar = c(4, 4, 1, 1), bg = "white")
    curve(pexp(x, rate = lam), from = 0, to = x_max, col = "#2980b9", lwd = 3, ylab = "F(x)", xlab = "x")
    grid()
  })
  
  # --- LOGICA PESTAÑA 2 (EFECTO SIN MEMORIA) ---
  output$plot_mem_orig <- renderPlot({
    lam <- input$mem_lambda; t <- input$mem_t
    x_max <- qexp(0.99, rate = lam)
    
    par(mar = c(4, 4, 1, 1), bg = "white")
    curve(dexp(x, rate = lam), from = 0, to = x_max, col = "black", lwd = 2, ylab = "Densidad f(x)", xlab = "Tiempo de espera (x)")
    
    x_vals <- seq(t, x_max, length.out = 100)
    y_vals <- dexp(x_vals, rate = lam)
    polygon(c(t, x_vals, x_max), c(0, y_vals, 0), col = rgb(0.2, 0.6, 0.8, 0.6), border = NA)
    abline(v = t, col = "#2980b9", lwd = 2, lty = 2)
  })
  
  output$plot_mem_cond <- renderPlot({
    lam <- input$mem_lambda; s <- input$mem_s; t <- input$mem_t
    x_max <- qexp(0.99, rate = lam)
    
    par(mar = c(4, 4, 1, 1), bg = "white")
    
    # La densidad condicionada matemáticamente: f(x) / P(X>s)
    cond_dens <- function(x) { dexp(x, rate = lam) / exp(-lam * s) }
    
    # El truco visual es graficar la curva condicionada pero en el rango de s hasta s+x_max
    curve(cond_dens(x), from = s, to = s + x_max, col = "black", lwd = 2, ylab = "Densidad Condicionada", xlab = "Tiempo total (x)")
    
    x_vals <- seq(s + t, s + x_max, length.out = 100)
    y_vals <- cond_dens(x_vals)
    polygon(c(s + t, x_vals, s + x_max), c(0, y_vals, 0), col = rgb(0.8, 0.4, 0.2, 0.6), border = NA)
    abline(v = s + t, col = "#d35400", lwd = 2, lty = 2)
  })
  
  output$texto_memoria <- renderUI({
    lam <- input$mem_lambda; s <- input$mem_s; t <- input$mem_t
    
    prob_orig <- exp(-lam * t)
    prob_cond <- exp(-lam * (s + t)) / exp(-lam * s)
    
    HTML(paste0(
      "Probabilidad Escenario A: <b>", round(prob_orig, 4), "</b><br>",
      "Probabilidad Escenario B: <b>", round(prob_cond, 4), "</b><br><br>",
      "<h3 style='color: #2c3e50; margin: 0;'>¡Las áreas de ambas gráficas son exactamente iguales!</h3>"
    ))
  })
  
  # --- LOGICA PESTAÑA 3 (CALCULADORA) ---
  get_lambda <- reactive({
    req(input$valor_param > 0)
    if(input$tipo_input == "lambda") return(input$valor_param)
    if(input$tipo_input == "media") return(1 / input$valor_param)
    if(input$tipo_input == "mediana") return(log(2) / input$valor_param)
  })
  
  output$resultado_calc <- renderUI({
    lam <- get_lambda(); tipo <- input$tipo_calculo
    
    if(tipo == "menor") {
      res <- pexp(input$val_x, rate = lam)
      HTML(sprintf("<b>Fórmula:</b> P(X < %s) = 1 - e^(-%s * %s)<br><b>Resultado:</b> <span style='color:red; font-size:22px;'><b>%.4f</b></span>", input$val_x, round(lam,4), input$val_x, res))
    } else if(tipo == "mayor") {
      res <- 1 - pexp(input$val_x, rate = lam)
      HTML(sprintf("<b>Fórmula:</b> P(X > %s) = e^(-%s * %s)<br><b>Resultado:</b> <span style='color:red; font-size:22px;'><b>%.4f</b></span>", input$val_x, round(lam,4), input$val_x, res))
    } else if(tipo == "intervalo") {
      a <- input$val_a; b <- input$val_b
      if(a >= b) return(HTML("<b style='color:red;'>Error: (a) debe ser menor a (b).</b>"))
      res <- pexp(b, rate = lam) - pexp(a, rate = lam)
      HTML(sprintf("<b>Fórmula:</b> e^(-%s * %s) - e^(-%s * %s)<br><b>Resultado:</b> <span style='color:red; font-size:22px;'><b>%.4f</b></span>", round(lam,4), a, round(lam,4), b, res))
    } else if(tipo == "condicional") {
      t <- input$val_t; res <- 1 - pexp(t, rate = lam) 
      HTML(sprintf("<b>Por propiedad sin memoria:</b> P(X > %s)<br><b>Resultado:</b> <span style='color:red; font-size:22px;'><b>%.4f</b></span>", t, res))
    } else if(tipo == "percentil") {
      p <- input$val_p; res <- qexp(p, rate = lam)
      HTML(sprintf("<b>Fórmula Inversa:</b> x = -ln(1 - %s) / %s<br><b>Resultado (Cuantil x):</b> <span style='color:red; font-size:22px;'><b>%.4f</b></span>", p, round(lam,4), res))
    } else if(tipo == "seguros") {
      d <- input$val_d; tiene_tope <- input$tiene_tope == "Sí (Tiene límite)"
      prob_pago <- exp(-lam * d); mediana_pago_base <- max(0, (log(2) / lam) - d)
      
      if(tiene_tope) {
        m <- input$val_m
        esp_pago <- (1/lam) * (exp(-lam * d) - exp(-lam * (d + m)))
        mediana_pago <- min(mediana_pago_base, m)
        momento2 <- (2/(lam^2)) * (exp(-lam * d) - exp(-lam * (d + m))) - (2*m/lam) * exp(-lam * (d + m))
        var_pago <- momento2 - (esp_pago^2)
        
        HTML(paste0(
          "<b>Deducible (d):</b> ", d, " | <b>Tope máximo de pago (m):</b> ", m, "<br><hr>",
          "<b>1. Prob. de que la aseguradora pague algo P(X > d):</b> ", round(prob_pago, 4), "<br>",
          "<b>2. Pago Esperado E[Y] (Media del Pago):</b> <span style='color:red; font-size:20px;'><b>", round(esp_pago, 4), "</b></span><br>",
          "<b>3. Mediana del Pago:</b> ", round(mediana_pago, 4), "<br>",
          "<b>4. Varianza del Pago Var(Y):</b> ", round(var_pago, 4), "<br><br>",
          "<i>Fórmula de Media E[Y]: (1/\u03bb) * [ e^(-\u03bb*d) - e^(-\u03bb*(d+m)) ]</i><br>"
        ))
      } else {
        esp_pago <- (1/lam) * exp(-lam * d)
        var_pago <- (1/(lam^2)) * exp(-lam * d) * (2 - exp(-lam * d))
        HTML(paste0(
          "<b>Deducible (d):</b> ", d, " | <b>Sin Tope</b><br><hr>",
          "<b>1. Prob. de que la aseguradora pague algo P(X > d):</b> ", round(prob_pago, 4), "<br>",
          "<b>2. Pago Esperado E[Y] (Media del Pago):</b> <span style='color:red; font-size:20px;'><b>", round(esp_pago, 4), "</b></span><br>",
          "<b>3. Mediana del Pago:</b> ", round(mediana_pago_base, 4), "<br>",
          "<b>4. Varianza del Pago Var(Y):</b> ", round(var_pago, 4), "<br><br>",
          "<i>Fórmula de Varianza Var(Y): (1/\u03bb\u00b2) * e^(-\u03bb*d) * (2 - e^(-\u03bb*d))</i>"
        ))
      }
    }
  })
  
  output$plot_calc <- renderPlot({
    lam <- get_lambda(); tipo <- input$tipo_calculo; x_max <- max(qexp(0.99, rate = lam), 10)
    par(mar = c(4, 4, 2, 1), bg = "white")
    curve(dexp(x, rate = lam), from = 0, to = x_max, col = "black", lwd = 2, ylab = "Densidad f(x)", xlab = "x", main = "Área bajo la curva")
    x_vals <- seq(0, x_max, length.out = 500); y_vals <- dexp(x_vals, rate = lam)
    
    if(tipo == "menor") {
      x_lim <- input$val_x
      polygon(c(0, x_vals[x_vals <= x_lim], x_lim), c(0, y_vals[x_vals <= x_lim], 0), col = rgb(0.2, 0.6, 0.8, 0.5), border = NA)
    } else if(tipo == "mayor") {
      x_lim <- input$val_x
      polygon(c(x_lim, x_vals[x_vals >= x_lim], max(x_vals)), c(0, y_vals[x_vals >= x_lim], 0), col = rgb(0.8, 0.3, 0.3, 0.5), border = NA)
    } else if(tipo == "intervalo") {
      a <- input$val_a; b <- input$val_b
      if(a < b) polygon(c(a, x_vals[x_vals >= a & x_vals <= b], b), c(0, y_vals[x_vals >= a & x_vals <= b], 0), col = rgb(0.2, 0.8, 0.2, 0.5), border = NA)
    } else if(tipo == "condicional") {
      t <- input$val_t
      polygon(c(t, x_vals[x_vals >= t], max(x_vals)), c(0, y_vals[x_vals >= t], 0), col = rgb(0.6, 0.2, 0.8, 0.5), border = NA)
      abline(v = t, col = "purple", lwd = 2, lty = 2)
    } else if(tipo == "percentil") {
      p <- input$val_p; x_lim <- qexp(p, rate = lam)
      polygon(c(0, x_vals[x_vals <= x_lim], x_lim), c(0, y_vals[x_vals <= x_lim], 0), col = rgb(0.9, 0.6, 0.2, 0.5), border = NA)
      abline(v = x_lim, col = "darkorange", lwd = 2, lty = 2)
    } else if(tipo == "seguros") {
      d <- input$val_d
      polygon(c(d, x_vals[x_vals >= d], max(x_vals)), c(0, y_vals[x_vals >= d], 0), col = rgb(0.2, 0.7, 0.3, 0.4), border = NA)
      abline(v = d, col = "darkgreen", lwd = 2, lty = 2)
      text(d, max(y_vals)/2, paste("Deducible\n(d =", d, ")"), pos = 4, col = "darkgreen")
      if(input$tiene_tope == "Sí (Tiene límite)") {
        m <- input$val_m
        abline(v = d + m, col = "red", lwd = 2, lty = 2)
        text(d + m, max(y_vals)/3, paste("Tope Máx\n(d+m =", d+m, ")"), pos = 4, col = "red")
      }
    }
  })
}

shinyApp(ui = ui, server = server)
