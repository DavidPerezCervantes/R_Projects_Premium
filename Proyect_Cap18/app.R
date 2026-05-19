# ==========================================
# 0. LIBRERÍAS (Asegúrate de tenerlas instaladas)
# install.packages(c("shiny", "shinythemes", "plotly", "ggplot2", "MASS"))
# ==========================================

library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(MASS)

# ==========================================
# 1. GENERACIÓN DE DATOS BASE (SATT)
# ==========================================
set.seed(42)
n <- 50
prcnt_take <- runif(n, 5, 85)
spend <- 4 + 0.05 * prcnt_take + rnorm(n, 0, 0.5)

# La verdad: Gasto (+) y Participación (-)
sat_score <- 1050 + 16 * spend - 2.9 * prcnt_take + rnorm(n, 0, 15)

# Basura correlacionada para Pestaña 3
ratio_alumnos <- 30 - 0.5 * prcnt_take + rnorm(n, 0, 2)  
salario_profes <- 20 + 3 * spend + rnorm(n, 0, 2)         
inasistencia <- 15 + 0.2 * prcnt_take + rnorm(n, 0, 2)    

datos_master <- data.frame(
  SAT = sat_score, Gasto = spend, Participacion = prcnt_take,
  Ratio_Alumnos = ratio_alumnos, Salario_Docente = salario_profes, Inasistencia = inasistencia
)

# ==========================================
# 2. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- navbarPage(
  theme = shinytheme("darkly"),
  title = "Masterclass Bayesiana: Cap. 18 (Kruschke)",
  
  # --- PESTAÑAS 1 A 4 ---
  tabPanel("1. La Paradoja",
           fluidRow(
             column(12, wellPanel(
               style = "background-color: #1a1a1a; border: 1px solid #00bc8c;",
               h3("Comparativa: ¿El dinero realmente empeora el SAT?", style="color:#00bc8c;"),
               p("Izquierda: El error de omitir variables. Parece que gastar 9k da peores notas que gastar 4k."),
               p("Derecha: La corrección Bayesiana en 3D revela que el gasto sí ayuda si controlamos la participación.")
             ))
           ),
           fluidRow(
             column(5, h4("Regresión Simple (Vista Sesgada)", align="center"), plotOutput("sec1_2d", height = "550px")),
             column(7, h4("Regresión Múltiple (Vista Correcta)", align="center"), plotlyOutput("sec1_3d", height = "550px"))
           )
  ),
  tabPanel("2. Alabeo (Interacción)",
           sidebarLayout(
             sidebarPanel(
               width = 3, h3("Interacción (\\(\\beta_3\\))"),
               sliderInput("sec2_int", "Fuerza del Alabeo:", -1.5, 1.5, 0, 0.1), hr(),
               p("Al aumentar el slider, 'tuercas' el plano. El impacto del Gasto depende de la Participación."),
               withMathJax(), p("$$y = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\mathbf{\\beta_3 (x_1 x_2)}$$")
             ),
             mainPanel(width = 9, plotlyOutput("sec2_plot", height = "650px"))
           )
  ),
  tabPanel("3. Encogimiento (Shrinkage)",
           fluidRow(
             column(12, wellPanel(
               h3("El Efecto de los Priors Jerárquicos"),
               p("Mueve el slider drásticamente hasta 1000. La basura matemática es borrada de la ecuación."),
               sliderInput("sec3_lambda", "Escepticismo del Modelo:", 0, 1000, 0, 20)
             ))
           ),
           fluidRow(column(12, plotOutput("sec3_plot", height = "750px")))
  ),
  tabPanel("4. Selección Causal",
           sidebarLayout(
             sidebarPanel(
               width = 3, h3("Inclusión (PIP)"),
               sliderInput("sec4_ruido", "Inyectar Ruido:", 0, 5, 0, 1), hr(),
               p("Verde: Alta probabilidad de ser causal. Rojo: Ruido detectado y 'apagado'.")
             ),
             mainPanel(width = 9, plotOutput("sec4_plot", height = "650px"))
           )
  ),
  
  # --- PESTAÑA 5: SIMULADOR DE RECORTES (EL JAQUE MATE) ---
  tabPanel("5. Simulador de Recortes",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h3("Decisión Ejecutiva"),
               p("La pregunta clave: 'Si los que gastan menos tienen mejores notas, ¿debemos recortar el presupuesto?'"),
               hr(),
               sliderInput("sec5_recorte", "Propuesta de Recorte por Alumno:", 
                           min = -5, max = 0, value = 0, step = 0.5, 
                           pre = "$", post = "k"),
               hr(),
               checkboxInput("sec5_espejismo", "Inyectar Trampa: Estado Espejismo", FALSE),
               helpText("Simula un estado que recortó todos sus fondos ($0), pero 10 genios tomaron el examen y sacaron 1600 de calificación perfecta.")
             ),
             mainPanel(
               width = 9,
               fluidRow(
                 column(6, wellPanel(
                   style = "background-color: #2c2c2c; border-left: 5px solid #ff3333; text-align: center;",
                   h4("Falsa Promesa (Regresión Simple)"),
                   uiOutput("kpi_falso")
                 )),
                 column(6, wellPanel(
                   style = "background-color: #2c2c2c; border-left: 5px solid #00bc8c; text-align: center;",
                   h4("Impacto Real (Múltiple Bayesiano)"),
                   uiOutput("kpi_real")
                 ))
               ),
               fluidRow(
                 column(12, plotOutput("sec5_plot", height = "500px"))
               )
             )
           )
  )
)

# ==========================================
# 3. LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output) {
  
  # --- Lógica Pestañas 1 a 4 ---
  output$sec1_2d <- renderPlot({ ggplot(datos_master, aes(x = Gasto, y = SAT)) + geom_point(color = "#00bc8c", size = 4, alpha = 0.6) + geom_smooth(method = "lm", color = "#e74c3c", size = 2, linetype="dashed") + theme_dark() + theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333")) })
  output$sec1_3d <- renderPlotly({ mod <- lm(SAT ~ Gasto + Participacion, data = datos_master); g <- seq(min(datos_master$Gasto), max(datos_master$Gasto), length.out = 15); p <- seq(min(datos_master$Participacion), max(datos_master$Participacion), length.out = 15); m <- expand.grid(Gasto = g, Participacion = p); m$SAT <- predict(mod, newdata = m); z <- matrix(m$SAT, nrow = 15); plot_ly(datos_master, x = ~Gasto, y = ~Participacion, z = ~SAT) %>% add_markers(marker = list(color = "#00bc8c", size = 5)) %>% add_surface(x = g, y = p, z = z, opacity = 0.6, colorscale = "Viridis", showscale = F) %>% layout(scene = list(xaxis = list(title = 'Gasto'), yaxis = list(title = 'Part.'), zaxis = list(title = 'SAT')), paper_bgcolor = '#222222', font = list(color = 'white')) })
  output$sec2_plot <- renderPlotly({ g <- seq(4, 10, length.out = 20); p <- seq(5, 85, length.out = 20); m <- expand.grid(Gasto = g, Participacion = p); m$SAT <- 1050 + 15*m$Gasto - 3*m$Participacion + input$sec2_int*(m$Gasto*m$Participacion); z <- matrix(m$SAT, nrow = 20); plot_ly(x = g, y = p, z = z) %>% add_surface(colorscale = "Plasma", showscale = F) %>% layout(scene = list(xaxis = list(title = 'Gasto'), yaxis = list(title = 'Part.'), zaxis = list(title = 'SAT')), paper_bgcolor = '#222222', font = list(color = 'white')) })
  output$sec3_plot <- renderPlot({ df_esc <- as.data.frame(scale(datos_master)); mod_c <- lm(SAT ~ ., data = df_esc); co <- coef(mod_c)[-1]; ev <- summary(mod_c)$coefficients[-1, 3]; surv <- 1 / (1 + (input$sec3_lambda/20) * (1 / abs(ev)^4)); co_b <- co * surv; df <- data.frame(Var = names(co_b), Val = as.numeric(co_b)); ggplot(df, aes(x = reorder(Var, -abs(Val)), y = Val, fill = Var)) + geom_bar(stat = "identity") + geom_hline(yintercept = 0, color = "white", size=1.5) + coord_cartesian(ylim = c(-0.8, 0.8)) + theme_dark() + theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), legend.position="none", text=element_text(size=20, color="white")) })
  output$sec4_plot <- renderPlot({ df_s <- datos_master; if(input$sec4_ruido>0) for(i in 1:input$sec4_ruido) df_s[[paste0("Basura_", i)]] <- rnorm(50); mod <- lm(SAT ~ ., data = df_s); pip <- plogis(abs(summary(mod)$coefficients[-1, 3]) * 2 - 3); df_p <- data.frame(Var = names(pip), PIP = pip); ggplot(df_p, aes(x = reorder(Var, -PIP), y = PIP, fill = PIP > 0.5)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("TRUE"="#00bc8c", "FALSE"="#e74c3c")) + theme_dark() + theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text=element_text(size=18, color="white"), legend.position="none") })
  
  # --- LÓGICA PESTAÑA 5: SIMULADOR DE RECORTES ---
  df_sec5 <- reactive({
    df <- datos_master
    if(input$sec5_espejismo) {
      # Estado Espejismo: Gasto $0, 1% participación, 1600 puntos perfectos
      df <- rbind(df, data.frame(SAT = 1600, Gasto = 0, Participacion = 1, 
                                 Ratio_Alumnos=30, Salario_Docente=20, Inasistencia=15))
    }
    df
  })
  
  modelos_sec5 <- reactive({
    df <- df_sec5()
    mod_simple <- lm(SAT ~ Gasto, data = df)
    mod_robusto <- rlm(SAT ~ Gasto + Participacion, data = df)
    list(simple = mod_simple, robusto = mod_robusto)
  })
  
  # Tarjeta Roja (Falsa Promesa)
  output$kpi_falso <- renderUI({
    mods <- modelos_sec5()
    recorte <- input$sec5_recorte
    impacto_puntos <- coef(mods$simple)["Gasto"] * recorte 
    ahorro_millones <- abs(recorte) * 100 
    
    div(
      span(style="color: #ff3333; font-size: 28px; font-weight: bold;", sprintf("%+d Puntos SAT", round(impacto_puntos))),
      br(),
      span(style="color: white; font-size: 16px;", sprintf("Al ahorrar $%.0f Millones.", ahorro_millones)),
      p(style="color: gray; font-size: 12px; margin-top:5px;", "¿Ves? ¡Parece una idea brillante!")
    )
  })
  
  # Tarjeta Verde (Realidad)
  output$kpi_real <- renderUI({
    mods <- modelos_sec5()
    recorte <- input$sec5_recorte
    impacto_puntos <- coef(mods$robusto)["Gasto"] * recorte 
    ahorro_millones <- abs(recorte) * 100
    
    div(
      span(style="color: #00bc8c; font-size: 28px; font-weight: bold;", sprintf("%+d Puntos SAT", round(impacto_puntos))),
      br(),
      span(style="color: white; font-size: 16px;", sprintf("Al ahorrar $%.0f Millones.", ahorro_millones)),
      p(style="color: gray; font-size: 12px; margin-top:5px;", "¡Estás arruinando la educación del estado!")
    )
  })
  
  # Gráfico del Efecto Ascensor con Etiqueta Dinámica
  output$sec5_plot <- renderPlot({
    df <- df_sec5()
    mods <- modelos_sec5()
    recorte <- input$sec5_recorte
    gasto_promedio <- mean(df$Gasto)
    
    int_simple <- coef(mods$simple)[1] + coef(mods$simple)["Gasto"] * (gasto_promedio + recorte)
    int_robusto <- coef(mods$robusto)[1] + coef(mods$robusto)["Gasto"] * (gasto_promedio + recorte)
    pendiente_robusta <- coef(mods$robusto)["Participacion"]
    
    p <- ggplot(df, aes(x = Participacion, y = SAT)) +
      geom_point(aes(color = Gasto == 0, size = Gasto == 0), alpha = 0.5) +
      scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "#ff3333")) +
      scale_size_manual(values = c("FALSE" = 3, "TRUE" = 8)) +
      geom_hline(yintercept = int_simple, color = "#ff3333", size = 2, linetype = "dashed") +
      geom_abline(intercept = int_robusto, slope = pendiente_robusta, color = "#00bc8c", size = 2) +
      theme_dark() +
      # Ampliamos el eje Y para que el punto de 1600 se vea perfecto
      coord_cartesian(ylim = c(800, 1650)) +
      labs(x = "% de Alumnos que toman el SAT (Participación)", y = "Puntaje Promedio del Estado",
           title = "El Gasto como 'Impulso' Vertical (Mueve el Slider de Recorte)",
           subtitle = "Línea Verde (Verdad) = El recorte hunde el SAT | Línea Roja (Falsa) = El recorte 'mejora' el SAT") +
      theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"),
            text = element_text(color = "white", size = 16), legend.position = "none")
    
    # Si la trampa está activa, añadimos la etiqueta de texto flotante
    if(input$sec5_espejismo) {
      p <- p + annotate("text", x = 3, y = 1600, label = "Trampa: 10 Genios", 
                        color = "#ff3333", fontface = "bold", size = 6, hjust = 0)
    }
    
    p
  })
}

shinyApp(ui = ui, server = server)
