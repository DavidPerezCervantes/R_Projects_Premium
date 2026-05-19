library(shiny)
library(bslib)

# 1. Interfaz de Usuario (UI)
ui <- fluidPage(
  theme = bs_theme(bootswatch = "sandstone"),
  titlePanel("🏈 Normalidad por Adición: Pasos de Longitud Aleatoria"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Configuración del Experimento"),
      p("Imagina a miles de jugadores en la yarda 50 (el centro). Con cada silbatazo, eligen una dirección al azar y dan un paso de ", strong("longitud aleatoria (entre 0.2 y 3 yardas).")),
      p("El caos individual es absoluto, pero el orden colectivo es perfecto."),
      
      sliderInput("n_personas", "Número de jugadores en el campo:", 
                  min = 100, max = 10000, value = 2000, step = 100),
      
      sliderInput("n_pasos", "Número de silbatazos:", 
                  min = 1, max = 100, value = 16),
      
      hr(),
      h4("Selector de Vista"),
      radioButtons("vista_superior", "Elige qué quieres ver:",
                   choices = list("1. Vista de pájaro (Campo de Fútbol)" = "campo",
                                  "2. Trayectorias en el tiempo (Los Rayos)" = "rayos")),
      
      hr(),
      h4("📊 Resumen y Regla Empírica"),
      div(style = "background-color: #e9ecef; padding: 15px; border-radius: 5px; font-family: monospace;",
          verbatimTextOutput("stats_text")
      )
    ),
    
    mainPanel(
      plotOutput("grafica_superior", height = "280px"),
      plotOutput("density_position", height = "350px")
    )
  )
)

# 2. Lógica del Servidor (Server)
server <- function(input, output, session) {
  
  # MOTOR ACTUALIZADO: Pasos aleatorios continuos entre 0.2 y 3 yardas
  caminata_aleatoria <- reactive({
    set.seed(100) 
    n <- input$n_personas
    pasos <- input$n_pasos
    
    # Por cada persona, sumamos N pasos. 
    # Cada paso = Distancia aleatoria (0.2 a 3) * Dirección aleatoria (+1 o -1)
    posiciones_finales <- replicate(n, sum(runif(pasos, min = 0.2, max = 3) * sample(c(-1, 1), size = pasos, replace = TRUE)))
    return(posiciones_finales)
  })
  
  output$stats_text <- renderPrint({
    pos <- caminata_aleatoria()
    media <- mean(pos)
    desv <- sd(pos)
    
    pct_1sd <- sum(pos >= (media - desv) & pos <= (media + desv)) / length(pos) * 100
    pct_2sd <- sum(pos >= (media - 2*desv) & pos <= (media + 2*desv)) / length(pos) * 100
    
    cat("Centro (Media):   ", round(media, 2), "yardas\n")
    cat("Desv. Estándar:   ", round(desv, 2), "yardas\n")
    cat("---------------------------------\n")
    cat("Jugadores a ±1 SD:", round(pct_1sd, 1), "% (Teoría: 68.3%)\n")
    cat("Jugadores a ±2 SD:", round(pct_2sd, 1), "% (Teoría: 95.4%)")
  })
  
  # --- GRÁFICA SUPERIOR ---
  output$grafica_superior <- renderPlot({
    pasos <- input$n_pasos
    
    # La cámara se ajusta. Como los pasos pueden ser de hasta 3 yardas, 
    # aumentamos un poco el "zoom out" máximo para que no se salgan de la pantalla.
    limite_campo <- max(50, pasos * 1.5) 
    
    if (input$vista_superior == "campo") {
      pos <- caminata_aleatoria()
      par(bg = "#FDFBF7", mar = c(2, 4, 3, 2))
      
      plot(NULL, xlim = c(-limite_campo, limite_campo), ylim = c(0, 100),
           xlab = "", ylab = "", yaxt = "n", bty = "n",
           main = "Vista de pájaro: Campo Expandible")
      
      # Campo verde gigante (para cubrir si hacen zoom out)
      rect(-limite_campo * 2, 0, limite_campo * 2, 100, col = "#2E8B57", border = "white", lwd = 3)
      
      # Zonas de anotación (siempre en la 50 a 60)
      rect(-60, 0, -50, 100, col = "#1a365d", border = "white", lwd = 2)
      rect(50, 0, 60, 100, col = "#1a365d", border = "white", lwd = 2)
      text(-55, 50, "TOUCHDOWN", col = "white", srt = 90, font = 2, cex = 1.5)
      text(55, 50, "TOUCHDOWN", col = "white", srt = 270, font = 2, cex = 1.5)
      
      # Líneas de la cuadrícula
      abline(v = seq(-limite_campo, limite_campo, by = 5), col = rgb(1,1,1, 0.5), lwd = 1)
      abline(v = seq(-limite_campo, limite_campo, by = 10), col = "white", lwd = 3)
      abline(v = 0, col = "#FFD700", lwd = 4) # Centro
      
      # Números de las yardas originales
      yard_x <- seq(-40, 40, by = 10)
      yard_labels <- c("10", "20", "30", "40", "50", "40", "30", "20", "10")
      text(yard_x, 10, yard_labels, col = "white", font = 2, cex = 1.2)
      text(yard_x, 90, yard_labels, col = "white", font = 2, cex = 1.2, srt = 180) 
      
      # Quitamos el jitter artificial porque ahora los datos ya son decimales continuos y naturales
      points(pos, runif(length(pos), 2, 98), pch = 20, col = rgb(1, 0, 0, 0.6), cex = 0.5)
      
    } else {
      # RAYOS
      set.seed(100)
      n_lineas <- 50
      
      # Generamos distancias aleatorias y direcciones aleatorias para el dibujo
      distancias <- matrix(runif(n_lineas * pasos, min = 0.2, max = 3), nrow = pasos, ncol = n_lineas)
      direcciones <- matrix(sample(c(-1, 1), size = n_lineas * pasos, replace = TRUE), nrow = pasos, ncol = n_lineas)
      matriz_pasos <- distancias * direcciones
      
      trayectorias <- apply(matriz_pasos, 2, cumsum)
      trayectorias <- rbind(0, trayectorias)
      
      par(bg = "#FDFBF7", mar = c(4, 4, 3, 2))
      matplot(0:pasos, trayectorias, type = "l", lty = 1, lwd = 1.5,
              col = rgb(0.2, 0.4, 0.7, 0.5), 
              xlim = c(0, pasos), ylim = c(-limite_campo, limite_campo), 
              xlab = "Número de silbatazo (Paso)", ylab = "Posición en yardas",
              main = paste("Evolución en el tiempo hasta", pasos, "pasos (Longitudes aleatorias)"),
              bty = "n")
      abline(h = 0, col = "#FFD700", lty = 2, lwd = 3) 
    }
  })
  
  # --- GRÁFICA INFERIOR ---
  output$density_position <- renderPlot({
    pos <- caminata_aleatoria()
    limite_campo <- max(50, input$n_pasos * 1.5)
    media <- mean(pos)
    desv <- sd(pos)
    
    par(bg = "#FDFBF7", mar = c(4, 4, 2, 2))
    
    d <- density(pos) 
    
    plot(d, xlim = c(-limite_campo, limite_campo),
         main = "Densidad vs. Posición",
         xlab = "Distancia desde la Yarda 50 (Yardas)", ylab = "Densidad",
         col = "#3b5b92", lwd = 4, bty = "n", zero.line = FALSE)
    
    polygon(d, col = rgb(0.2, 0.4, 0.7, 0.2), border = NA)
    
    x_1sd <- d$x[d$x >= (media - desv) & d$x <= (media + desv)]
    y_1sd <- d$y[d$x >= (media - desv) & d$x <= (media + desv)]
    polygon(c(x_1sd, rev(x_1sd)), c(rep(0, length(x_1sd)), rev(y_1sd)), 
            col = rgb(0.13, 0.55, 0.13, 0.4), border = NA)
    
    abline(v = 0, col = "gray", lty = 2, lwd = 2) 
    abline(v = c(media - desv, media + desv), col = "darkgreen", lty = 3, lwd = 2)
    
    x_teorico <- seq(-limite_campo, limite_campo, length.out = 200)
    y_teorico <- dnorm(x_teorico, mean = media, sd = desv)
    lines(x_teorico, y_teorico, col = "#222222", lwd = 3, lty = 2)
    
    texto_altura <- max(d$y) / 2
    text(media, texto_altura, "Aprox. 68%\nde jugadores", col = "darkgreen", font = 2)
  })
}

# 3. Ejecutar
shinyApp(ui = ui, server = server)
