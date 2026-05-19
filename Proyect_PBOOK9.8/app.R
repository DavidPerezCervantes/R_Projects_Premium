library(shiny)
library(bslib)
library(ggplot2)

# ==========================================
# 1. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- navbarPage(
  title = "🌳 Laboratorio de Probabilidad: Secciones 5.2 y 5.3",
  theme = bs_theme(bootswatch = "lumen", primary = "#2c3e50"),
  
  tabPanel("Solucionador de Bayes (2 Escenarios)",
           fluidPage(
             withMathJax(), # Inicializa la librería matemática para renderizar fórmulas
             
             sidebarLayout(
               sidebarPanel(
                 h4("1. Extraer Datos del Problema"),
                 p("Introduce los datos de tus ejercicios del Pbook2020 aquí."),
                 
                 h5("Probabilidades A Priori (Raíz)"),
                 sliderInput("pA", "P(A) - Probabilidad inicial del Evento A:", 
                             min = 0, max = 1, value = 0.40, step = 0.01),
                 div(style = "color: #7f8c8d; font-size: 12px;", textOutput("txt_complemento_A")),
                 
                 hr(),
                 h5("Probabilidades Condicionales (Ramas)"),
                 sliderInput("pB_A", "P(B|A) - Prob. de B dado que ocurrió A:", 
                             min = 0, max = 1, value = 0.85, step = 0.01),
                 sliderInput("pB_Ac", "P(B|A\u0304) - Prob. de B dado que ocurrió A\u0304:", 
                             min = 0, max = 1, value = 0.15, step = 0.01),
                 
                 hr(),
                 div(style = "background-color: #fcf3cf; padding: 15px; border-radius: 5px; font-size: 13px;",
                     strong("Notación Actuarial:"),
                     p("En muchos libros, A\u0304 (A barra) o A^c representa el complemento de A (Lo que falta para llegar a 1).")
                 )
               ),
               
               mainPanel(
                 h3("1. Diagrama de Árbol (Regla de la Multiplicación)"),
                 p("Las probabilidades en los cuadros grises finales (Hojas) representan la probabilidad conjunta: P(A \u2229 B). Son el resultado de multiplicar las ramas que te llevan hasta ahí."),
                 plotOutput("plot_arbol", height = "350px"),
                 
                 hr(),
                 h3("2. Ley de la Probabilidad Total (Sec. 5.2)"),
                 p("Para encontrar la probabilidad total de que ocurra B, sumamos los caminos finales del árbol que terminan en B."),
                 uiOutput("formula_total"),
                 
                 hr(),
                 h3("3. Teorema de Bayes (Sec. 5.3)"),
                 p("Si sabemos que B ya ocurrió, ¿cuál es la probabilidad de que haya provenido de la rama A?"),
                 uiOutput("formula_bayes")
               )
             )
           )
  )
)

# ==========================================
# 2. LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # Texto de ayuda dinámico en el panel lateral
  output$txt_complemento_A <- renderText({
    paste0("Automático: P(A\u0304) = 1 - ", input$pA, " = ", 1 - input$pA)
  })
  
  # Motor de dibujo del Diagrama de Árbol usando ggplot2
  output$plot_arbol <- renderPlot({
    pA <- input$pA
    pAc <- 1 - pA
    pB_A <- input$pB_A
    pBc_A <- 1 - pB_A
    pB_Ac <- input$pB_Ac
    pBc_Ac <- 1 - pB_Ac
    
    # Coordenadas de los Nodos (Cajas)
    nodos <- data.frame(
      x = c(0, 1, 1, 2.2, 2.2, 2.2, 2.2),
      y = c(0.5, 0.8, 0.2, 0.95, 0.65, 0.35, 0.05),
      label = c("Raíz", 
                paste0("Evento A\n(", pA, ")"), 
                paste0("Evento A\u0304\n(", pAc, ")"),
                paste0("B\nP(A\u2229B) = ", round(pA * pB_A, 4)), 
                paste0("B\u0304\nP(A\u2229B\u0304) = ", round(pA * pBc_A, 4)),
                paste0("B\nP(A\u0304\u2229B) = ", round(pAc * pB_Ac, 4)), 
                paste0("B\u0304\nP(A\u0304\u2229B\u0304) = ", round(pAc * pBc_Ac, 4)))
    )
    
    # Coordenadas de las Ramas (Líneas)
    ramas <- data.frame(
      x1 = c(0, 0, 1, 1, 1, 1),
      y1 = c(0.5, 0.5, 0.8, 0.8, 0.2, 0.2),
      x2 = c(1, 1, 2.2, 2.2, 2.2, 2.2),
      y2 = c(0.8, 0.2, 0.95, 0.65, 0.35, 0.05),
      label = c(paste0("P(A) = ", pA), 
                paste0("P(A\u0304) = ", pAc),
                paste0("P(B|A) = ", pB_A), 
                paste0("P(B\u0304|A) = ", pBc_A),
                paste0("P(B|A\u0304) = ", pB_Ac), 
                paste0("P(B\u0304|A\u0304) = ", pBc_Ac))
    )
    
    # Dibujo del lienzo
    ggplot() +
      # Dibujar las líneas (Ramas)
      geom_segment(data = ramas, aes(x=x1, y=y1, xend=x2, yend=y2), color="#7f8c8d", linewidth=1) +
      # Etiquetas sobre las líneas
      geom_label(data = ramas, aes(x=(x1+x2)/2, y=(y1+y2)/2 + 0.04, label=label), 
                 size=4, color="#c0392b", fontface="bold", fill="white", label.size=0) +
      # Cajas de los nodos
      geom_label(data = nodos, aes(x=x, y=y, label=label), 
                 size=5, fill="#ecf0f1", color="#2c3e50", fontface="bold", 
                 label.padding = unit(0.6, "lines"), label.r = unit(0.3, "lines")) +
      theme_void() +
      xlim(-0.2, 2.5) + ylim(0, 1)
  })
  
  # Generador Dinámico de la Fórmula de Probabilidad Total
  output$formula_total <- renderUI({
    pA <- input$pA
    pAc <- 1 - pA
    pB_A <- input$pB_A
    pB_Ac <- input$pB_Ac
    
    total_B <- (pA * pB_A) + (pAc * pB_Ac)
    
    # Renderizamos las ecuaciones usando MathJax para que se vean como libro de texto
    withMathJax(
      HTML(paste0(
        "$$P(B) = P(A) \\cdot P(B|A) + P(\\bar{A}) \\cdot P(B|\\bar{A})$$",
        "$$P(B) = (", pA, ")(", pB_A, ") + (", pAc, ")(", pB_Ac, ")$$",
        "$$P(B) = ", round(pA * pB_A, 4), " + ", round(pAc * pB_Ac, 4), " = \\mathbf{", round(total_B, 4), "}$$"
      ))
    )
  })
  
  # Generador Dinámico de la Fórmula de Bayes
  output$formula_bayes <- renderUI({
    pA <- input$pA
    pAc <- 1 - pA
    pB_A <- input$pB_A
    pB_Ac <- input$pB_Ac
    
    total_B <- (pA * pB_A) + (pAc * pB_Ac)
    bayes_res <- (pA * pB_A) / total_B
    
    withMathJax(
      HTML(paste0(
        "$$P(A|B) = \\frac{P(A \\cap B)}{P(B)} = \\frac{P(A) \\cdot P(B|A)}{P(B)}$$",
        "$$P(A|B) = \\frac{(", pA, ")(", pB_A, ")}{", round(total_B, 4), "}$$",
        "$$P(A|B) = \\mathbf{", round(bayes_res, 4), "}$$"
      ))
    )
  })
}

shinyApp(ui = ui, server = server)
