# =====================================================================
# DASHBOARD ECONOMÉTRICO FINANCIERO BMV
# VERSIÓN MEJORADA — DATOS DIARIOS 2019-2026
# INCLUYE: Series, Johansen, VAR, Simulador de Desplome
# =====================================================================

rm(list = ls())
options(timeout = 1200)
options(scipen = 999)
options(warn = -1)

# =====================================================================
# PAQUETES
# =====================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(quantmod)
library(tidyverse)
library(xts)
library(zoo)
library(vars)
library(urca)
library(tseries)
library(forecast)
library(lmtest)
library(plotly)
library(corrplot)
library(PerformanceAnalytics)
library(TTR)
library(DT)
library(scales)
library(e1071)

# =====================================================================
# CARGA DE DATOS PRE-DESCARGADOS (DIARIOS 2019-2026)
# =====================================================================
# Se cargan de forma local para evitar errores de CORS/Red en el navegador (WebAssembly)
load("data.RData")

# =====================================================================
# SERIES DE PRECIOS AJUSTADOS (DIARIOS)
# =====================================================================

amx     <- Ad(AMXB.MX)
banorte <- Ad(GFNORTEO.MX)
walmex  <- Ad(WALMEX.MX)
bimbo   <- Ad(BIMBOA.MX)
femsa   <- Ad(FEMSAUBD.MX)
cemex   <- Ad(CEMEXCPO.MX)
ipc     <- Ad(ipc_raw)

# =====================================================================
# BASE DIARIA — PRECIOS ORIGINALES
# =====================================================================

precios_diarios <- na.omit(merge(
  amx, banorte, walmex, bimbo, femsa, cemex, ipc
))

colnames(precios_diarios) <- c(
  "AMX", "BANORTE", "WALMEX", "BIMBO", "FEMSA", "CEMEX", "IPC"
)

# =====================================================================
# LOG Y DIFERENCIAS DIARIAS
# =====================================================================

log_precios <- log(precios_diarios)
colnames(log_precios) <- colnames(precios_diarios)

dlog_diario <- na.omit(diff(log_precios))
colnames(dlog_diario) <- colnames(log_precios)

# Rendimientos acumulados base 100
rendimiento_acum <- function(serie) {
  100 * exp(cumsum(na.omit(diff(log(serie)))))
}

# Data frame de precios para plotly
precios_df <- data.frame(
  fecha = index(precios_diarios),
  coredata(precios_diarios)
)

log_df <- data.frame(
  fecha = index(log_precios),
  coredata(log_precios)
)

dlog_df <- data.frame(
  fecha = index(dlog_diario),
  coredata(dlog_diario)
)

acciones <- c("AMX", "BANORTE", "WALMEX", "BIMBO", "FEMSA", "CEMEX")

# =====================================================================
# MATRICES PARA JOHANSEN Y VAR (log de precios)
# =====================================================================

mat_log    <- coredata(log_precios)
mat_dlog   <- coredata(dlog_diario)

# =====================================================================
# UI
# =====================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      icon("chart-line"),
      " Econometría Financiera BMV"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploratorio",        tabName = "explore",      icon = icon("chart-line")),
      menuItem("Series Originales",   tabName = "series_tab",   icon = icon("eye")),
      menuItem("Estacionariedad",     tabName = "stationary",   icon = icon("wave-square")),
      menuItem("Cointegración",       tabName = "cointegration",icon = icon("project-diagram")),
      menuItem("VAR y Rezagos",       tabName = "var",          icon = icon("chart-bar")),
      menuItem("IRF y FEVD",          tabName = "irf",          icon = icon("sync")),
      menuItem("MCO",                 tabName = "ols",          icon = icon("calculator")),
      menuItem("Pronóstico",          tabName = "forecast_tab", icon = icon("chart-area")),
      menuItem("Comparativo",         tabName = "compare",      icon = icon("table")),
      menuItem("🔴 Simulador",         tabName = "simulator",    icon = icon("bomb"))
    ),
    
    br(),
    
    selectInput(
      "accion",
      "Selecciona Acción",
      choices = acciones,
      selected = "AMX"
    ),
    
    sliderInput(
      "lags",
      "Rezagos VAR",
      min = 1, max = 12, value = 2
    ),
    
    hr(),
    
    tags$div(
      style = "padding: 10px; color: #bbb; font-size: 12px;",
      tags$b("Datos:"), " Diarios 2019–2026", br(),
      tags$b("Fuente:"), " Yahoo Finance"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;600&display=swap"
      ),
      tags$style(HTML("
        body, .content-wrapper {
          background-color: #0f1117 !important;
          font-family: 'IBM Plex Sans', sans-serif;
          color: #e0e0e0;
        }
        .skin-blue .main-header .logo,
        .skin-blue .main-header .navbar {
          background-color: #1a1f2e !important;
          border-bottom: 2px solid #2563eb;
        }
        .skin-blue .main-sidebar {
          background-color: #0d1117 !important;
        }
        .skin-blue .sidebar-menu > li > a {
          color: #c9d1d9 !important;
          border-left: 3px solid transparent;
          transition: all 0.2s;
        }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li > a:hover {
          background-color: #1c2333 !important;
          border-left: 3px solid #2563eb !important;
          color: #ffffff !important;
        }
        .box {
          background-color: #161b27 !important;
          border: 1px solid #21273d !important;
          border-radius: 8px !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.4) !important;
          color: #e0e0e0 !important;
        }
        .box-header {
          background-color: #1c2333 !important;
          border-bottom: 1px solid #21273d !important;
          border-radius: 8px 8px 0 0 !important;
          color: #ffffff !important;
        }
        .box-title {
          font-family: 'IBM Plex Mono', monospace !important;
          font-size: 13px !important;
          letter-spacing: 0.05em;
          color: #c9d1d9 !important;
        }
        .value-box {
          border-radius: 8px !important;
          box-shadow: 0 4px 15px rgba(0,0,0,0.4) !important;
        }
        .texto {
          font-size: 13.5px;
          line-height: 1.9;
          padding: 10px 14px;
          color: #b0bec5;
          border-left: 3px solid #2563eb;
          background-color: #0d1117;
          border-radius: 0 6px 6px 0;
          margin-top: 10px;
        }
        pre {
          background-color: #0d1117 !important;
          color: #a8d8a8 !important;
          font-family: 'IBM Plex Mono', monospace !important;
          font-size: 11.5px !important;
          border: 1px solid #21273d !important;
          border-radius: 6px !important;
          padding: 12px !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active > a {
          border-top: 3px solid #2563eb !important;
        }
        .sim-card {
          background: linear-gradient(135deg, #1c2333 0%, #0d1117 100%);
          border: 1px solid #21273d;
          border-radius: 10px;
          padding: 15px;
          margin-bottom: 10px;
        }
        .sim-shock-label {
          font-family: 'IBM Plex Mono', monospace;
          color: #ef4444;
          font-size: 14px;
          font-weight: 600;
        }
        .impacto-positivo { color: #22c55e; font-weight: 600; }
        .impacto-negativo { color: #ef4444; font-weight: 600; }
        .impacto-neutral  { color: #f59e0b; font-weight: 600; }
        hr { border-color: #21273d !important; }
        .selectize-input, .selectize-dropdown {
          background-color: #1c2333 !important;
          color: #e0e0e0 !important;
          border: 1px solid #21273d !important;
        }
        .irs-bar, .irs-bar-edge { background: #2563eb !important; border-color: #2563eb !important; }
        .irs-handle > i { background: #2563eb !important; }
        label { color: #b0bec5 !important; }
      "))
    ),
    
    tabItems(
      
      # =====================================================================
      # EXPLORATORIO
      # =====================================================================
      
      tabItem(
        tabName = "explore",
        
        fluidRow(
          valueBoxOutput("media_box"),
          valueBoxOutput("vol_box"),
          valueBoxOutput("corr_box"),
          valueBoxOutput("sharpe_box")
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Precios Ajustados — Diarios 2019–2026",
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(9,
                     tags$div(
                       style = "padding: 4px 10px 0 10px; font-size: 13px; color: #b0bec5;",
                       tags$b("Filtrar series:"),
                       checkboxGroupInput(
                         "explore_series",
                         label = NULL,
                         choices  = c("AMX","BANORTE","WALMEX","BIMBO","FEMSA","CEMEX","IPC"),
                         selected = c("AMX","BANORTE","WALMEX","BIMBO","FEMSA","CEMEX"),
                         inline   = TRUE
                       )
                     )
              ),
              column(3,
                     tags$div(
                       style = "padding: 10px 10px 0 0; text-align: right;",
                       actionButton(
                         "explore_toggle_ipc",
                         "Toggle IPC",
                         class = "btn btn-xs btn-default",
                         style = "font-size:11px; background:#1c2333; color:#c9d1d9; border:1px solid #2563eb;"
                       )
                     )
              )
            ),
            plotlyOutput("plot_precios", height = 420),
            br(),
            htmlOutput("explore_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Rendimientos de la Acción Seleccionada",
            status = "info", solidHeader = TRUE,
            plotlyOutput("returns_plot_ly", height = 300),
            br(),
            htmlOutput("returns_text")
          ),
          box(
            width = 6,
            title = "Correlaciones entre Activos (Rendimientos Log)",
            status = "warning", solidHeader = TRUE,
            plotOutput("corr_plot", height = 300),
            br(),
            htmlOutput("corr_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Rendimientos Acumulados Base 100",
            status = "success", solidHeader = TRUE,
            tags$div(
              style = "padding: 4px 10px 0 10px; font-size: 13px; color: #b0bec5;",
              tags$b("Incluir IPC en acumulados:"),
              checkboxInput("acum_show_ipc", "Mostrar IPC", value = FALSE)
            ),
            plotlyOutput("acum_plot", height = 360)
          )
        )
        
      ),
      
      # =====================================================================
      # SERIES ORIGINALES
      # =====================================================================
      
      tabItem(
        tabName = "series_tab",
        
        fluidRow(
          box(
            width = 12,
            title = "📊 Series en Niveles Logarítmicos — ¿Necesitan Diferenciarse?",
            status = "primary", solidHeader = TRUE,
            plotlyOutput("series_niveles_plot", height = 420),
            br(),
            htmlOutput("series_niveles_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "📈 Series Diferenciadas (Rendimientos Log Diarios)",
            status = "warning", solidHeader = TRUE,
            plotlyOutput("series_diff_plot_all", height = 420),
            br(),
            htmlOutput("series_diff_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "🔬 Diagnóstico de Estacionariedad — Tabla Resumen ADF para Todas las Series",
            status = "success", solidHeader = TRUE,
            DT::dataTableOutput("tabla_adf_global"),
            br(),
            htmlOutput("tabla_adf_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Volatilidad Rodante 30 días — Acción Seleccionada",
            status = "info", solidHeader = TRUE,
            plotlyOutput("vol_rodante_plot", height = 300),
            htmlOutput("vol_rodante_text")
          ),
          box(
            width = 6,
            title = "Distribución de Rendimientos Diarios",
            status = "danger", solidHeader = TRUE,
            plotlyOutput("dist_returns_plot", height = 300),
            htmlOutput("dist_returns_text")
          )
        )
        
      ),
      
      # =====================================================================
      # ESTACIONARIEDAD
      # =====================================================================
      
      tabItem(
        tabName = "stationary",
        
        fluidRow(
          box(
            width = 6,
            title = "Serie Original (Log)",
            status = "primary", solidHeader = TRUE,
            plotOutput("serie_original_plot", height = 280),
            br(),
            htmlOutput("serie_original_text")
          ),
          box(
            width = 6,
            title = "Serie Diferenciada (Log-Diff)",
            status = "warning", solidHeader = TRUE,
            plotOutput("serie_diff_plot", height = 280),
            br(),
            htmlOutput("serie_diff_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Tendencia con Media Móvil (60, 120, 252 días)",
            status = "success", solidHeader = TRUE,
            plotlyOutput("trend_plot", height = 380),
            br(),
            htmlOutput("trend_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Test ADF (Augmented Dickey-Fuller)",
            status = "primary", solidHeader = TRUE,
            verbatimTextOutput("adf_output"),
            br(),
            htmlOutput("adf_text")
          ),
          box(
            width = 6,
            title = "Test KPSS",
            status = "danger", solidHeader = TRUE,
            verbatimTextOutput("kpss_output"),
            br(),
            htmlOutput("kpss_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "ACF — Función de Autocorrelación",
            status = "info", solidHeader = TRUE,
            plotOutput("acf_plot", height = 280),
            br(),
            htmlOutput("acf_text")
          ),
          box(
            width = 6,
            title = "PACF — Autocorrelación Parcial",
            status = "warning", solidHeader = TRUE,
            plotOutput("pacf_plot", height = 280),
            br(),
            htmlOutput("pacf_text")
          )
        )
        
      ),
      
      # =====================================================================
      # COINTEGRACIÓN
      # =====================================================================
      
      tabItem(
        tabName = "cointegration",
        
        fluidRow(
          box(
            width = 12,
            title = "📐 Criterio de Johansen — Test de Traza y Máximo Eigenvalor",
            status = "success", solidHeader = TRUE,
            
            fluidRow(
              column(
                4,
                selectInput(
                  "johansen_type",
                  "Tipo de Test",
                  choices = c("Traza" = "trace", "Eigenvalor Máximo" = "eigen"),
                  selected = "trace"
                )
              ),
              column(
                4,
                selectInput(
                  "johansen_ecdet",
                  "Determinística",
                  choices = c("Ninguna" = "none", "Constante" = "const", "Tendencia" = "trend"),
                  selected = "const"
                )
              ),
              column(
                4,
                selectInput(
                  "johansen_vars",
                  "Variables a incluir",
                  choices = c("Todas" = "todas", "Sin INPC" = "sin_inpc"),
                  selected = "todas"
                )
              )
            ),
            
            verbatimTextOutput("johansen_output"),
            br(),
            htmlOutput("johansen_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Valores Propios (Eigenvalues)",
            status = "info", solidHeader = TRUE,
            verbatimTextOutput("johansen_eigen_output"),
            br(),
            htmlOutput("johansen_eigen_text")
          ),
          box(
            width = 6,
            title = "Vectores Cointegrantes (VECM)",
            status = "warning", solidHeader = TRUE,
            verbatimTextOutput("johansen_vec_output"),
            br(),
            htmlOutput("johansen_vec_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Relación de Largo Plazo — Residuos del Vector Cointegrante",
            status = "primary", solidHeader = TRUE,
            plotlyOutput("johansen_residuos_plot", height = 350),
            br(),
            htmlOutput("johansen_residuos_text")
          )
        )
        
      ),
      
      # =====================================================================
      # VAR Y REZAGOS
      # =====================================================================
      
      tabItem(
        tabName = "var",
        
        fluidRow(
          box(
            width = 12,
            title = "🔢 Selección Óptima de Rezagos — Criterios de Información",
            status = "primary", solidHeader = TRUE,
            
            fluidRow(
              column(
                6,
                verbatimTextOutput("lag_output")
              ),
              column(
                6,
                plotlyOutput("lag_criterios_plot", height = 650)
              )
            ),
            
            br(),
            htmlOutput("lag_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "📊 Modelo VAR — Resumen del Sistema",
            status = "info", solidHeader = TRUE,
            verbatimTextOutput("var_output"),
            br(),
            htmlOutput("var_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Estabilidad del VAR — Módulo de Raíces",
            status = "warning", solidHeader = TRUE,
            plotOutput("roots_plot_companion", height = 320),
            verbatimTextOutput("roots_output"),
            br(),
            htmlOutput("roots_text")
          ),
          box(
            width = 6,
            title = "Causalidad de Granger",
            status = "danger", solidHeader = TRUE,
            verbatimTextOutput("granger_output"),
            br(),
            htmlOutput("granger_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Diagnóstico de Residuos del VAR",
            status = "success", solidHeader = TRUE,
            plotOutput("var_resid_plot", height = 380),
            br(),
            htmlOutput("var_resid_text")
          )
        )
        
      ),
      
      # =====================================================================
      # IRF Y FEVD
      # =====================================================================
      
      tabItem(
        tabName = "irf",
        
        fluidRow(
          box(
            width = 12,
            title = "Función Impulso-Respuesta (IRF)",
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4,
                     selectInput("irf_impulse", "Variable Impulso",
                                 choices  = c(acciones, "IPC"),
                                 selected = "IPC")
              ),
              column(4,
                     selectInput("irf_response", "Variable Respuesta",
                                 choices  = c(acciones, "IPC"),
                                 selected = "AMX")
              ),
              column(4,
                     sliderInput("irf_ahead", "Horizontes", min = 5, max = 30, value = 20)
              )
            ),
            plotOutput("irf_plot", height = 400),
            br(),
            htmlOutput("irf_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Descomposición de Varianza del Error de Pronóstico (FEVD)",
            status = "success", solidHeader = TRUE,
            plotlyOutput("fevd_plot", height = 420),
            br(),
            htmlOutput("fevd_text")
          )
        )
        
      ),
      
      # =====================================================================
      # OLS
      # =====================================================================
      
      tabItem(
        tabName = "ols",
        
        fluidRow(
          box(
            width = 6,
            title = "Modelo MCO — Regresión Múltiple",
            status = "primary", solidHeader = TRUE,
            verbatimTextOutput("ols_output"),
            br(),
            htmlOutput("ols_text")
          ),
          box(
            width = 6,
            title = "Diagnóstico de Residuos",
            status = "warning", solidHeader = TRUE,
            plotOutput("ols_diag", height = 400)
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Test de Heteroscedasticidad (Breusch-Pagan)",
            status = "danger", solidHeader = TRUE,
            verbatimTextOutput("bp_output"),
            htmlOutput("bp_text")
          ),
          box(
            width = 6,
            title = "Test de Autocorrelación (Durbin-Watson)",
            status = "info", solidHeader = TRUE,
            verbatimTextOutput("dw_output"),
            htmlOutput("dw_text")
          )
        )
        
      ),
      
      # =====================================================================
      # PRONÓSTICO
      # =====================================================================
      
      tabItem(
        tabName = "forecast_tab",
        
        fluidRow(
          box(
            width = 12,
            title = "Pronóstico VAR — Rendimientos Futuros",
            status = "success", solidHeader = TRUE,
            sliderInput("forecast_ahead", "Días a pronosticar", min = 5, max = 60, value = 20),
            plotOutput("forecast_plot", height = 450),
            br(),
            htmlOutput("forecast_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Pronóstico ARIMA Univariado",
            status = "info", solidHeader = TRUE,
            plotOutput("arima_forecast_plot", height = 320),
            htmlOutput("arima_text")
          ),
          box(
            width = 6,
            title = "Estadísticos de Pronóstico",
            status = "warning", solidHeader = TRUE,
            verbatimTextOutput("forecast_stats"),
            htmlOutput("forecast_stats_text")
          )
        )
        
      ),
      
      # =====================================================================
      # COMPARATIVO
      # =====================================================================
      
      tabItem(
        tabName = "compare",
        
        fluidRow(
          box(
            width = 12,
            title = "Comparativo — Rendimientos Diarios Normalizados",
            status = "primary", solidHeader = TRUE,
            plotlyOutput("compare_plot", height = 420),
            br(),
            htmlOutput("compare_text")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Tabla de Estadísticos Descriptivos",
            status = "info", solidHeader = TRUE,
            DT::dataTableOutput("stats_table")
          ),
          box(
            width = 6,
            title = "Boxplot de Rendimientos",
            status = "success", solidHeader = TRUE,
            plotlyOutput("boxplot_returns", height = 350)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Scatter Matrix — Relaciones Bivariadas",
            status = "warning", solidHeader = TRUE,
            plotlyOutput("scatter_matrix_plot", height = 500)
          )
        )
        
      ),
      
      # =====================================================================
      # SIMULADOR DE DESPLOME
      # =====================================================================
      
      tabItem(
        tabName = "simulator",
        
        fluidRow(
          
          column(
            width = 3,
            box(
              width = 12, 
              status = "danger", solidHeader = TRUE,
              title = "⚡ Parámetros del Choque Exógeno",
              tags$div(
                style = "padding: 5px; color: #c9d1d9;",
                selectInput(
                  "sim_accion",
                  "Variable Vector de Impacto Inicial:",
                  choices = c(acciones, "IPC"),
                  selected = "BANORTE"
                ),
                br(),
                sliderInput(
                  "sim_shock",
                  "Magnitud de la Caída Instantánea (%):",
                  min = -50, max = -5,
                  value = -20, step = 1,
                  post = "%"
                ),
                br(),
                sliderInput(
                  "sim_dias",
                  "Horizonte de Propagación Temporal (Días):",
                  min = 10, max = 90, value = 30, step = 5
                ),
                br(),
                actionButton(
                  "sim_run",
                  "🚨 Propagar Choque Estructural",
                  style = "width:100%; font-weight:700; font-size:15px; color: white; background-color: #ef4444; border-color: #dc2626; border-radius: 6px; padding: 10px;"
                )
              )
            )
          ),
          
          column(
            width = 9,
            fluidRow(
              
              column(
                width = 8,
                box(
                  width = 12, 
                  status = "primary", solidHeader = TRUE,
                  title = "Simulación Dinámica Acumulada: Trayectoria de Propagación del Riesgo",
                  plotlyOutput("sim_trayectoria_plot", height = 550),
                  br(),
                  htmlOutput("sim_trayectoria_text")
                )
              ),
              
              column(
                width = 4,
                box(
                  width = 12,
                  status = "warning", solidHeader = TRUE,
                  title = "💥 Pérdida Estimada",
                  uiOutput("sim_kpis_vertical")
                )
              )
              
            )
          )
        ), 
        
        fluidRow(
          box(
            width = 6,
            title = "Impacto Estimado por Variable (Correlaciones Históricas)",
            status = "warning", solidHeader = TRUE,
            plotlyOutput("sim_impacto_plot", height = 380),
            br(),
            htmlOutput("sim_impacto_text")
          ),
          box(
            width = 6,
            title = "Contagio IRF — Respuesta ante el Shock",
            status = "info", solidHeader = TRUE,
            plotOutput("sim_irf_plot", height = 380),
            br(),
            htmlOutput("sim_irf_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "📋 Tabla de Impactos Proyectados",
            status = "success", solidHeader = TRUE,
            DT::dataTableOutput("sim_tabla"),
            br(),
            htmlOutput("sim_tabla_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Análisis Histórico — Periodos de Alta Correlación con el Activo",
            status = "primary", solidHeader = TRUE,
            plotlyOutput("sim_hist_plot", height = 360),
            br(),
            htmlOutput("sim_hist_text")
          )
        )
        
      )
      
    ) # cierre tabItems
  )   # cierre dashboardBody
)     # cierre dashboardPage

# =====================================================================
# SERVER
# =====================================================================

server <- function(input, output, session) {
  
  # =====================================================================
  # REACTIVOS PRINCIPALES — BLINDAJE CONTRA EL ERROR DE SCOPING EN VAR
  # =====================================================================
  
  serie_sel <- reactive({
    mat <- dlog_df[, c(input$accion, "IPC")]
    na.omit(mat)
  })
  
  serie_xts_sel <- reactive({
    dlog_diario[, c(input$accion, "IPC")]
  })
  
  vars_var <- reactive({
    na.omit(dlog_df[, c(acciones, "IPC")])
  })
  
  modelo_var <- reactive({
    dat <- vars_var()
    tryCatch({
      p_val <- as.integer(input$lags)
      mod <- VAR(dat[, c(input$accion, "IPC")], p = p_val, type = "const")
      
      # 🔴 HACK METAPROGRAMÁTICO: Inyectamos el valor duro en la memoria del modelo 
      # para evitar que update() busque "input$lags" y dispare el error durante el IRF.
      mod$call$p <- p_val
      
      mod
    }, error = function(e) NULL)
  })
  
  modelo_var_full <- reactive({
    dat <- vars_var()
    tryCatch({
      p_val <- as.integer(input$lags)
      mod <- VAR(dat, p = p_val, type = "const")
      
      # 🔴 HACK METAPROGRAMÁTICO: Mismo blindaje para el modelo sistémico.
      mod$call$p <- p_val
      
      mod
    }, error = function(e) NULL)
  })
  
  modelo_ols <- reactive({
    dat <- serie_sel()
    lm(dat[, 1] ~ dat[, 2])
  })
  
  # =====================================================================
  # KPIs — EXPLORATORIO
  # =====================================================================
  
  output$media_box <- renderValueBox({
    media <- mean(serie_sel()[, 1], na.rm = TRUE) * 100 * 252
    valueBox(
      paste0(round(media, 2), "%"),
      paste("Rdto Anualizado —", input$accion),
      color = ifelse(media > 0, "green", "red"),
      icon = icon("chart-line")
    )
  })
  
  output$vol_box <- renderValueBox({
    vol <- sd(serie_sel()[, 1], na.rm = TRUE) * sqrt(252) * 100
    valueBox(
      paste0(round(vol, 2), "%"),
      "Volatilidad Anualizada",
      color = ifelse(vol > 30, "red", ifelse(vol > 20, "yellow", "green")),
      icon = icon("chart-area")
    )
  })
  
  output$corr_box <- renderValueBox({
    corr <- cor(serie_sel()[, 1], serie_sel()[, 2], use = "complete.obs")
    valueBox(
      round(corr, 3),
      paste("Correlación", input$accion, "↔ IPC"),
      color = ifelse(abs(corr) > 0.6, "blue", "light-blue"),
      icon = icon("sync")
    )
  })
  
  output$sharpe_box <- renderValueBox({
    ret  <- mean(serie_sel()[, 1], na.rm = TRUE) * 252
    vol  <- sd(serie_sel()[, 1], na.rm = TRUE) * sqrt(252)
    rf   <- 0.10  
    sharpe <- (ret - rf) / vol
    valueBox(
      round(sharpe, 3),
      "Sharpe Ratio",
      color = ifelse(sharpe > 0.5, "green", ifelse(sharpe > 0, "yellow", "red")),
      icon = icon("star")
    )
  })
  
  # =====================================================================
  # EXPLORATORIO — GRÁFICAS
  # =====================================================================
  
  observeEvent(input$explore_toggle_ipc, {
    actuales <- input$explore_series
    if ("IPC" %in% actuales) {
      updateCheckboxGroupInput(session, "explore_series", selected = setdiff(actuales, "IPC"))
    } else {
      updateCheckboxGroupInput(session, "explore_series", selected = c(actuales, "IPC"))
    }
  })
  
  output$plot_precios <- renderPlotly({
    series_sel_exp <- if (length(input$explore_series) == 0) acciones else input$explore_series
    df_plot <- precios_df[, c("fecha", intersect(series_sel_exp, names(precios_df))), drop = FALSE]
    df_long <- pivot_longer(df_plot, -fecha, names_to = "Serie", values_to = "Precio")
    colores_todos <- c(
      "AMX" = "#2563eb", "BANORTE" = "#16a34a", "WALMEX" = "#d97706",
      "BIMBO" = "#dc2626", "FEMSA" = "#7c3aed", "CEMEX" = "#0891b2",
      "IPC"  = "#f59e0b"
    )
    colores_activos <- colores_todos[names(colores_todos) %in% series_sel_exp]
    p <- ggplot(df_long, aes(fecha, Precio, color = Serie)) +
      geom_line(linewidth = 0.7, alpha = 0.9) +
      scale_color_manual(values = colores_activos) +
      labs(x = NULL, y = "Precio Ajustado (MXN)", title = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        panel.grid.minor = element_blank(),
        text  = element_text(color = "#c9d1d9"),
        axis.text = element_text(color = "#b0bec5"),
        legend.background = element_rect(fill = "#161b27"),
        legend.text = element_text(color = "#c9d1d9"),
        legend.position = "bottom"
      )
    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#161b27",
        plot_bgcolor  = "#161b27",
        font = list(color = "#c9d1d9")
      )
  })
  
  output$explore_text <- renderUI({
    HTML("
    <div class='texto'>
    <b>Precios Ajustados Diarios (2019–2026).</b>
    Se observan claramente los impactos del COVID-19 (marzo 2020),
    recuperación de 2021, ciclo alcista de 2022–2023 en sectores defensivos
    y la divergencia entre emisoras de consumo (WALMEX, BIMBO) frente
    a cíclicas (CEMEX, AMX). La corrección de tasas de 2024–2025 afectó
    especialmente a sectores intensivos en deuda.
    </div>
    ")
  })
  
  output$returns_plot_ly <- renderPlotly({
    df_r <- data.frame(
      fecha = dlog_df$fecha,
      ret   = dlog_df[, input$accion]
    )
    p <- ggplot(df_r, aes(fecha, ret)) +
      geom_col(aes(fill = ret > 0), width = 0.8, alpha = 0.8) +
      scale_fill_manual(values = c("TRUE" = "#22c55e", "FALSE" = "#ef4444"), guide = FALSE) +
      labs(x = NULL, y = "Rendimiento Diario", title = paste("Rdtos —", input$accion)) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"),
        axis.text = element_text(color = "#b0bec5"),
        legend.position = "none"
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"), showlegend = FALSE)
  })
  
  output$returns_text <- renderUI({
    vol  <- sd(dlog_df[, input$accion], na.rm = TRUE) * sqrt(252) * 100
    sesgo <- e1071::skewness(dlog_df[, input$accion], na.rm = TRUE)
    kurt  <- e1071::kurtosis(dlog_df[, input$accion], na.rm = TRUE)
    HTML(paste0("
    <div class='texto'>
    Volatilidad anualizada: <b>", round(vol, 2), "%</b> &nbsp;|&nbsp;
    Asimetría: <b>", round(sesgo, 3), "</b> &nbsp;|&nbsp;
    Curtosis: <b>", round(kurt, 3), "</b>.
    <br>Distribución de retornos con colas pesadas, típico de activos financieros.
    </div>
    "))
  })
  
  output$corr_plot <- renderPlot({
    mat_c <- cor(dlog_df[, c(acciones, "IPC")], use = "complete.obs")
    par(bg = "#161b27", fg = "#c9d1d9")
    corrplot(
      mat_c, method = "color", addCoef.col = "#c9d1d9",
      col = colorRampPalette(c("#ef4444", "#161b27", "#2563eb"))(200),
      tl.col = "#c9d1d9", cl.pos = "r",
      number.cex = 0.75, tl.cex = 0.85
    )
  }, bg = "#161b27")
  
  output$corr_text <- renderUI({
    corr <- cor(dlog_df[, input$accion], dlog_df[, "IPC"], use = "complete.obs")
    nivel <- ifelse(abs(corr) > 0.6, "alta", ifelse(abs(corr) > 0.3, "moderada", "baja"))
    HTML(paste0("<div class='texto'>Correlación con IPC: <b>", round(corr, 3),
                "</b> — relación <b>", nivel, "</b>. ",
                ifelse(corr > 0.5,
                       "La acción sigue de cerca al mercado.",
                       "La acción tiene comportamiento idiosincrático."),
                "</div>"))
  })
  
  output$acum_plot <- renderPlotly({
    mostrar_ipc <- isTRUE(input$acum_show_ipc)
    vars_acum   <- if (mostrar_ipc) c(acciones, "IPC") else acciones
    acum <- precios_df[, c("fecha", vars_acum)]
    for (nm in vars_acum) {
      base_val <- as.numeric(acum[[nm]][1])
      if (!is.na(base_val) && base_val != 0) {
        acum[[nm]] <- 100 * acum[[nm]] / base_val
      }
    }
    colores_todos <- c(
      "AMX" = "#2563eb", "BANORTE" = "#16a34a", "WALMEX" = "#d97706",
      "BIMBO" = "#dc2626", "FEMSA" = "#7c3aed", "CEMEX" = "#0891b2",
      "IPC"  = "#f59e0b"
    )
    colores_activos <- colores_todos[names(colores_todos) %in% vars_acum]
    df_long <- pivot_longer(acum, -fecha, names_to = "Serie", values_to = "Valor")
    p <- ggplot(df_long, aes(fecha, Valor, color = Serie)) +
      geom_line(linewidth = 0.85) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "#4b5563", linewidth = 0.5) +
      scale_color_manual(values = colores_activos) +
      labs(x = NULL, y = "Base 100 = Primer día disponible") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"),
        axis.text = element_text(color = "#b0bec5"),
        legend.background = element_rect(fill = "#161b27"),
        legend.text = element_text(color = "#c9d1d9")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  # =====================================================================
  # SERIES ORIGINALES
  # =====================================================================
  
  output$series_niveles_plot <- renderPlotly({
    df_long <- pivot_longer(log_df, -fecha, names_to = "Serie", values_to = "LogPrecio")
    p <- ggplot(df_long, aes(fecha, LogPrecio, color = Serie)) +
      geom_line(linewidth = 0.7) +
      labs(x = NULL, y = "Log(Precio)", title = "Logaritmo de Precios — Niveles") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"),
        axis.text = element_text(color = "#b0bec5"),
        legend.background = element_rect(fill = "#161b27"),
        legend.text = element_text(color = "#c9d1d9")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$series_niveles_text <- renderUI({
    HTML("
    <div class='texto'>
    <b>Series en Niveles Logarítmicos.</b>
    Todas las series muestran tendencia creciente y no-estacionariedad en niveles
    (presencia de raíz unitaria). Esto es consistente con la hipótesis de caminata aleatoria
    en mercados financieros. <b>Se requiere tomar primeras diferencias</b> para inducir
    estacionariedad antes de aplicar modelos VAR. Excepción: si existe cointegración,
    se puede modelar en VECM con las series en niveles.
    </div>
    ")
  })
  
  output$series_diff_plot_all <- renderPlotly({
    df_long <- pivot_longer(dlog_df, -fecha, names_to = "Serie", values_to = "Rdto")
    p <- ggplot(df_long, aes(fecha, Rdto, color = Serie)) +
      geom_line(linewidth = 0.5, alpha = 0.8) +
      facet_wrap(~Serie, ncol = 2, scales = "free_y") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#4b5563") +
      labs(x = NULL, y = "Rendimiento Log-Diario") +
      theme_minimal(base_size = 10) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        strip.background = element_rect(fill = "#1c2333"),
        strip.text = element_text(color = "#c9d1d9"),
        text = element_text(color = "#c9d1d9"),
        axis.text = element_text(color = "#b0bec5", size = 7),
        legend.position = "none"
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$series_diff_text <- renderUI({
    HTML("
    <div class='texto'>
    <b>Primeras Diferencias del Logaritmo (Rendimientos Continuos).</b>
    Las series diferenciadas presentan media cercana a cero y oscilan alrededor
    de ella, siendo compatibles con estacionariedad. Se observan <b>rachas de
    volatilidad</b> (volatility clustering), especialmente en 2020 (COVID),
    2022 (ciclo de alza de tasas BANXICO) y episodios específicos por sector.
    </div>
    ")
  })
  
  output$tabla_adf_global <- DT::renderDataTable({
    resultados <- lapply(c(acciones, "IPC"), function(nm) {
      serie <- na.omit(log_df[[nm]])
      dserie <- na.omit(dlog_df[[nm]])
      
      adf_n <- tryCatch(adf.test(serie)$p.value, error = function(e) NA)
      adf_d <- tryCatch(adf.test(dserie)$p.value, error = function(e) NA)
      
      data.frame(
        Variable = nm,
        `ADF Nivel (p-val)` = round(adf_n, 4),
        `ADF Diferencia (p-val)` = round(adf_d, 4),
        `Estacionaria en Niveles` = ifelse(!is.na(adf_n) & adf_n < 0.05, "✅ Sí", "❌ No"),
        `Estacionaria en Dif.`   = ifelse(!is.na(adf_d) & adf_d < 0.05, "✅ Sí", "❌ No"),
        `Orden Integración`      = ifelse(!is.na(adf_n) & adf_n < 0.05, "I(0)", "I(1)")
      )
    })
    df_out <- do.call(rbind, resultados)
    DT::datatable(
      df_out,
      options = list(
        pageLength = 10, dom = "t",
        columnDefs = list(list(className = "dt-center", targets = 1:5))
      ),
      rownames = FALSE,
      style = "bootstrap4"
    )
  })
  
  output$tabla_adf_text <- renderUI({
    HTML("
    <div class='texto'>
    <b>Interpretación:</b> El test ADF contrasta H₀: existe raíz unitaria.
    Un p-valor &lt; 0.05 rechaza H₀ → la serie es estacionaria.
    Las series financieras de precios son típicamente I(1): integradas de orden 1,
    estacionarias sólo tras una primera diferencia.
    </div>
    ")
  })
  
  output$vol_rodante_plot <- renderPlotly({
    ret <- dlog_df[, input$accion]
    vol_r <- zoo::rollapply(ret, 30, sd, fill = NA, align = "right") * sqrt(252) * 100
    df_v <- data.frame(fecha = dlog_df$fecha, vol = vol_r)
    p <- ggplot(df_v, aes(fecha, vol)) +
      geom_line(color = "#f59e0b", linewidth = 1) +
      geom_ribbon(aes(ymin = 0, ymax = vol), fill = "#f59e0b", alpha = 0.15) +
      labs(x = NULL, y = "Volatilidad Anualizada (%)") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$vol_rodante_text <- renderUI({
    HTML("<div class='texto'>Volatilidad rodante de 30 días anualizada. Picos corresponden a episodios de estrés financiero.</div>")
  })
  
  output$dist_returns_plot <- renderPlotly({
    ret <- dlog_df[, input$accion]
    p <- ggplot(data.frame(ret = ret), aes(ret)) +
      geom_histogram(aes(y = after_stat(density)), bins = 80, fill = "#2563eb", alpha = 0.7, color = "#0d1117") +
      geom_density(color = "#f59e0b", linewidth = 1) +
      stat_function(fun = dnorm, args = list(mean = mean(ret, na.rm=T), sd = sd(ret, na.rm=T)),
                    color = "#ef4444", linewidth = 1, linetype = "dashed") +
      labs(x = "Rendimiento Diario", y = "Densidad") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$dist_returns_text <- renderUI({
    HTML("<div class='texto'>Histograma vs. densidad empírica (amarillo) y normal teórica (rojo). Las colas pesadas revelan leptocurtosis.</div>")
  })
  
  # =====================================================================
  # ESTACIONARIEDAD
  # =====================================================================
  
  output$serie_original_plot <- renderPlot({
    serie <- log_df[, input$accion]
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5", col.lab = "#c9d1d9",
        col.main = "#ffffff", mar = c(4, 4, 2, 1))
    plot(log_df$fecha, serie, type = "l", col = "#2563eb", lwd = 2,
         xlab = "Fecha", ylab = paste("Log(", input$accion, ")"),
         main = paste("Log-Precio:", input$accion))
  }, bg = "#161b27")
  
  output$serie_original_text <- renderUI({
    HTML("
    <div class='texto'>
    La serie logarítmica exhibe tendencia estocástica (caminata aleatoria),
    no estacionaria en media ni varianza. Incompatible para modelación directa.
    </div>
    ")
  })
  
  output$serie_diff_plot <- renderPlot({
    serie_d <- dlog_df[, input$accion]
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5", col.lab = "#c9d1d9",
        col.main = "#ffffff", mar = c(4, 4, 2, 1))
    plot(dlog_df$fecha, serie_d, type = "l", col = "#ef4444", lwd = 1.5,
         xlab = "Fecha", ylab = "Log-Diff",
         main = paste("Rendimientos:", input$accion))
    abline(h = 0, lty = 2, col = "#4b5563")
  }, bg = "#161b27")
  
  output$serie_diff_text <- renderUI({
    HTML("
    <div class='texto'>
    Primera diferencia del logaritmo = rendimiento continuo diario.
    La serie oscila alrededor de cero, siendo estacionaria en media.
    Heteroscedasticidad condicional (ARCH) sigue presente.
    </div>
    ")
  })
  
  output$trend_plot <- renderPlotly({
    serie <- log_df[[input$accion]]
    fechas <- log_df$fecha
    ma60  <- zoo::rollmean(serie, 60,  fill = NA, align = "right")
    ma120 <- zoo::rollmean(serie, 120, fill = NA, align = "right")
    ma252 <- zoo::rollmean(serie, 252, fill = NA, align = "right")
    df_t <- data.frame(fecha = fechas, original = serie,
                       MA60 = ma60, MA120 = ma120, MA252 = ma252)
    df_long <- pivot_longer(df_t, -fecha, names_to = "Serie", values_to = "Valor")
    p <- ggplot(df_long, aes(fecha, Valor, color = Serie, linewidth = Serie)) +
      geom_line(alpha = 0.9) +
      scale_color_manual(values = c("original" = "#4b5563", "MA60" = "#2563eb",
                                    "MA120" = "#f59e0b", "MA252" = "#ef4444")) +
      scale_linewidth_manual(values = c("original" = 0.5, "MA60" = 1.2,
                                        "MA120" = 1.5, "MA252" = 2), guide = FALSE) +
      labs(x = NULL, y = paste("Log(", input$accion, ")"),
           title = paste("Tendencia con MAs —", input$accion)) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5"),
        legend.background = element_rect(fill = "#161b27"),
        legend.text = element_text(color = "#c9d1d9")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$trend_text <- renderUI({
    HTML("
    <div class='texto'>
    Medias móviles de 60, 120 y 252 días (3 meses, 6 meses, 1 año).
    Cuando el precio cruza hacia arriba la MA252 se genera señal alcista (golden cross);
    cruzar hacia abajo indica tendencia bajista (death cross).
    </div>
    ")
  })
  
  output$adf_output <- renderPrint({
    serie_d <- na.omit(dlog_df[, input$accion])
    cat("=== ADF en NIVELES ===\n")
    print(adf.test(log_df[[input$accion]]))
    cat("\n=== ADF en DIFERENCIAS ===\n")
    print(adf.test(serie_d))
  })
  
  output$adf_text <- renderUI({
    adf_n <- tryCatch(adf.test(log_df[[input$accion]])$p.value, error = function(e) NA)
    adf_d <- tryCatch(adf.test(na.omit(dlog_df[[input$accion]]))$p.value, error = function(e) NA)
    HTML(paste0("
    <div class='texto'>
    <b>ADF Niveles:</b> p = ", round(adf_n, 4), " — ",
                ifelse(!is.na(adf_n) & adf_n < 0.05, "✅ Estacionaria", "❌ Raíz Unitaria"), "<br>
    <b>ADF Diferencias:</b> p = ", round(adf_d, 4), " — ",
                ifelse(!is.na(adf_d) & adf_d < 0.05, "✅ Estacionaria", "❌ Raíz Unitaria"), "<br>
    La serie es I(1): integrada de orden 1.
    </div>
    "))
  })
  
  output$kpss_output <- renderPrint({
    cat("=== KPSS en NIVELES ===\n")
    print(kpss.test(log_df[[input$accion]]))
    cat("\n=== KPSS en DIFERENCIAS ===\n")
    print(kpss.test(na.omit(dlog_df[[input$accion]])))
  })
  
  output$kpss_text <- renderUI({
    kpss_n <- tryCatch(kpss.test(log_df[[input$accion]])$p.value, error = function(e) NA)
    HTML(paste0("
    <div class='texto'>
    KPSS: H₀ = estacionaria. P-valor niveles: <b>", round(kpss_n, 4), "</b><br>
    ",
                ifelse(!is.na(kpss_n) & kpss_n < 0.05,
                       "❌ Se rechaza H₀: no estacionaria en niveles.",
                       "✅ No se rechaza H₀: posible estacionariedad."),
                "</div>
    "))
  })
  
  output$acf_plot <- renderPlot({
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5",
        col.lab = "#c9d1d9", col.main = "#ffffff", mar = c(4, 4, 3, 1))
    Acf(na.omit(dlog_df[, input$accion]),
        main = paste("ACF —", input$accion),
        col = "#2563eb", lwd = 2)
  }, bg = "#161b27")
  
  output$acf_text <- renderUI({
    HTML("<div class='texto'>ACF de rendimientos. Poca autocorrelación lineal, pero posibles efectos ARCH en los cuadrados (volatility clustering).</div>")
  })
  
  output$pacf_plot <- renderPlot({
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5",
        col.lab = "#c9d1d9", col.main = "#ffffff", mar = c(4, 4, 3, 1))
    Pacf(na.omit(dlog_df[, input$accion]),
         main = paste("PACF —", input$accion),
         col = "#f59e0b", lwd = 2)
  }, bg = "#161b27")
  
  output$pacf_text <- renderUI({
    HTML("<div class='texto'>PACF útil para identificar el orden AR del proceso subyacente en los rendimientos.</div>")
  })
  
  # =====================================================================
  # COINTEGRACIÓN
  # =====================================================================
  
  mat_jo <- reactive({
    vars_sel <- c(acciones, "IPC")
    if (input$johansen_vars == "todas") {
      na.omit(log_df[, vars_sel])
    } else {
      na.omit(log_df[, acciones])
    }
  })
  
  johansen_result <- reactive({
    mat <- mat_jo()
    tryCatch(
      ca.jo(
        mat,
        type   = input$johansen_type,
        ecdet  = input$johansen_ecdet,
        K      = max(2, input$lags)
      ),
      error = function(e) NULL
    )
  })
  
  output$johansen_output <- renderPrint({
    res <- johansen_result()
    if (is.null(res)) {
      cat("Error en el cálculo de Johansen. Intente con otros parámetros.\n")
    } else {
      summary(res)
    }
  })
  
  output$johansen_text <- renderUI({
    res <- johansen_result()
    if (is.null(res)) {
      HTML("<div class='texto'>Error en el test. Revisa los parámetros.</div>")
    } else {
      tipo_lbl <- ifelse(input$johansen_type == "trace", "Traza", "Eigenvalor Máximo")
      HTML(paste0("
      <div class='texto'>
      <b>Test de Johansen (", tipo_lbl, "):</b>
      Contrasta H₀: r vectores cointegrantes vs. Hₐ: r+1 vectores.
      Se rechaza H₀ cuando el estadístico supera el valor crítico al 5%.
      <br><br>
      Si r ≥ 1 vector cointegrante: existe equilibrio de largo plazo entre las series.
      El número de vectores cointegrantes determina cuántas combinaciones lineales
      estacionarias existen, base del modelo VECM.
      </div>
      "))
    }
  })
  
  output$johansen_eigen_output <- renderPrint({
    res <- johansen_result()
    if (!is.null(res)) {
      cat("Eigenvalues (valores propios):\n")
      print(res@lambda)
      cat("\nEigenvectors (vectores propios):\n")
      print(round(res@V, 4))
    }
  })
  
  output$johansen_eigen_text <- renderUI({
    HTML("<div class='texto'>Los eigenvalores miden la fuerza de cada relación cointegrante. Eigenvalores más grandes indican relaciones de largo plazo más fuertes.</div>")
  })
  
  output$johansen_vec_output <- renderPrint({
    res <- johansen_result()
    if (!is.null(res)) {
      cat("Vectores Cointegrantes (normalizado al primer elemento):\n")
      pio <- res@pio
      print(round(pio, 4))
    }
  })
  
  output$johansen_vec_text <- renderUI({
    HTML("<div class='texto'>Vectores cointegrantes Pi = αβ'. β son los vectores que forman combinaciones lineales estacionarias; α son los coeficientes de ajuste al equilibrio.</div>")
  })
  
  output$johansen_residuos_plot <- renderPlotly({
    res <- johansen_result()
    if (is.null(res)) return(NULL)
    mat <- mat_jo()
    beta <- res@V[, 1]
    residuo <- as.numeric(as.matrix(mat) %*% beta)
    fechas  <- log_df$fecha[seq_len(length(residuo))]
    df_res  <- data.frame(fecha = fechas[seq_len(length(residuo))], residuo = residuo)
    p <- ggplot(df_res, aes(fecha, residuo)) +
      geom_line(color = "#2563eb", linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#ef4444", linewidth = 1) +
      labs(x = NULL, y = "Relación Cointegrante", title = "Vector Cointegrante (β'x_t)") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$johansen_residuos_text <- renderUI({
    HTML("
    <div class='texto'>
    La combinación lineal β'x_t debe ser estacionaria si existe cointegración real.
    Si oscila alrededor de cero sin tendencia → evidencia de relación de largo plazo.
    Desviaciones grandes representan desequilibrios temporales que el sistema corrige.
    </div>
    ")
  })
  
  # =====================================================================
  # VAR Y REZAGOS
  # =====================================================================
  
  varselect_result <- reactive({
    dat <- vars_var()
    tryCatch(
      VARselect(dat[, c(input$accion, "IPC")], lag.max = 12, type = "const"),
      error = function(e) NULL
    )
  })
  
  output$lag_output <- renderPrint({
    res <- varselect_result()
    if (!is.null(res)) print(res)
  })
  
  output$lag_criterios_plot <- renderPlotly({
    res <- varselect_result()
    if (is.null(res)) return(NULL)
    
    crit_mat <- as.data.frame(t(res$criteria))
    crit_mat$lag <- seq_len(nrow(crit_mat))
    
    crit_norm <- crit_mat
    for (col in setdiff(names(crit_norm), "lag")) {
      mn <- min(crit_norm[[col]], na.rm = TRUE)
      mx <- max(crit_norm[[col]], na.rm = TRUE)
      if (mx > mn) crit_norm[[col]] <- (crit_norm[[col]] - mn) / (mx - mn)
    }
    df_long <- pivot_longer(crit_norm, -lag, names_to = "Criterio", values_to = "Valor")
    colores_crit <- c(
      "AIC(n)" = "#2563eb", "HQ(n)" = "#22c55e",
      "SC(n)"  = "#f59e0b", "FPE(n)" = "#ef4444"
    )
    
    df_min <- df_long %>%
      group_by(Criterio) %>%
      slice_min(Valor, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    p <- ggplot(df_long, aes(lag, Valor, color = Criterio)) +
      geom_line(linewidth = 1.2, alpha = 0.9) +
      geom_point(size = 2.5, alpha = 0.8) +
      geom_point(data = df_min, aes(lag, Valor), shape = 23,
                 size = 5, fill = "white", alpha = 0.9) +
      scale_color_manual(values = colores_crit) +
      scale_x_continuous(breaks = seq_len(max(df_long$lag))) +
      labs(
        x     = "Número de Rezagos (p)",
        y     = "Criterio Normalizado [0–1]",
        title = "Criterios de Información (mínimo = óptimo ◇)",
        color = "Criterio"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        panel.grid.minor = element_blank(),
        text             = element_text(color = "#c9d1d9"),
        axis.text        = element_text(color = "#b0bec5"),
        strip.background = element_rect(fill = "#1c2333"),
        strip.text       = element_text(color = "#c9d1d9", face="bold"),
        legend.position  = "none"
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#161b27",
        plot_bgcolor  = "#161b27",
        font          = list(color = "#c9d1d9")
      )
  })
  
  output$lag_text <- renderUI({
    res <- varselect_result()
    if (is.null(res)) {
      HTML("<div class='texto'>Error en selección de rezagos.</div>")
    } else {
      opt_aic <- which.min(res$criteria["AIC(n)",])
      opt_bic <- which.min(res$criteria["SC(n)",])
      HTML(paste0("
      <div class='texto'>
      <b>AIC óptimo:</b> p = ", opt_aic, " rezagos &nbsp;|&nbsp;
      <b>BIC/SC óptimo:</b> p = ", opt_bic, " rezagos.
      <br>AIC tiende a sobreajustar; BIC es más parsimonioso.
      El rezago seleccionado actualmente es <b>p = ", input$lags, "</b>.
      </div>
      "))
    }
  })
  
  output$var_output <- renderPrint({
    mod <- modelo_var()
    if (!is.null(mod)) summary(mod) else cat("Error en modelo VAR.\n")
  })
  
  output$var_text <- renderUI({
    mod <- modelo_var()
    if (is.null(mod)) {
      HTML("<div class='texto'>Error al estimar VAR.</div>")
    } else {
      HTML(paste0("
      <div class='texto'>
      VAR(", input$lags, ") estimado con las variables: <b>", input$accion, " + IPC</b>.
      <br>El sistema captura la dinámica de retroalimentación entre la acción y el mercado.
      Los coeficientes muestran cómo los valores pasados de cada variable predicen el valor presente.
      </div>
      "))
    }
  })
  
  output$roots_plot_companion <- renderPlot({
    mod <- modelo_var()
    if (is.null(mod)) return(NULL)
    r <- roots(mod)
    theta <- seq(0, 2 * pi, length.out = 200)
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5",
        col.lab = "#c9d1d9", col.main = "#ffffff", mar = c(4, 4, 3, 1))
    plot(cos(theta), sin(theta), type = "l", col = "#21273d", lwd = 2,
         asp = 1, xlab = "Real", ylab = "Imaginario",
         main = "Raíces del Polinomio Característico", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))
    abline(h = 0, v = 0, col = "#4b5563", lty = 2)
    points(Re(r), Im(r), pch = 19, col = "#ef4444", cex = 2)
  }, bg = "#161b27")
  
  output$roots_output <- renderPrint({
    mod <- modelo_var()
    if (!is.null(mod)) {
      r <- roots(mod)
      cat("Módulo de las raíces:\n")
      print(round(Mod(r), 5))
      cat("\nEstable:", all(Mod(r) < 1), "\n")
    }
  })
  
  output$roots_text <- renderUI({
    mod <- modelo_var()
    if (is.null(mod)) return(HTML("<div class='texto'>—</div>"))
    r <- roots(mod)
    estable <- all(Mod(r) < 1)
    HTML(paste0("
    <div class='texto'>
    Estabilidad VAR: <b>", ifelse(estable, "✅ ESTABLE", "❌ INESTABLE"), "</b>.
    <br>Todas las raíces dentro del círculo unitario → modelo convergente.
    ", ifelse(!estable, "⚠️ Raíces ≥ 1 indican no-estacionariedad. Considere VECM.", ""),
                "</div>"))
  })
  
  output$granger_output <- renderPrint({
    mod <- modelo_var()
    if (!is.null(mod)) {
      cat("=== Granger: IPC → ", input$accion, "===\n")
      tryCatch(print(causality(mod, cause = "IPC")), error = function(e) cat("Error:", e$message))
      cat("\n=== Granger:", input$accion, "→ IPC ===\n")
      tryCatch(print(causality(mod, cause = input$accion)), error = function(e) cat("Error:", e$message))
    }
  })
  
  output$granger_text <- renderUI({
    HTML("
    <div class='texto'>
    <b>Causalidad de Granger:</b> Determina si los valores pasados de X
    mejoran el pronóstico de Y (más allá del pasado de Y).
    No implica causalidad estructural, sino precedencia temporal.
    </div>
    ")
  })
  
  output$var_resid_plot <- renderPlot({
    mod <- modelo_var()
    if (is.null(mod)) return(NULL)
    residuos <- residuals(mod)
    par(mfrow = c(2, 2), bg = "#161b27", fg = "#c9d1d9",
        col.axis = "#b0bec5", col.lab = "#c9d1d9", col.main = "#ffffff",
        mar = c(3, 3, 2, 1))
    
    for (i in 1:min(2, ncol(residuos))) {
      plot(residuos[, i], type = "l", col = c("#2563eb", "#f59e0b")[i],
           main = paste("Residuos:", colnames(residuos)[i]), xlab = "", ylab = "")
      abline(h = 0, lty = 2, col = "#4b5563")
      acf(residuos[, i], main = paste("ACF Residuos:", colnames(residuos)[i]),
          col = "#ef4444", lag.max = 20)
    }
  }, bg = "#161b27")
  
  output$var_resid_text <- renderUI({
    HTML("
    <div class='texto'>
    Los residuos del VAR deben ser ruido blanco (no autocorrelación).
    La ACF de residuos dentro de bandas de confianza confirma especificación correcta.
    </div>
    ")
  })
  
  # =====================================================================
  # IRF Y FEVD
  # =====================================================================
  
  output$irf_plot <- renderPlot({
    mod <- modelo_var_full()
    if (is.null(mod)) {
      par(bg = "#161b27", fg = "#c9d1d9")
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Modelo VAR no disponible. Ajuste los rezagos.")
      return(invisible(NULL))
    }
    vars_mod <- colnames(mod$y)
    imp  <- if (!is.null(input$irf_impulse)  && input$irf_impulse  %in% vars_mod) input$irf_impulse  else vars_mod[1]
    resp <- if (!is.null(input$irf_response) && input$irf_response %in% vars_mod) input$irf_response else vars_mod[2]
    
    h_ahead <- as.integer(input$irf_ahead)
    imp_val <- as.character(imp)
    resp_val <- as.character(resp)
    
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5",
        col.lab = "#c9d1d9", col.main = "#ffffff", mar = c(4, 4, 3, 1))
    
    tryCatch({
      modelo_irf <- irf(
        mod,
        impulse  = imp_val,
        response = resp_val,
        n.ahead  = h_ahead,
        boot     = TRUE,
        runs     = 100,
        ci       = 0.95
      )
      plot(modelo_irf, main = paste("Respuesta de", resp_val, "ante shock en", imp_val))
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = paste("Error al calcular IRF:", conditionMessage(e)))
      text(1, 1, conditionMessage(e), col = "#ef4444", cex = 0.9)
    })
  }, bg = "#161b27")
  
  output$irf_text <- renderUI({
    HTML(paste0("
    <div class='texto'>
    IRF: respuesta de <b>", input$irf_response, "</b> ante un shock de 1 desv. est.
    en <b>", input$irf_impulse, "</b>. La banda sombreada es IC al 95% (bootstrap).
    Un efecto que decae hacia cero indica que el shock es transitorio.
    </div>
    "))
  })
  
  output$fevd_plot <- renderPlotly({
    mod <- modelo_var()
    if (is.null(mod)) return(NULL)
    tryCatch({
      fevd_res <- fevd(mod, n.ahead = input$irf_ahead)
      df_fevd <- do.call(rbind, lapply(names(fevd_res), function(var) {
        mat <- fevd_res[[var]]
        df  <- as.data.frame(mat)
        df$horizon  <- 1:nrow(df)
        df$variable <- var
        pivot_longer(df, -c(horizon, variable), names_to = "fuente", values_to = "proporcion")
      }))
      p <- ggplot(df_fevd, aes(horizon, proporcion * 100, fill = fuente)) +
        geom_area(alpha = 0.85) +
        facet_wrap(~variable, ncol = 2) +
        scale_fill_manual(values = c(
          "AMX" = "#2563eb", "BANORTE" = "#16a34a", "WALMEX" = "#d97706",
          "BIMBO" = "#dc2626", "FEMSA" = "#7c3aed", "CEMEX" = "#0891b2",
          "IPC" = "#f59e0b"
        )) +
        labs(x = "Horizonte (días)", y = "% Varianza Explicada") +
        theme_minimal(base_size = 10) +
        theme(
          plot.background = element_rect(fill = "#161b27", color = NA),
          panel.background = element_rect(fill = "#161b27", color = NA),
          panel.grid.major = element_line(color = "#21273d"),
          strip.background = element_rect(fill = "#1c2333"),
          strip.text = element_text(color = "#c9d1d9"),
          text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5"),
          legend.background = element_rect(fill = "#161b27"),
          legend.text = element_text(color = "#c9d1d9")
        )
      ggplotly(p) %>%
        layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
               font = list(color = "#c9d1d9"))
    }, error = function(e) NULL)
  })
  
  output$fevd_text <- renderUI({
    HTML("
    <div class='texto'>
    FEVD: qué porcentaje de la varianza del error de pronóstico de cada variable
    es atribuible a shocks propios vs. shocks de otras variables.
    Horizonte 1 = innovaciones propias; horizontes largos = impacto cruzado.
    </div>
    ")
  })
  
  # =====================================================================
  # OLS
  # =====================================================================
  
  output$ols_output <- renderPrint({
    summary(modelo_ols())
  })
  
  output$ols_text <- renderUI({
    mod <- modelo_ols()
    r2 <- round(summary(mod)$r.squared, 4)
    HTML(paste0("
    <div class='texto'>
    <b>R² = ", r2, "</b> — El IPC explica el ", round(r2 * 100, 1),
                "% de la variación en los rendimientos de ", input$accion, ".
    El coeficiente β del IPC mide el <b>beta de mercado</b> de la acción.
    </div>
    "))
  })
  
  output$ols_diag <- renderPlot({
    par(mfrow = c(2, 2), bg = "#161b27", fg = "#c9d1d9",
        col.axis = "#b0bec5", col.lab = "#c9d1d9", col.main = "#ffffff", mar = c(3,3,2,1))
    plot(modelo_ols(), col = "#2563eb")
  }, bg = "#161b27")
  
  output$bp_output <- renderPrint({
    tryCatch(
      print(lmtest::bptest(modelo_ols())),
      error = function(e) cat("Error:", e$message)
    )
  })
  
  output$bp_text <- renderUI({
    bp <- tryCatch(lmtest::bptest(modelo_ols())$p.value, error = function(e) NA)
    HTML(paste0("<div class='texto'>Breusch-Pagan p = <b>", round(bp, 4), "</b> — ",
                ifelse(!is.na(bp) & bp < 0.05,
                       "❌ Heteroscedasticidad detectada.",
                       "✅ No se detecta heteroscedasticidad."),
                "</div>"))
  })
  
  output$dw_output <- renderPrint({
    tryCatch(
      print(lmtest::dwtest(modelo_ols())),
      error = function(e) cat("Error:", e$message)
    )
  })
  
  output$dw_text <- renderUI({
    dw <- tryCatch(lmtest::dwtest(modelo_ols())$statistic, error = function(e) NA)
    HTML(paste0("<div class='texto'>Estadístico DW = <b>", round(dw, 4), "</b>. ",
                "Valor ~2 indica no autocorrelación. < 1.5 o > 2.5 sugiere autocorrelación.",
                "</div>"))
  })
  
  # =====================================================================
  # PRONÓSTICO
  # =====================================================================
  
  output$forecast_plot <- renderPlot({
    mod <- modelo_var()
    if (is.null(mod)) return(NULL)
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5",
        col.lab = "#c9d1d9", col.main = "#ffffff")
    tryCatch({
      pred <- predict(mod, n.ahead = input$forecast_ahead)
      plot(pred, col.pred = "#2563eb")
    }, error = function(e) plot(1, type = "n", main = paste("Error:", e$message)))
  }, bg = "#161b27")
  
  output$forecast_text <- renderUI({
    HTML(paste0("
    <div class='texto'>
    Pronóstico VAR a <b>", input$forecast_ahead, " días</b>.
    Las bandas representan intervalos de confianza al 95%.
    El VAR captura interdependencias dinámicas; útil para análisis de escenarios.
    </div>
    "))
  })
  
  output$arima_forecast_plot <- renderPlot({
    serie <- na.omit(dlog_diario[, input$accion])
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5",
        col.lab = "#c9d1d9", col.main = "#ffffff", mar = c(4, 4, 3, 1))
    tryCatch({
      fit_arima <- auto.arima(as.numeric(serie), seasonal = FALSE)
      fc <- forecast(fit_arima, h = input$forecast_ahead)
      plot(fc, main = paste("ARIMA —", input$accion),
           col = "#2563eb", fcol = "#f59e0b", flwd = 2)
    }, error = function(e) {
      plot(1, type = "n", main = paste("Error ARIMA:", e$message))
    })
  }, bg = "#161b27")
  
  output$arima_text <- renderUI({
    HTML("
    <div class='texto'>
    auto.arima selecciona el mejor modelo ARIMA univariado por AICc.
    El pronóstico puntual tiende a la media; las bandas se amplían con el horizonte.
    </div>
    ")
  })
  
  output$forecast_stats <- renderPrint({
    mod <- modelo_var()
    if (is.null(mod)) {
      cat("Modelo no disponible.\n")
      return()
    }
    tryCatch({
      pred <- predict(mod, n.ahead = input$forecast_ahead)
      cat("=== Pronóstico", input$forecast_ahead, "días ===\n\n")
      print(pred$fcst[[input$accion]])
    }, error = function(e) cat("Error:", e$message))
  })
  
  output$forecast_stats_text <- renderUI({
    HTML("
    <div class='texto'>
    'fcst' = valor central; 'lower/upper' = IC 95%.
    El CI se amplía exponencialmente; pronósticos > 20 días tienen alta incertidumbre.
    </div>
    ")
  })
  
  # =====================================================================
  # COMPARATIVO
  # =====================================================================
  
  output$compare_plot <- renderPlotly({
    df_long <- pivot_longer(dlog_df, -fecha, names_to = "Serie", values_to = "Rdto")
    p <- ggplot(df_long, aes(fecha, Rdto, color = Serie)) +
      geom_line(linewidth = 0.5, alpha = 0.8) +
      facet_wrap(~Serie, ncol = 2, scales = "free_y") +
      labs(x = NULL, y = "Rendimiento Diario") +
      theme_minimal(base_size = 10) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        strip.background = element_rect(fill = "#1c2333"),
        strip.text = element_text(color = "#c9d1d9"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5"),
        legend.position = "none"
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$compare_text <- renderUI({
    HTML("
    <div class='texto'>
    Panel comparativo de rendimientos diarios.
    BANORTE y CEMEX muestran mayor sensibilidad a ciclos macroeconómicos;
    WALMEX y BIMBO mayor defensividad.
    </div>
    ")
  })
  
  output$stats_table <- DT::renderDataTable({
    stats <- do.call(rbind, lapply(c(acciones, "IPC"), function(nm) {
      r <- na.omit(dlog_df[[nm]])
      data.frame(
        Variable    = nm,
        Media       = round(mean(r) * 252 * 100, 2),
        Volatilidad = round(sd(r) * sqrt(252) * 100, 2),
        Min         = round(min(r) * 100, 2),
        Max         = round(max(r) * 100, 2),
        Asimetría   = round(e1071::skewness(r), 3),
        Curtosis    = round(e1071::kurtosis(r), 3),
        Sharpe      = round((mean(r) * 252 - 0.10) / (sd(r) * sqrt(252)), 3)
      )
    }))
    DT::datatable(stats, rownames = FALSE, style = "bootstrap4",
                  options = list(pageLength = 10, dom = "t"))
  })
  
  output$boxplot_returns <- renderPlotly({
    df_long <- pivot_longer(dlog_df[, c("fecha", acciones, "IPC")],
                            -fecha, names_to = "Serie", values_to = "Rdto")
    p <- ggplot(df_long, aes(Serie, Rdto * 100, fill = Serie)) +
      geom_boxplot(alpha = 0.8, outlier.size = 0.5, outlier.alpha = 0.4) +
      labs(x = NULL, y = "Rendimiento Diario (%)") +
      theme_minimal(base_size = 10) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5"),
        legend.position = "none"
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"), showlegend = FALSE)
  })
  
  output$scatter_matrix_plot <- renderPlotly({
    mat_c <- dlog_df[, c(acciones, "IPC")]
    p <- plotly::plot_ly()
    vars_plot <- c(acciones, "IPC")
    n <- length(vars_plot)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) {
          p <- add_histogram(p, x = mat_c[[vars_plot[i]]],
                             xaxis = paste0("x", i), yaxis = paste0("y", j),
                             marker = list(color = "#2563eb"), opacity = 0.7)
        } else {
          p <- add_markers(p, x = mat_c[[vars_plot[j]]], y = mat_c[[vars_plot[i]]],
                           xaxis = paste0("x", j), yaxis = paste0("y", i),
                           marker = list(size = 2, color = "#2563eb", opacity = 0.3))
        }
      }
    }
    p %>% layout(
      title = list(text = "Scatter Matrix — Rendimientos", font = list(color = "#c9d1d9")),
      paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
      font = list(color = "#c9d1d9"), showlegend = FALSE
    )
  })
  
  # =====================================================================
  # SIMULADOR DE DESPLOME
  # =====================================================================
  
  sim_resultado <- eventReactive(input$sim_run, ignoreNULL = TRUE, {
    accion_shock  <- input$sim_accion
    shock_pct     <- input$sim_shock / 100
    dias          <- as.integer(input$sim_dias)
    
    vars_todos <- c(acciones, "IPC")
    mat_ret    <- dlog_df[, vars_todos]
    
    if (!(accion_shock %in% names(mat_ret))) {
      return(list(error = paste("Variable no encontrada:", accion_shock)))
    }
    
    correlaciones <- vapply(vars_todos, function(v) {
      if (v == accion_shock) return(1.0)
      cor(mat_ret[[v]], mat_ret[[accion_shock]], use = "complete.obs")
    }, numeric(1))
    
    impacto_directo <- correlaciones * shock_pct
    vols <- vapply(vars_todos, function(v) sd(mat_ret[[v]], na.rm = TRUE), numeric(1))
    
    set.seed(2025)
    N_sim <- 300 
    paths <- list()
    
    for (v in vars_todos) {
      mu_v    <- mean(mat_ret[[v]], na.rm = TRUE)
      sigma_v <- sd(mat_ret[[v]],  na.rm = TRUE)
      rho_v   <- correlaciones[[v]]
      
      precio_post_shock <- 100 * (1 + shock_pct * rho_v)
      
      sims      <- matrix(NA_real_, nrow = dias + 1L, ncol = N_sim)
      sims[1, ] <- 100 
      
      if(dias >= 1) sims[2, ] <- precio_post_shock   
      
      if(dias >= 2) {
        for (d in 3:(dias + 1)) {
          innov       <- rnorm(N_sim, mean = mu_v, sd = sigma_v)
          sims[d, ]   <- sims[d - 1L, ] * exp(innov)
        }
      }
      paths[[v]] <- sims
    }
    
    list(
      accion_shock  = accion_shock,
      shock_pct     = shock_pct,
      dias          = dias,
      correlaciones = correlaciones,
      impacto       = impacto_directo,
      vols          = vols,
      paths         = paths,
      vars_todos    = vars_todos,
      error         = NULL
    )
  })
  
  output$sim_kpis_vertical <- renderUI({
    req(input$sim_run > 0)
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res) || !is.null(res$error)) {
      return(tags$div(
        style = "color: #b0bec5; font-size: 14px; padding: 20px; text-align: center;",
        "⚙️ Ejecuta la simulación para calcular y clasificar las pérdidas estimadas."
      ))
    }
    
    impactos_pct <- res$impacto * 100
    orden_peor_a_mejor <- names(sort(impactos_pct)) 
    
    kpis <- lapply(orden_peor_a_mejor, function(v) {
      imp  <- round(impactos_pct[v], 2)
      col  <- ifelse(imp <= -10, "red", ifelse(imp <= -3, "orange", ifelse(imp < 0, "yellow", "green")))
      
      fluidRow(
        valueBox(
          paste0(imp, "%"),
          paste("Impacto en", v),
          color = col,
          icon = icon(ifelse(imp < 0, "arrow-down", "arrow-up")),
          width = 12 
        )
      )
    })
    
    do.call(tagList, kpis)
  })
  
  output$sim_trayectoria_plot <- renderPlotly({
    validate(need(input$sim_run > 0, "Configura los parámetros y presiona 'Propagar Choque Estructural'..."))
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res) || !is.null(res$error)) return(NULL)
    
    dias_vec <- 0:res$dias
    
    df_list <- lapply(res$vars_todos, function(v) {
      mediana <- as.numeric(apply(res$paths[[v]], 1, median, na.rm = TRUE))
      data.frame(dia = dias_vec, variable = v, mediana = mediana)
    })
    df_all <- do.call(rbind, df_list)
    
    df_wide <- pivot_wider(df_all, names_from = variable, values_from = mediana)
    
    p <- plot_ly(df_wide, x = ~dia)
    
    colores <- c("AMX" = "#ef4444", "BANORTE" = "#3b82f6", "WALMEX" = "#ec4899",
                 "BIMBO" = "#eab308", "FEMSA" = "#06b6d4", "CEMEX" = "#22c55e", "IPC" = "#d946ef")
    
    for (v in res$vars_todos) {
      is_shock <- (v == res$accion_shock)
      w <- ifelse(is_shock, 4, 1.5)
      
      p <- p %>% add_lines(
        y = df_wide[[v]] - 100, 
        name = v,
        line = list(color = colores[[v]], width = w, shape = "spline"),
        hovertemplate = paste0("<b>", v, "</b>: %{y:.2f}%<extra></extra>")
      )
    }
    
    p <- p %>% add_lines(
      y = rep(0, nrow(df_wide)), 
      name = "Base Pre-Shock",
      line = list(color = "#4b5563", width = 1.5, dash = "dot"),
      hoverinfo = "skip",
      showlegend = FALSE
    )
    
    p <- p %>% layout(
      paper_bgcolor = "#161b27", 
      plot_bgcolor  = "#161b27",
      font = list(color = "#c9d1d9"),
      hovermode = "x unified",
      xaxis = list(
        title = "Días Transcurridos", 
        gridcolor = "#21273d", 
        zerolinecolor = "#21273d"
      ),
      yaxis = list(
        title = "Efecto Estructural Acumulado (%)", 
        gridcolor = "#21273d", 
        zerolinecolor = "#21273d"
      ),
      legend = list(
        title = list(text = "<b>Activo</b>"),
        orientation = "v", 
        x = 1.02, y = 0.9
      ),
      margin = list(r = 100) 
    )
    p
  })
  
  output$sim_trayectoria_text <- renderUI({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res)) return(HTML("<div class='texto'>Ejecuta la simulación.</div>"))
    HTML(paste0("
    <div class='texto'>
    Simulación Monte Carlo (N=300 trayectorias). El gráfico expone las <b>medianas consolidadas</b> 
    de impacto sistémico ante un colapso del <b>", round(res$shock_pct * 100, 1), "%</b> en 
    <b>", res$accion_shock, "</b>. <br>Pasa el cursor sobre las líneas para comparar 
    exactamente cuánto valor relativo han perdido todos los activos en un día en específico.
    </div>
    "))
  })
  
  output$sim_impacto_plot <- renderPlotly({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res) || !is.null(res$error)) return(NULL)
    
    df_imp <- data.frame(
      variable = names(res$impacto),
      impacto  = res$impacto * 100
    ) %>%
      arrange(impacto)
    
    df_imp$color <- ifelse(df_imp$impacto < -10, "#ef4444",
                           ifelse(df_imp$impacto < -3, "#f59e0b",
                                  ifelse(df_imp$impacto >= 0, "#22c55e", "#f59e0b")))
    
    df_imp$variable <- factor(df_imp$variable, levels = df_imp$variable)
    
    p <- ggplot(df_imp, aes(variable, impacto, fill = color)) +
      geom_col(width = 0.7) +
      scale_fill_identity() +
      geom_hline(yintercept = 0, color = "#4b5563") +
      coord_flip() +
      labs(x = NULL, y = "Impacto Estimado (%)",
           title = paste("Contagio desde", res$accion_shock)) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"), showlegend = FALSE)
  })
  
  output$sim_impacto_text <- renderUI({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res)) return(HTML("<div class='texto'>—</div>"))
    HTML(paste0("
    <div class='texto'>
    Impacto estimado = correlación histórica × magnitud del shock.
    Activos con alta correlación con <b>", res$accion_shock,
                "</b> sufren mayor contagio. El IPC absorbe el promedio ponderado del mercado.
    </div>
    "))
  })
  
  output$sim_irf_plot <- renderPlot({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    mod_full <- modelo_var_full()
    if (is.null(mod_full)) return(NULL)
    vars_mod <- colnames(mod_full$y)
    imp <- if (res$accion_shock %in% vars_mod) res$accion_shock else vars_mod[1]
    
    dias_sim <- as.integer(res$dias)
    imp_val  <- as.character(imp)
    
    par(bg = "#161b27", fg = "#c9d1d9", col.axis = "#b0bec5",
        col.lab = "#c9d1d9", col.main = "#ffffff")
    tryCatch({
      modelo_irf_sim <- irf(mod_full, impulse = imp_val, response = "IPC",
                            n.ahead = dias_sim, boot = TRUE, runs = 100)
      plot(modelo_irf_sim,
           main = paste("IRF: Shock en", imp_val, "→ IPC"))
    }, error = function(e) {
      plot(1, type = "n", main = paste("Error IRF:", e$message))
    })
  }, bg = "#161b27")
  
  output$sim_irf_text <- renderUI({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res)) return(HTML("<div class='texto'>—</div>"))
    HTML(paste0("
    <div class='texto'>
    IRF estimado del VAR sistémico: respuesta del IPC ante un shock de 1 desv. est. en
    <b>", res$accion_shock, "</b>. El efecto suele disiparse en 5–15 días.
    </div>
    "))
  })
  
  output$sim_tabla <- DT::renderDataTable({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res) || !is.null(res$error)) {
      return(DT::datatable(data.frame(Mensaje = "Ejecuta la simulación primero."),
                           rownames = FALSE))
    }
    
    dias_final <- res$dias
    
    df_tab <- do.call(rbind, lapply(res$vars_todos, function(v) {
      mat     <- res$paths[[v]]
      precio_final <- apply(mat, 2, function(col) col[dias_final + 1])
      mediana <- round(median(precio_final), 2)
      p05_f   <- round(quantile(precio_final, 0.05), 2)
      p95_f   <- round(quantile(precio_final, 0.95), 2)
      cambio  <- round(mediana - 100, 2)
      
      data.frame(
        Variable        = v,
        `Impacto Dir. (%)` = round(res$impacto[v] * 100, 2),
        `Correlación`   = round(res$correlaciones[v], 3),
        `Precio Final (Med.)` = mediana,
        `IC 5%`         = p05_f,
        `IC 95%`        = p95_f,
        `Cambio vs Pre-shock` = paste0(ifelse(cambio >= 0, "+", ""), cambio, "%")
      )
    }))
    
    DT::datatable(df_tab, rownames = FALSE, style = "bootstrap4",
                  options = list(pageLength = 10, dom = "t")) %>%
      DT::formatStyle(
        "Impacto.Dir....",
        color = DT::styleInterval(c(-10, -3, 0), c("#ef4444", "#f59e0b", "#b0bec5", "#22c55e"))
      )
  })
  
  output$sim_tabla_text <- renderUI({
    HTML("
    <div class='texto'>
    Tabla resumen: impacto directo estimado (correlación × shock), correlación histórica
    con el activo afectado, precio indexado al final del periodo de propagación (mediana
    de 500 simulaciones) e intervalos de confianza.
    </div>
    ")
  })
  
  output$sim_hist_plot <- renderPlotly({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res) || !is.null(res$error)) return(NULL)
    ac <- res$accion_shock
    if (!(ac %in% names(dlog_df)) || !("IPC" %in% names(dlog_df))) return(NULL)
    
    r_ac  <- dlog_df[[ac]]
    r_ipc <- dlog_df[["IPC"]]
    n_obs <- length(r_ac)
    ventana <- 60
    
    corr_rod <- zoo::rollapply(
      data = data.frame(r_ac, r_ipc),
      width = ventana,
      FUN   = function(x) cor(x[, 1], x[, 2]),
      by.column = FALSE,
      fill  = NA, align = "right"
    )
    
    df_cr <- data.frame(
      fecha  = dlog_df$fecha,
      correl = as.numeric(corr_rod),
      r_ac   = r_ac
    )
    
    p <- ggplot(df_cr, aes(fecha)) +
      geom_line(aes(y = r_ac * 100), color = "#4b5563", linewidth = 0.5, alpha = 0.6) +
      geom_line(aes(y = correl * 30), color = "#f59e0b", linewidth = 1.2) +
      geom_hline(yintercept = 0, color = "#21273d") +
      scale_y_continuous(
        name = paste("Rdto Diario —", ac, "(%)"),
        sec.axis = sec_axis(~ . / 30, name = paste("Corr. Rodante 60d con IPC"))
      ) +
      labs(x = NULL,
           title = paste("Correlación Rodante 60d:", ac, "↔ IPC")) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#161b27", color = NA),
        panel.background = element_rect(fill = "#161b27", color = NA),
        panel.grid.major = element_line(color = "#21273d"),
        text = element_text(color = "#c9d1d9"), axis.text = element_text(color = "#b0bec5"),
        axis.title.y.right = element_text(color = "#f59e0b"),
        axis.text.y.right = element_text(color = "#f59e0b")
      )
    ggplotly(p) %>%
      layout(paper_bgcolor = "#161b27", plot_bgcolor = "#161b27",
             font = list(color = "#c9d1d9"))
  })
  
  output$sim_hist_text <- renderUI({
    res <- tryCatch(sim_resultado(), error = function(e) NULL)
    if (is.null(res)) return(HTML("<div class='texto'>—</div>"))
    HTML(paste0("
    <div class='texto'>
    Correlación rodante de 60 días entre <b>", res$accion_shock,
                "</b> y el IPC. Periodos de correlación alta (cercana a 1) indican
    mayor riesgo sistemático: un shock en la acción se transfiere más
    intensamente al mercado. Periodos de baja correlación o negativa
    representan comportamiento idiosincrático.
    </div>
    "))
  })
  
} # cierre server

# =====================================================================
# LANZAR APP
# =====================================================================

shinyApp(ui = ui, server = server)