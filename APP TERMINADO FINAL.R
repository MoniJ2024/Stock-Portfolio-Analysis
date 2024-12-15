library(quantmod)
library(PerformanceAnalytics)
library(DT)
library(tseries)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(moments)  # Agregamos la librería moments

# Reemplaza la ruta del archivo Excel con la ubicación de tu archivo
ruta_excel <- "C:/Users/Monica Jimeno/OneDrive - upb.edu/UPB/OCTAVO SEMESTRE/GESTIÓN DE RIESGOS EN EIF/3ER PARCIAL/database of stocks.xlsx"
base_datos <- readxl::read_excel(ruta_excel)

obtener_simbolo <- function(nombre_compania, datos) {
  nombre_compania <- tolower(nombre_compania)
  fila <- agrepl(nombre_compania, tolower(datos$Company), ignore.case = TRUE)
  if (any(fila)) {
    return(unique(datos$Symbol[fila])[1])
  } else {
    return(NULL)
  }
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .mi-panel-personalizado {
        background-color: blue;
      }
    "))
  ),
  titlePanel("Análisis de Portafolio de Acciones"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("nombreCompania", "Nombre de la Compañía",
                  choices = base_datos$Company, options = list('live-search' = TRUE)),
      actionButton("buscarSymbol", "Buscar Ticket"),
      HTML('<style>.output_resultadoSymbol {font-size: 18px;}</style>'),
      verbatimTextOutput("resultadoSymbol"),
      textInput("ticker", "Ticker del Activo", value = "AAPL"),
      numericInput("peso", "Peso del Activo", value = 0.1, min = 0, max = 1, step = 0.01),
      dateRangeInput("rangoFechas", "Rango de Fechas",
                     start = Sys.Date() - 365, end = Sys.Date(),
                     separator = " - ",
                     format = "yyyy-mm-dd"),
      selectInput("periodicidad", "Periodicidad",
                  choices = c("diaria" = "daily", "semanal" = "weekly", "mensual" = "monthly")),
      actionButton("add", "Añadir Activo"),
      numericInput("valorPortafolio", "Valor del Portafolio", value = 10000, step = 1000),
      actionButton("calcularJarqueBera", "Calcular Jarque-Bera"),
      actionButton("calcularRetornoyRiesgo", "Calcular Retorno y Riesgo"),
      actionButton("analisisPortafolio", "Análisis Portafolio"),
      actionButton("limpiar", "Limpiar Datos"),
      hr(),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico",
                 DTOutput("tablaActivos"),
                 plotOutput("graficoRetornos")
        ),
        tabPanel("Jarque-Bera",
                 DTOutput("tablaJarqueBera"),
        ),        
        tabPanel("Análisis Retorno y Riesgo",
                 DTOutput("tablaRiesgoSharpe"),
                 DTOutput("tablaVaR"),
                 DTOutput("tablaES"),
                 DTOutput("tablaSemiDev")
        ),
        tabPanel("Portafolio",
                 numericInput("Rf", "Rf", value = 0.04, step = 0.0001),
                 plotOutput("graficoPortafolio"),
                 DTOutput("tablaVaRPortafolio"),
                 textOutput("rendimientoPortafolio"),
                 textOutput("riesgoPortafolio"),
                 textOutput("ratiodeSharpe"),
                 textOutput("ratiodeSortino") 
        )
      )
    )
  )
)

server <- function(input, output, session) {
  activos <- reactiveVal(data.frame(Ticker = character(), Peso = numeric()))
  datosActivos <- reactiveVal(list())
  jarque_result <- reactiveVal(data.frame())
  Vector_r_portafolio <- numeric()
  observeEvent(input$add, {
    rango_fechas <- input$rangoFechas
    activo <- getSymbols(input$ticker, src = 'yahoo', 
                         from = rango_fechas[1], 
                         to = rango_fechas[2],
                         periodicity = input$periodicidad, auto.assign = FALSE)
    if (NCOL(activo) > 0) {
      activos(rbind(activos(), data.frame(Ticker = input$ticker, Peso = input$peso)))
      datosActivos(c(datosActivos(), list(Ad(activo))))
      jarque_result_temp <- jarque.test(as.vector(Return.calculate(Ad(activo))[-1]))
      new_row <- data.frame(Ticker = input$ticker, 
                            Estadistico_JB = round(jarque_result_temp$statistic, 4),
                            P_valor_JB = round(jarque_result_temp$p.value, 4))
      jarque_result(rbind(jarque_result(), new_row))
    }
  })
  
  output$tablaActivos <- renderDT({
    activos()
  }, options = list(lengthChange = FALSE, searching = FALSE))
  
  output$graficoRetornos <- renderPlot({
    if (nrow(activos()) > 0) {
      Portafolio <- do.call(merge, datosActivos())
      R_portafolio <- Return.calculate(Portafolio)
      chart.CumReturns(R_portafolio, wealth.index = TRUE, legend = "topright")
    }
  })
  
  observeEvent(input$calcularRetornoyRiesgo, {
    if (nrow(activos()) > 0) {
      Portafolio <- do.call(merge, datosActivos())
      R_portafolio <- Return.calculate(Portafolio)[-1]
      Tabla_R_D_Sharpe <- table.AnnualizedReturns(R_portafolio, scale = 252, Rf = 0.04/252)
      output$tablaRiesgoSharpe <- renderDT({
        Tabla_R_D_Sharpe
      }, options = list(searching = FALSE))
    }
    if (nrow(activos()) > 0) {
      Portafolio <- do.call(merge, datosActivos())
      R_portafolio <- Return.calculate(Portafolio)[-1]
      
      VaR <- VaR(R_portafolio, p = 0.01)
      ES <- ES(R_portafolio, p = 0.01)
      Semi_Dev <- SemiDeviation(R_portafolio)
      
      output$tablaVaR <- renderDT({ round(data.frame(VaR), 4) }, options = list(searching = FALSE))
      output$tablaES <- renderDT({ round(data.frame(ES), 4) }, options = list(searching = FALSE))
      output$tablaSemiDev <- renderDT({ round(data.frame(Semi_Dev), 4) }, options = list(searching = FALSE, pageLength = 5))
    }
    
  })
  
  observeEvent(input$calcularJarqueBera, {
    if (nrow(activos()) > 0) {
      Portafolio <- do.call(merge, datosActivos())
      R_portafolio <- Return.calculate(Portafolio)[-1]
      jarque_result_temp <- jarque.test(as.vector(R_portafolio))
      new_row <- data.frame(Ticker = "Portafolio", 
                            Estadistico_JB = round(jarque_result_temp$statistic, 4),
                            P_valor_JB = round(jarque_result_temp$p.value, 4))
      jarque_result(rbind(jarque_result(), new_row))
    }
  })
  
  output$tablaJarqueBera <- renderDT({
    jarque_result()
  }, options = list(searching = FALSE))
  
  observeEvent(input$limpiar, {
    activos(data.frame(Ticker = character(), Peso = numeric()))
    datosActivos(list())
    jarque_result(data.frame())
    
    output$tablaActivos <- renderDT({ activos() }, options = list(lengthChange = FALSE, searching = FALSE))
    output$graficoRetornos <- renderPlot({ NULL })
    output$tablaRiesgoSharpe <- renderDT({ data.frame() }, options = list(searching = FALSE))
    output$tablaVaR <- renderDT({ data.frame() }, options = list(searching = FALSE))
    output$tablaES <- renderDT({ data.frame() }, options = list(searching = FALSE))
    output$tablaSemiDev <- renderDT({ data.frame() }, options = list(searching = FALSE))
    output$tablaJarqueBera <- renderDT({ data.frame() }, options = list(searching = FALSE))
    output$graficoPortafolio <- renderPlot({
      plot(Vector_r_portafolio, type = 'l', col = 'purple', 
           main = 'Retornos del Portafolio', 
           sub = 'Análisis de tendencias y variaciones', 
           xlab = 'Tiempo', ylab = 'Retornos', 
           las = 1, col.axis = 'grey50', col.lab = 'grey50',
           col.main = 'black', col.sub = 'grey50',
           lwd = 2, lty = 1)
      grid(nx = NULL, ny = NULL, col = 'lightgray', lty = 'dotted')
      abline(h = 0, col = 'red', lty = 2)
      # Aquí puedes agregar más elementos como puntos, anotaciones, etc.
    })
    Vector_r_portafolio_clean <- na.omit(Vector_r_portafolio)
    VaR_SH_porcentaje <- quantile(Vector_r_portafolio_clean, 0.01, na.rm = TRUE)
    VaR_SH_dolares <- VaR_SH_porcentaje * input$valorPortafolio
    
    output$tablaVaRPortafolio <- renderDT({ round(data.frame(VaR_Porcentaje = VaR_SH_porcentaje, VaR_Dolares = VaR_SH_dolares), 4) }, options = list(searching = FALSE))
  })
  
  observeEvent(input$analisisPortafolio, {
    if (nrow(activos()) > 0) {
      Portafolio <- do.call(merge, datosActivos())
      R_portafolio <- Return.calculate(Portafolio)[-1]
      
      Vector_r_portafolio <<- rowSums(input$peso * R_portafolio, na.rm = TRUE)
      
      output$graficoPortafolio <- renderPlot({
        plot(Vector_r_portafolio, type = 'l', col = 'purple', 
             main = 'Retornos del Portafolio', 
             sub = 'Análisis de tendencias y variaciones', 
             xlab = 'Tiempo', ylab = 'Retornos', 
             las = 1, col.axis = 'grey50', col.lab = 'grey50',
             col.main = 'black', col.sub = 'grey50',
             lwd = 2, lty = 1)
        grid(nx = NULL, ny = NULL, col = 'lightgray', lty = 'dotted')
        abline(h = 0, col = 'red', lty = 2)
        # Aquí puedes agregar más elementos como puntos, anotaciones, etc.
      })
      Vector_r_portafolio_clean <- na.omit(Vector_r_portafolio)
      VaR_SH_porcentaje <- quantile(Vector_r_portafolio_clean, 0.01, na.rm = TRUE)
      VaR_SH_dolares <- VaR_SH_porcentaje * input$valorPortafolio
      
      output$tablaVaRPortafolio <- renderDT({ round(data.frame(VaR_Porcentaje = VaR_SH_porcentaje, VaR_Dolares = VaR_SH_dolares), 4) }, options = list(searching = FALSE))
    }
    Portafolio <- do.call(merge, datosActivos())
    R_portafolio <- Return.calculate(Portafolio)[-1]
    
    # Usa solo la columna de pesos de 'activos'
    v_pesos <- matrix(activos()$Peso, nrow = 1)
    
    matriz_var_cov <- cov(R_portafolio)
    var_portafolio <- v_pesos %*% matriz_var_cov %*% t(v_pesos)
    
    riesgo_portafolio <- sqrt(as.numeric(var_portafolio))
    riesgo_portafolio_1 <- riesgo_portafolio*100
    riesgo_portafolio_1 <- round(riesgo_portafolio_1, 4)
    rendimiento_portafolio <- mean(R_portafolio, na.rm = TRUE)
    rendimiento_portafolio_1 <-  rendimiento_portafolio*100
    rendimiento_portafolio_1 <- round(rendimiento_portafolio_1, 4)
    Rf <- input$Rf
    Ratio_Sharpe <- (rendimiento_portafolio - Rf) / riesgo_portafolio
    Ratio_Sharpe <- round(Ratio_Sharpe, 2)
    
    R_portafolio_negativos <- R_portafolio
    R_portafolio_negativos[R_portafolio_negativos >= 0] <- NA 
    
    R_portafolio_negativos <- na.omit(R_portafolio_negativos) 
    desv_est_negativos <- sqrt(mean((R_portafolio_negativos - mean(R_portafolio_negativos))^2, na.rm = TRUE))
    
    Ratio_Sortino <- (rendimiento_portafolio - Rf) / desv_est_negativos
    
    output$rendimientoPortafolio <- renderText({
      paste("Rendimiento Portafolio %: ", rendimiento_portafolio_1)
    })
    output$riesgoPortafolio <- renderText({
      paste("Riesgo Portafolio %: ", riesgo_portafolio_1)
    })
    output$ratiodeSharpe <- renderText({
      paste("Ratio de Sharpe: ", Ratio_Sharpe)
    })
    output$ratiodeSortino <- renderText({
      paste("Ratio de Sortino: ", Ratio_Sortino)
    })
    
  })
  
  observeEvent(input$buscarSymbol, {
    nombre_compania <- input$nombreCompania
    simbolo <- obtener_simbolo(nombre_compania, base_datos)
    
    if (!is.null(simbolo)) {
      shinyjs::enable("resultadoSymbol")
    } else {
      shinyjs::disable("resultadoSymbol")
    }
    
    output$resultadoSymbol <- renderText({
      simbolo
    })
  })
}

shinyApp(ui = ui, server = server)
