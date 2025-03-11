library(shiny)
library(plotly)
library(dplyr)

# Asegurar que los caracteres especiales se manejen correctamente
df$Activo <- iconv(df$Activo, from = "UTF-8", to = "latin1")

# Definir la interfaz de usuario (UI) de Shiny
ui <- fluidPage(
  titlePanel("Gráfica interactiva de riesgos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("escenario", "Selecciona el escenario:", choices = unique(df$escenario)),
      selectInput("plazo", "Selecciona la temporalidad:", choices = unique(df$plazo)),
      selectInput("activo", "Selecciona el Activo:", choices = unique(df$Activo))
    ),
    mainPanel(
      plotlyOutput("grafica"),
      tableOutput("tabla_riesgos")
    )
  )
)

# Definir el servidor (Server) de Shiny
server <- function(input, output) {
  datos_filtrados <- reactive({
    df %>%
      filter(escenario == input$escenario, 
             plazo == input$plazo, 
             Activo == input$activo)
  })
  
  datos_agrupados <- reactive({
    datos_filtrados() %>%
      group_by(Activo, Evaluación) %>%
      summarise(cantidad = n(), .groups = 'drop')
  })
  
  output$grafica <- renderPlotly({
    datos <- datos_agrupados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay datos para mostrar"))
    }
    
    colores <- c(
      "Muy alto" = "#ff9999",
      "Alto" = "#f8cbad",
      "Medio" = "#ffe699",
      "Bajo" = "#c6e0b4",
      "Muy bajo" = "#00cc99"
    )
    
    plot_ly(datos, x = ~Activo, y = ~cantidad, color = ~Evaluación, colors = colores, type = "bar") %>%
      layout(
        title = paste("Riesgos para", input$escenario, "(", input$plazo, ") -", input$activo),
        xaxis = list(title = "Ubicación (Activo)"),
        yaxis = list(title = "Cantidad de riesgos"),
        barmode = "stack"
      )
  })
  
  output$tabla_riesgos <- renderTable({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) return(data.frame(Evaluación = character(), Riesgos = character()))
    
    datos %>%
      group_by(Evaluación) %>%
      summarise(Riesgos = paste(unique(riesgo), collapse = ", "), .groups = 'drop')
  })
}

shinyApp(ui = ui, server = server)
