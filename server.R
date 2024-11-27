library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# Ruta al archivo local de Excel
file_path <- "diagnosticoGE.xlsx"  # Cambia el nombre al archivo que estás usando

# Lista de variables que requieren separación de valores
variables_separadas <- c(
  "Imagine que en esta empresa, un grupo de mujeres está hablando mal de otra mujer. ¿Cuál sería la razón por la que se hablaría mal de ella?",
  "Imagine que en esta empresa, un grupo de hombres se está burlando de otro hombre. ¿Cuál sería la razón por la que se estarían burlando de él?",
  "En mi infancia me educaron principalmente",
  "En mi infancia, la persona más presente en mi crianza fue:"
)

# Función genérica para gráficos cruzados por género (ajustada para variables con separación)
crear_grafico_genero <- function(data, variable, titulo, eje_x, colores = c("#c63018", "#3984a5", "#0065ba")) {
  # Separar valores en variables específicas
  if (variable %in% variables_separadas) {
    data <- data %>%
      separate_rows(!!sym(variable), sep = ",\\s*")
  }
  
  # Procesar los datos
  data_procesada <- data %>%
    group_by(`¿Cómo te identificas?...4`, !!sym(variable)) %>%
    summarise(Cantidad = n(), .groups = "drop") %>%
    group_by(`¿Cómo te identificas?...4`) %>%
    mutate(
      TotalGenero = sum(Cantidad),
      Porcentaje = (Cantidad / TotalGenero) * 100
    )
  
  # Crear el gráfico
  ggplot(data_procesada, aes(
    x = !!sym(variable),
    y = Porcentaje,
    fill = `¿Cómo te identificas?...4`
  )) +
    geom_bar(stat = "identity", position = "dodge", color = "white") +
    geom_text(
      aes(label = paste0(round(Porcentaje, 1), "%")),
      position = position_dodge(width = 0.9),
      vjust = -0.5
    ) +
    scale_fill_manual(values = colores) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(
      title = titulo,
      x = eje_x,
      y = "Porcentaje",
      fill = "Género"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Server del Dashboard
server <- function(input, output, session) {
  # Cargar datos desde el archivo Excel y seleccionar la pestaña "Datos_ajustados"
  datos <- reactiveVal(read_excel(file_path, sheet = "Datos_ajustados"))
  
  # Actualizar opciones del dropdown de empresas
  observe({
    updateSelectInput(session, "empresa", choices = unique(datos()[["¿Cuál es el nombre de la empresa a la que perteneces?"]]))
  })
  
  # Filtrar datos por empresa seleccionada
  datos_filtrados <- reactive({
    req(input$empresa)  # Asegura que se haya seleccionado una empresa
    datos() %>%
      filter(`¿Cuál es el nombre de la empresa a la que perteneces?` == input$empresa)
  })
  
  ### Pestaña 1: Tabla Resumen por Género
  output$tabla <- renderDT({
    datos_resumen <- datos_filtrados() %>%
      group_by(`¿Cómo te identificas?...4`) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    datatable(
      datos_resumen,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  ### Gráficos Dinámicos
  # Formación enfoque de género
  output$grafico_formacion_genero <- renderPlot({
    req(input$empresa)
    data_formacion <- datos_filtrados()
    crear_grafico_genero(
      data = data_formacion,
      variable = "¿Has recibido formación en enfoque de género en esta empresa?",
      titulo = paste("Formación en enfoque de género en", input$empresa),
      eje_x = "Formación recibida"
    )
  })
  
  # Sociodemográfico
  output$grafico_dinamico_socio <- renderPlot({
    req(input$variable_sociodemografico)
    data_socio <- datos_filtrados()
    crear_grafico_genero(
      data = data_socio,
      variable = input$variable_sociodemografico,
      titulo = paste("Cruce entre Género y", input$variable_sociodemografico),
      eje_x = input$variable_sociodemografico
    )
  })
  
  # Reporte 1
  output$grafico_dinamico <- renderPlot({
    req(input$variable_cruce)
    data_cruce <- datos_filtrados()
    crear_grafico_genero(
      data = data_cruce,
      variable = input$variable_cruce,
      titulo = paste("Cruce entre Género y", input$variable_cruce),
      eje_x = input$variable_cruce
    )
  })
  
  # Reporte de violencias
  output$grafico_violencias_dinamico <- renderPlot({
    req(input$variable_violencias)
    data_violencias <- datos_filtrados()
    crear_grafico_genero(
      data = data_violencias,
      variable = input$variable_violencias,
      titulo = paste("Cruce entre Género y", input$variable_violencias),
      eje_x = input$variable_violencias
    )
  })
  
  # Emociones
  output$grafico_emociones_dinamico <- renderPlot({
    req(input$variable_emociones)
    data_emociones <- datos_filtrados()
    crear_grafico_genero(
      data = data_emociones,
      variable = input$variable_emociones,
      titulo = paste("Cruce entre Género y", input$variable_emociones),
      eje_x = "Emociones"
    )
  })
  
  # Creencias
  output$grafico_creencias_dinamico <- renderPlot({
    req(input$variable_creencias)
    data_creencias <- datos_filtrados()
    crear_grafico_genero(
      data = data_creencias,
      variable = input$variable_creencias,
      titulo = paste("Cruce entre Género y", input$variable_creencias),
      eje_x = "Creencias"
    )
  })
  
  # Percepciones
  output$grafico_percepciones_dinamico <- renderPlot({
    req(input$variable_percepciones)
    data_percepciones <- datos_filtrados()
    crear_grafico_genero(
      data = data_percepciones,
      variable = input$variable_percepciones,
      titulo = paste("Cruce entre Género y", input$variable_percepciones),
      eje_x = "Percepciones"
    )
  })
  
  # Valores
  output$grafico_valores_dinamico <- renderPlot({
    req(input$variable_valores)
    data_valores <- datos_filtrados()
    crear_grafico_genero(
      data = data_valores,
      variable = input$variable_valores,
      titulo = paste("Cruce entre Género y", input$variable_valores),
      eje_x = "Afirmación"
    )
  })
  
  # Normas sociales
  output$grafico_normas_dinamico <- renderPlot({
    req(input$variable_normas)
    data_normas <- datos_filtrados()
    crear_grafico_genero(
      data = data_normas,
      variable = input$variable_normas,
      titulo = paste("Cruce entre Género y", input$variable_normas),
      eje_x = " "
    )
  })
}


