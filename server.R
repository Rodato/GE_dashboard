library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# Ruta al archivo local de Excel
file_path <- "diagnosticoGE.xlsx"  # Cambia el nombre al archivo que estás usando

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
  
  ###Pestaña 1
  # Tabla dinámica: Resumen filtrado de género por empresa
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
  
  ##Formación enfoque de género
  
  output$grafico_formacion_genero <- renderPlot({
    req(input$empresa)  # Asegura que se haya seleccionado una empresa
    
    # Filtrar datos por empresa seleccionada
    data_formacion <- datos_filtrados() %>%
      group_by(`¿Cómo te identificas?...4`, `¿Has recibido formación en enfoque de género en esta empresa?`) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_formacion, aes(x = `¿Has recibido formación en enfoque de género en esta empresa?`, 
                               y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste(" ", input$empresa),
        x = " ",
        y = " ",
        fill = "Género"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  
  ##Pestaña Sociodemográfico
  
  # Gráfico dinámico: Cruce entre género y variable seleccionada
  output$grafico_dinamico <- renderPlot({
    req(input$variable_cruce)  # Asegura que se haya seleccionado una variable
    
    # Filtrar y procesar los datos según la variable seleccionada
    data_cruce <- datos_filtrados()
    
    # Separar valores múltiples si la variable lo requiere
    if (input$variable_cruce %in% c("En mi infancia me educaron principalmente", "En mi infancia, la persona más presente en mi crianza fue:")) {
      data_cruce <- data_cruce %>%
        separate_rows(all_of(input$variable_cruce), sep = ",\\s*")
    }
    
    # Agrupar y calcular porcentajes
    data_cruce <- data_cruce %>%
      group_by(`¿Cómo te identificas?...4`, !!sym(input$variable_cruce)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_cruce, aes(x = !!sym(input$variable_cruce), y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Cruce entre Género y", input$variable_cruce),
        x = input$variable_cruce,
        y = "Porcentaje",
        fill = "Género"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  # Pestaña: Reporte de violencias
  
  # Gráfico dinámico: Cruce entre género y variable seleccionada en "Reporte de violencias"
  output$grafico_violencias_dinamico <- renderPlot({
    req(input$variable_violencias)  # Asegura que se haya seleccionado una variable
    
    # Filtrar y procesar los datos según la variable seleccionada
    data_cruce <- datos_filtrados()
    
    # Manejar casos donde la variable tiene valores separados por comas
    if (input$variable_violencias %in% c(
      "En medio de este conflicto, ¿ocurrieron alguna de las siguientes acciones?",
      "Podrías contarnos, ¿con quién se dio este conflicto?"
    )) {
      data_cruce <- data_cruce %>%
        separate_rows(all_of(input$variable_violencias), sep = ",\\s*")
    }
    
    # Agrupar y calcular porcentajes
    data_cruce <- data_cruce %>%
      group_by(`¿Cómo te identificas?...4`, !!sym(input$variable_violencias)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_cruce, aes(x = !!sym(input$variable_violencias), y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Cruce entre Género y", input$variable_violencias),
        x = input$variable_violencias,
        y = "Porcentaje",
        fill = "Género"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ##Pestaña Emociones
  output$grafico_emociones_dinamico <- renderPlot({
    req(input$variable_emociones)  # Asegura que se haya seleccionado una variable
    
    # Verificar si la columna seleccionada existe en los datos
    if (!input$variable_emociones %in% names(datos_filtrados())) {
      stop("La columna seleccionada no existe en los datos.")
    }
    
    # Filtrar y procesar los datos
    data_emociones <- datos_filtrados() %>%
      group_by(`¿Cómo te identificas?...4`, !!sym(input$variable_emociones)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_emociones, aes(x = !!sym(input$variable_emociones), y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Cruce entre Género y", input$variable_emociones),
        x = input$variable_emociones,
        y = " ",
        fill = " "
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  ##Pestaña Creencias
  
  output$grafico_creencias_dinamico <- renderPlot({
    req(input$variable_creencias)  # Asegura que se haya seleccionado una variable
    
    # Verificar si la columna seleccionada existe en los datos
    if (!input$variable_creencias %in% names(datos_filtrados())) {
      stop("La columna seleccionada no existe en los datos.")
    }
    
    # Filtrar y procesar los datos
    data_creencias <- datos_filtrados() %>%
      group_by(`¿Cómo te identificas?...4`, !!sym(input$variable_creencias)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_creencias, aes(x = !!sym(input$variable_creencias), y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Cruce entre Género y", input$variable_creencias),
        x = input$variable_creencias,
        y = "Porcentaje",
        fill = "Género"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  #Pestañas desconocimiento
  
  output$grafico_percepciones_dinamico <- renderPlot({
    req(input$variable_percepciones)  # Asegura que se haya seleccionado una variable
    
    # Verificar si la columna seleccionada existe en los datos
    if (!input$variable_percepciones %in% names(datos_filtrados())) {
      stop("La columna seleccionada no existe en los datos.")
    }
    
    # Filtrar y procesar los datos
    data_percepciones <- datos_filtrados() %>%
      group_by(`¿Cómo te identificas?...4`, !!sym(input$variable_percepciones)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_percepciones, aes(x = !!sym(input$variable_percepciones), y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Cruce entre Género y", input$variable_percepciones),
        x = input$variable_percepciones,
        y = "Porcentaje",
        fill = "Género"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ###Valores
  output$grafico_valores_dinamico <- renderPlot({
    req(input$variable_valores)  # Asegura que se haya seleccionado una variable
    
    # Verificar si la columna seleccionada existe en los datos
    if (!input$variable_valores %in% names(datos_filtrados())) {
      stop("La columna seleccionada no existe en los datos.")
    }
    
    # Filtrar y procesar los datos
    data_valores <- datos_filtrados() %>%
      group_by(`¿Cómo te identificas?...4`, !!sym(input$variable_valores)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_valores, aes(x = !!sym(input$variable_valores), y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Cruce entre Género y", input$variable_valores),
        x = "Afirmación",
        y = "Porcentaje",
        fill = "Género"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  ###Normas sociales
  
  output$grafico_dinamicas_dinamico <- renderPlot({
    req(input$variable_dinamicas)  # Asegura que se haya seleccionado una variable
    
    # Verificar si la columna seleccionada existe en los datos
    if (!input$variable_dinamicas %in% names(datos_filtrados())) {
      stop("La columna seleccionada no existe en los datos.")
    }
    
    # Filtrar y procesar los datos
    data_dinamicas <- datos_filtrados() %>%
      separate_rows(!!sym(input$variable_dinamicas), sep = ",\\s*") %>%  # Descomponer respuestas múltiples si aplica
      group_by(`¿Cómo te identificas?...4`, !!sym(input$variable_dinamicas)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)
    
    # Crear el gráfico
    ggplot(data_dinamicas, aes(x = !!sym(input$variable_dinamicas), y = Porcentaje, fill = `¿Cómo te identificas?...4`)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
                position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("#c63018", "#3984a5", "#0065ba")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Cruce entre Género y", input$variable_dinamicas),
        x = input$variable_dinamicas,
        y = "Porcentaje",
        fill = "Género"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
}

