library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# Ruta al archivo local de Excel
file_path <- "diagnosticoGE.xlsx"

# Lista de variables que requieren separación de valores
variables_separadas <- c(
  "Imagine que en esta empresa, un grupo de mujeres está hablando mal de otra mujer. ¿Cuál sería la razón por la que se hablaría mal de ella?",
  "Imagine que en esta empresa, un grupo de hombres se está burlando de otro hombre. ¿Cuál sería la razón por la que se estarían burlando de él?",
  "En mi infancia me educaron principalmente",
  "En mi infancia, la persona más presente en mi crianza fue:"
)

# Función genérica para gráficos cruzados por género
crear_grafico_genero <- function(data, variable, titulo, eje_x, colores = c("#c63018", "#3984a5", "#0065ba")) {
  if (variable %in% variables_separadas) {
    data <- data %>% separate_rows(!!sym(variable), sep = ",\\s*")
  }
  
  data_procesada <- data %>%
    group_by(`¿Cómo te identificas?...4`, !!sym(variable)) %>%
    summarise(Cantidad = n(), .groups = "drop") %>%
    group_by(`¿Cómo te identificas?...4`) %>%
    mutate(
      TotalGenero = sum(Cantidad),
      Porcentaje = (Cantidad / TotalGenero) * 100
    )
  
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

# Función genérica para manejar la descarga de gráficos
crear_descargar_grafico <- function(output, id, plot_func) {
  output[[id]] <- downloadHandler(
    filename = function() {
      paste(id, Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_func(), device = "png", width = 10, height = 7)
    }
  )
}

# Función genérica para manejar la descarga de datos
crear_descargar_datos <- function(output, id, data_func) {
  output[[id]] <- downloadHandler(
    filename = function() {
      paste(id, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_func(), file, row.names = FALSE)
    }
  )
}

# Server del Dashboard
server <- function(input, output, session) {
  datos <- reactiveVal(read_excel(file_path, sheet = "Datos_ajustados"))
  
  # Actualizar opciones del dropdown de empresas
  observe({
    updateSelectInput(session, "empresa", choices = unique(datos()[["¿Cuál es el nombre de la empresa a la que perteneces?"]]))
  })
  
  datos_filtrados <- reactive({
    req(input$empresa)
    datos() %>%
      filter(`¿Cuál es el nombre de la empresa a la que perteneces?` == input$empresa)
  })
  
  # Especial: Pestaña de Formación
  output$grafico_dinamico_formacion <- renderPlot({
    variable <- "¿Has recibido formación en enfoque de género en esta empresa?"
    req(input$empresa)
    validate(
      need(any(!is.na(datos_filtrados()[[variable]])), "No se registraron respuestas para esta pregunta en el dataset.")
    )
    crear_grafico_genero(
      data = datos_filtrados(),
      variable = variable,
      titulo = "Formación en enfoque de género",
      eje_x = "Formación recibida"
    )
  })
  
  crear_descargar_grafico(output, "descargar_grafico_formacion", function() {
    crear_grafico_genero(
      data = datos_filtrados(),
      variable = "¿Has recibido formación en enfoque de género en esta empresa?",
      titulo = "Formación en enfoque de género",
      eje_x = "Formación recibida"
    )
  })
  
  crear_descargar_datos(output, "descargar_datos_formacion", function() {
    variable <- "¿Has recibido formación en enfoque de género en esta empresa?"
    datos_filtrados() %>%
      group_by(`¿Cómo te identificas?...4`, !!sym(variable)) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      group_by(`¿Cómo te identificas?...4`) %>%
      mutate(
        TotalGenero = sum(Cantidad),
        Porcentaje = (Cantidad / TotalGenero) * 100
      )
  })
  
  # Pestañas restantes
  pestañas <- list(
    list(
      id = "socio",
      variable = reactive({ input$variable_sociodemografico }),
      titulo = "Cruce entre Género y Variable Seleccionada",
      eje_x = "Variable Seleccionada"
    ),
    list(
      id = "autoreporte",
      variable = reactive({ input$variable_cruce }),
      titulo = "Cruce entre Género y Variable Seleccionada",
      eje_x = "Variable Seleccionada"
    ),
    list(
      id = "violencias",
      variable = reactive({ input$variable_violencias }),
      titulo = "Cruce entre Género y Variable Seleccionada en Reporte de Violencias",
      eje_x = "Variable Seleccionada"
    ),
    list(
      id = "emociones",
      variable = reactive({ input$variable_emociones }),
      titulo = "Emociones",
      eje_x = "Emociones"
    ),
    list(
      id = "creencias",
      variable = reactive({ input$variable_creencias }),
      titulo = "Cruce entre Género y Afirmación Seleccionada",
      eje_x = "Creencias"
    ),
    list(
      id = "percepciones",
      variable = reactive({ input$variable_percepciones }),
      titulo = "Desconocimiento",
      eje_x = "Percepciones"
    ),
    list(
      id = "valores",
      variable = reactive({ input$variable_valores }),
      titulo = "Valores",
      eje_x = "Valores"
    ),
    list(
      id = "normas",
      variable = reactive({ input$variable_normas }),
      titulo = "Cruce entre Género y Normas Sociales",
      eje_x = "Normas Sociales"
    )
  )
  
  for (pestaña in pestañas) {
    local({
      id <- pestaña$id
      variable <- pestaña$variable
      titulo <- pestaña$titulo
      eje_x <- pestaña$eje_x
      
      data <- reactive({
        req(variable())
        if (variable() %in% variables_separadas) {
          datos_filtrados() %>%
            separate_rows(!!sym(variable()), sep = ",\\s*")
        } else {
          datos_filtrados()
        } %>%
          group_by(`¿Cómo te identificas?...4`, !!sym(variable())) %>%
          summarise(Cantidad = n(), .groups = "drop") %>%
          group_by(`¿Cómo te identificas?...4`) %>%
          mutate(
            TotalGenero = sum(Cantidad),
            Porcentaje = (Cantidad / TotalGenero) * 100
          )
      })
      
      output[[paste0("grafico_dinamico_", id)]] <- renderPlot({
        req(variable())
        validate(
          need(any(!is.na(datos_filtrados()[[variable()]])), "No se registraron respuestas para esta pregunta en el dataset.")
        )
        crear_grafico_genero(
          data = datos_filtrados(),
          variable = variable(),
          titulo = titulo,
          eje_x = eje_x
        )
      })
      
      crear_descargar_grafico(output, paste0("descargar_grafico_", id), function() {
        crear_grafico_genero(
          data = datos_filtrados(),
          variable = variable(),
          titulo = titulo,
          eje_x = eje_x
        )
      })
      
      crear_descargar_datos(output, paste0("descargar_datos_", id), function() {
        data()
      })
    })
  }
}





