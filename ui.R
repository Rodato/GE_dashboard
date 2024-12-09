library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Reporte de Resultados - Diagnóstico Programa Generando Equidad"),
  sidebarLayout(
    sidebarPanel(
      selectInput("empresa", "Selecciona una empresa:", choices = NULL),
      actionButton("actualizar", "Actualizar datos")
    ),
    mainPanel(
      tabsetPanel(
        # Pestaña: Formación enfoque de género
        tabPanel(
          "Formación",
          sidebarLayout(
            sidebarPanel(
              h4("Formación en enfoque de género")
            ),
            mainPanel(
              plotOutput("grafico_dinamico_formacion"),
              downloadButton("descargar_grafico_formacion", "Descargar Gráfico"),
              downloadButton("descargar_datos_formacion", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Sociodemográfico
        tabPanel(
          "Sociodemográfico",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_sociodemografico",
                "Selecciona la variable de cruce:",
                choices = c(
                  "grupos_etarios",
                  "¿Te consideras parte de la comunidad LGTBIQ+?",
                  "¿Cómo te identificas?...6"
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_socio"),
              downloadButton("descargar_grafico_socio", "Descargar Gráfico"),
              downloadButton("descargar_datos_socio", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Autoreporte
        tabPanel(
          "Autoreporte",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_cruce",
                "Selecciona la variable de cruce:",
                choices = c(
                  "¿Tienes personas a tu cargo?",
                  "¿Hay alguien en la empresa que te supervise o esté a cargo de ti?",
                  "En mi infancia me educaron principalmente",
                  "En mi infancia, la persona más presente en mi crianza fue:"
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_autoreporte"),
              downloadButton("descargar_grafico_autoreporte", "Descargar Gráfico"),
              downloadButton("descargar_datos_autoreporte", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Reporte de Violencias
        tabPanel(
          "Violencias",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_violencias",
                "Selecciona la variable de cruce:",
                choices = c(
                  "Durante la última semana, ¿tuviste conflictos con otras personas en esta empresa?",
                  "En medio de este conflicto, ¿ocurrieron alguna de las siguientes acciones?",
                  "Podrías contarnos, ¿con quién se dio este conflicto?",
                  "Esta persona tiene un cargo...",
                  "¿Cuál consideras que fue la principal causa que motivó este conflicto?"
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_violencias"),
              downloadButton("descargar_grafico_violencias", "Descargar Gráfico"),
              downloadButton("descargar_datos_violencias", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Emociones
        tabPanel(
          "Emociones",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_emociones",
                "Selecciona una variable para el análisis:",
                choices = c(
                  "Cuando pienso en mi relación con las compañeras mujeres de trabajo, siento:",
                  "Cuando pienso en mi relación con los compañeros hombres de trabajo, siento:"
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_emociones"),
              downloadButton("descargar_grafico_emociones", "Descargar Gráfico"),
              downloadButton("descargar_datos_emociones", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Creencias
        tabPanel(
          "Creencias",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_creencias",
                "Selecciona una afirmación para el análisis:",
                choices = c(
                  "En esta empresa toca hablar duro para hacerse respetar",
                  "Generalmente a las mujeres no se les puede decir nada porque son muy sensibles",
                  "Los hombres quisieran manejar mejor sus emociones pero no saben cómo",
                  "Es irrespetuoso que dos personas del mismo sexo se expresen afecto públicamente",
                  "Las mujeres y los hombres tienen habilidades distintas y por eso se complementan",
                  "Hoy en día, se exagera con la igualdad entre hombres y mujeres"
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_creencias"),
              downloadButton("descargar_grafico_creencias", "Descargar Gráfico"),
              downloadButton("descargar_datos_creencias", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Desconocimiento
        tabPanel(
          "Desconocimiento",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_percepciones",
                "Selecciona una afirmación para el análisis:",
                choices = c(
                  "A veces es necesario alzar la voz para que las personas hagan bien su trabajo",
                  "No hay que tomarse en serio los chistes pesados que se hagan en el trabajo porque no es tan grave.",
                  "Para tener buen desempeño laboral es mejor no traer los temas personales al trabajo.",
                  "Esparcir chismes sobre la vida personal de otra persona es una violencia.",
                  "Hoy en día las personas son tan sensibles, que a todo le dicen violencia."
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_percepciones"),
              downloadButton("descargar_grafico_percepciones", "Descargar Gráfico"),
              downloadButton("descargar_datos_percepciones", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Valores
        tabPanel(
          "Valores",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_valores",
                "Selecciona una afirmación para el análisis:",
                choices = c(
                  "Hoy en día se están perdiendo muchos valores al interior de la familia.",
                  "Actualmente, las mujeres están abandonando su responsabilidad con sus parejas y su familia"
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_valores"),
              downloadButton("descargar_grafico_valores", "Descargar Gráfico"),
              downloadButton("descargar_datos_valores", "Descargar Datos")
            )
          )
        ),
        
        # Pestaña: Normas Sociales
        tabPanel(
          "Normas sociales",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_normas",
                "Selecciona una variable para el análisis:",
                choices = c(
                  "Imagine que en esta empresa, un grupo de mujeres está hablando mal de otra mujer. ¿Cuál sería la razón por la que se hablaría mal de ella?",
                  "Imagine que en esta empresa, un grupo de hombres se está burlando de otro hombre. ¿Cuál sería la razón por la que se estarían burlando de él?",
                  "En esta empresa, escuchar atentamente es más común en",
                  "En esta empresa, comunicarse asertivamente es más común en:",
                  "En esta empresa, hacer de todo un conflicto es más común en:",
                  "En esta empresa hablar a espaldas de las demás personas es más común en:"
                )
              )
            ),
            mainPanel(
              plotOutput("grafico_dinamico_normas"),
              downloadButton("descargar_grafico_normas", "Descargar Gráfico"),
              downloadButton("descargar_datos_normas", "Descargar Datos")
            )
          )
        )
      )
    )
  )
)


