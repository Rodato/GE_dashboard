library(shiny)
library(DT)

# UI del Dashboard
ui <- fluidPage(
  titlePanel("Reporte de Resultados - Diagnóstico Programa Generando Equidad"),
  sidebarLayout(
    sidebarPanel(
      selectInput("empresa", "Selecciona una empresa:", choices = NULL),
      actionButton("actualizar", "Actualizar datos")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Tabla dinámica",
          fluidRow(
            column(
              width = 12,
              div(
                HTML("<p><strong>Descripción:</strong> Este tablero muestra un resumen descriptivo de los diagnósticos realizado en <em>Casa Luker, Cueros Vélez, y Sumasacha</em>.</p>")
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              DTOutput("tabla")  # La tabla dinámica
            ),
            column(
              width = 6,
              h3("Formación en Enfoque de Género"),
              plotOutput("grafico_formacion_genero")
            )
          )
        ),
        
        # Sociodemográico
        
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
              h3("Cruce entre Género y Variable Seleccionada"),
              plotOutput("grafico_dinamico_socio")
            )
          )
        ),
        
        
        ##Autoreporte 1
        
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
              h3("Cruce entre Género y Variable Seleccionada"),
              plotOutput("grafico_dinamico")
            )
          )
        ),
        
        #Reporte de Violencias
        
        tabPanel(
          "Autoreporte comportamental",
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
              h3("Cruce entre Género y Variable Seleccionada en Reporte de Violencias"),
              plotOutput("grafico_violencias_dinamico")
            )
          )
        ),
        
        ##Emociones
        
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
              h3("Emociones"),
              plotOutput("grafico_emociones_dinamico")
            )
          )
        ),
        
        #Creencias
        
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
                  "Hoy en día, se exagera con la igualdad entre hombres y mujeres",
                  "Si una mujer expresa su gusto por otras mujeres, es normal que sus compañeras se sientan incómodas frente a ella",
                  "En esta empresa, “se dan más duro” entre mujeres que entre hombres",
                  "En general, los hombres son menos emocionales que las mujeres",
                  "Es entendible que, en la empresa, los hombres comenten y se sientan atraídos frente a los cuerpos de las mujeres.",
                  "Si un hombre expresa su gusto por otros hombres, es normal que sus compañeros se sientan incómodos frente a él",
                  "Es normal que las mujeres sientan envidia por la belleza de otras mujeres",
                  "En general, el hombre propone y la mujer dispone",
                  "En general, los hombres son mejores líderes que las mujeres porque son más objetivos y menos emocionales",
                  "Los hombres no saben pedir ayuda cuando lo necesitan",
                  "En esta empresa, la mayoría de las personas trans se sienten tratadas equitativa e incluyentemente"
                )
              )
            ),
            mainPanel(
              h3("Cruce entre Género y Afirmación Seleccionada"),
              plotOutput("grafico_creencias_dinamico")
            )
          )
        ),
        
        ##Desconocimiento
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
              h3("Desconocimiento"),
              plotOutput("grafico_percepciones_dinamico")
            )
          )
        ),
        
        ##Valores
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
              h3("Valores"),
              plotOutput("grafico_valores_dinamico")
            )
          )
        ),
        
        
        #Dinámicas laborales
        
        tabPanel(
          "Normas sociales",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "variable_normas",
                "Selecciona una variable para el análisis:",
                choices = c(
                  "Imagine que en esta empresa, un grupo de mujeres está hablando mal de otra mujer. ¿Cuál sería la razón por la que se hablaría mal de ella?",
                  #"Cuéntanos ¿cuál, o cuáles, serían las razones por las que se hablaría mal de una mujer en la empresa?",
                  "Imagine que en esta empresa, un grupo de hombres se está burlando de otro hombre. ¿Cuál sería la razón por la que se estarían burlando de él?",
                  "En esta empresa, escuchar atentamente es más común en",
                  "En esta empresa, comunicarse asertivamente es más común en:",
                  "En esta empresa, hacer de todo un conflicto es más común en:",
                  "En esta empresa hablar a espaldas de las demás personas es más común en:"
                )
              )
            ),
            mainPanel(
              h3("Cruce entre Género y Dinámica Seleccionada"),
              plotOutput("grafico_normas_dinamico")
            )
          )
        )
        
        
        
        
      )
    )
  )
)

