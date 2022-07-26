ui = dashboardPage(
  header = dashboardHeader(skin = "light"),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab1",
        fluidRow(
        column(width = 6,
          leafletOutput("map", width = "100%")),
        column(
          width = 6,
          fluidRow(style = 'overflow-x: hidden',
            column(width = 4, div(style = "overflow-x: hidden"),
                   bs4InfoBoxOutput("Pie_kms",width = 12),
                   bs4InfoBoxOutput("Pie_patrullajes",width = 12),
                   bs4InfoBoxOutput("Pie_kms_guarda",width = 12)
                  
            ),
            column(width = 4,
                   valueBoxOutput("Motorizado_kms",
                                  width = "100%"),
                   valueBoxOutput("Motorizado_patrullajes",
                                  width = "100%"),
                   valueBoxOutput("Motorizado_kms_guarda",
                                  width = "100%"),
            ),
            column(width = 4,
                   valueBoxOutput("Acuatico_kms",
                                  width = "100%"),
                   valueBoxOutput("Acuatico_patrullajes",
                                  width = "100%"),
                   valueBoxOutput("Acuatico_kms_guarda",
                                  width = "100%"),
            ),
            
            
          )
          
        )),
        fluidRow(
          column(width=6,plotlyOutput('Grafico_datos',height = "100%", width = "100%")),
          column(width=6,plotlyOutput('Grafico_kms',height = "100%", width = "100%"))
        ),
        fluidRow(
          width = 12,
          dataTableOutput("tabla")
        )
        
      ),
      tabItem(
        tabName = "tab2",
        checkboxGroupInput(
          "variable", "Escoja las variables que desee en el informe:",
          c(
            "Datos de Fauna" = "cyl",
            "Datos de Amenaza" = "am",
            "Datos de Fauna Muerta" = "gear"
          )
        ),
        tableOutput("data")
      )
    )
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    inputId = "sidebarState",
    sidebarMenu(compact = T,
      id = "sidebar",
      menuItem(
        text = "Resumen Unidades",
        tabName = "tab1",
        icon = icon("signal"),
        selected = TRUE
      ),
      menuItem(
        text = "Generar Informe",
        tabName = "tab2",
        icon = icon("shuttle-van")
      )
    )
  ),
  controlbar = dashboardControlbar(skin = "light",
                                   br(),
                                   column(align="center",width = 12,
    
    selectInput(
        "Regiones",
        label = "Seleccione región:",
        choices = c(Seleccione = '', unique(inputs$REGION)),
        selected = '',width = "80%"
      ),
      selectInput(
        "Anos",
        label = "Seleccione Año:",
        choices = c(""),
        selected =  c(Seleccione= ''),width = "80%"),
      selectInput( 
        "Unidades",
        label = "Seleccione Unidad:",
                   choices = c(""),
                   selected =  c(Seleccione= ''),width = "80%"
      )
    
  )),
  footer = bs4DashFooter()
)