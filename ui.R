library(shiny)
library(leaflet)
library(ggplot2)
library(shinycssloaders)
library(shinyBS)
library(shinythemes)
library(shinyLP)
library(shinyjs)

dataset <- diamonds

shinyUI(

  navbarPage("Predict4",
      #theme = "style.css",
      theme = shinytheme("flatly"),
      #shinythemes::themeSelector(),
      tabPanel(
        "Inicio",
        
        jumbotron("Accidentalidad en Medellin", "Historial y Prediccion", button = FALSE),
        
        sidebarLayout(
          sidebarPanel(
                          
          ),
          mainPanel(
                          
          )
        )
      ),
      tabPanel(
        "Historial",
        useShinyjs(),
        #panel_div(class_type = "primary", panel_title = "Criterios",
         #         content =
        column(width=12, align = "center",
               tags$h2(
                 "Obtenga los datos historicos de los accidentes que se han presentado en la ciudad
                 desde el 2018 hasta el 2019"
               )),
        
         sidebarPanel(
            
            fluidRow(
              panel_div(class = "primary", panel_title = "Ubicacion",
                  content = 
                    list(
                      #tags$p("Seleccione un barrio o columna: "),
                      helpText("Seleccione si desea buscar por una comuna o un barrio particular."),
                      
                      actionButton("_SelComuna", "Por comuna",  width = '100%' ),
                      tags$br(),
                      tags$br(),
                      actionButton("_SelBarrio", "Por barrio",  width = '100%' ),
                      tags$br(),
                      tags$br(),
                      #hidden(
                        selectInput("_Comuna", 'Comuna:', comunas),
    
                     # ),
                      hidden(
                        selectInput("_Barrio", 'Barrio:', iconv(barrios@data$NOMBRE,"UTF-8","ISO_8859-1")))
                      )
                    )
              ),
            fluidRow(
              panel_div(class = "primary", panel_title = "Rango de fechas",
                   content =
                     list(
                        helpText("Seleccione el rango de fechas deseado
                                  (Valido desde el '2014-01-01' hasta '2018-12-31')"),  
                          #sliderInput('_Year', 'Fecha', min=2014, max=2018, value=min(2014, 2018), step=1, round=0),
                        dateRangeInput('_Fecha' , 'Fecha:', min = '2014-01-01',max = '2018-12-31', startview= "decade")
                          #dateInput('_fecha', 'Seleccine', min= '2014-01-01', max ='2018-12-31')) ),   
                          #selectInput('color', 'Color', c('None', names(dataset))),
                          #selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
                          #checkboxInput('jitter', 'Jitter'),
                          #checkboxInput('smooth', 'Smooth'),
                          #selectInput('facet_col', 'Facet Column', c(None='.', names(dataset))))
                      )
                    )
              ),
            column(width = 12, align = "center",
                   #submitButton("Buscar", width = '80%'),
                   actionButton("_Buscar", "Obtener Historico",  width = '80%' )
            )
          ),
         mainPanel(
            fluidRow(
              tags$h3("Mapa de la ciudad de Medellin segmentado por barrios", align = "center"),
              withSpinner(leafletOutput("myMap"), color = "#15a3c6"),
              tags$br(),
              panel_div(class_type = "Info", panel_title = "Resultados Historicos",
                      content = 
                        list(
                          tags$canvas()
                        )
              )
            )
         )
      )
    )
)        
  
  #fluidPage(
  


  #tags$head(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),

 
  


