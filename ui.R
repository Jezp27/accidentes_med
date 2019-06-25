library(shiny)
library(leaflet)
library(ggplot2)
library(shinycssloaders)
library(shinyBS)
library(shinythemes)
library(shinyLP)
library(shinyjs)
library(colorspace)
library(DT)

shinyUI(

  navbarPage("Predict4",
      #theme = "style.css",
      theme = shinytheme("flatly"),
      #shinythemes::themeSelector(),
      
      tabPanel(
        "Inicio",
        
        jumbotron("Accidentalidad en Medellin", "Historial y Prediccion", button = FALSE),
        
        fluidRow(
          column( width=12,
                  tags$div(class="logoWrapper",
                           align= "center",
                           HTML('<img src="inicio.png" alt="inicio">')
                  )
          )),
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
                 desde el 2014 hasta el 2018"
               )),
        
         sidebarPanel(
           
            fluidRow(
              panel_div(class = "primary", panel_title = "Ubicacion",
                  content = 
                    list(
                      helpText(class="text-info", 
                               "Seleccione si desea buscar por una comuna o un barrio particular.",
                               align = "center"),
                      
                      actionButton("_SelComuna", "Por comuna",  width = '100%' ),
                      tags$br(),
                      tags$br(),
                      actionButton("_SelBarrio", "Por barrio",  width = '100%' ),
                      tags$br(),
                      tags$br(),
                      selectInput("_Comuna", 'Comuna:', comunass$NOMBRE),
                      hidden(
                        selectInput("_Barrio", 'Barrio:', barrioss$NOMBRE))
                      )
                    )
              ),
            fluidRow(
              panel_div(class = "primary", panel_title = "Rango de fechas",
                   content =
                     list(
                        helpText(class="text-info", "Seleccione el rango de fechas deseado
                                  (Valido desde el '2014-01-01' hasta '2018-12-31')",
                                 align = "center"),  
                        dateRangeInput('_Fecha' , 'Fecha:', min = '2014-01-01',max = '2018-12-31', startview= "decade",
                                       language = "es", separator = " - ", format = "mm/dd/yyyy"),
                        hidden(tags$div(
                                  id = "msgError1", 
                                  class="text-danger", 
                                  tags$h5(textOutput("error1"), 
                                          align = "center")
                               )
                        ),
                        hidden(tags$div(
                                  id = "msgError2", 
                                  class="text-danger", 
                                  tags$h5(textOutput("error2"), 
                                          align = "center")
                                )
                        )
                        
                          #dateInput('_fecha', 'Seleccine', min= '2014-01-01', max ='2018-12-31')) ),   
                          #selectInput('color', 'Color', c('None', names(dataset))),
                          #selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
                          #checkboxInput('jitter', 'Jitter'),
                          #checkboxInput('smooth', 'Smooth'),
                          #selectInput('facet_col', 'Facet Column', c(None='.', names(dataset))))
                      )
                    )
              ),
            fluidRow(
              panel_div(class = "primary", panel_title = "Escala",
                        content = 
                          list(
                          helpText(class="text-info",
                                   "Seleccione la escala de tiempo que desea usar para visualizar los resultados",
                                   align = "center"),
                          radioButtons('_escala', '',
                              choiceNames = list("Día", "Semana", "Mes"),
                              choiceValues =  list("dia", "semana", "mes")))
                        
              )
            ),
            
            helpText(class="text-warning", "**Todos los campos son obligatorios", align = "center"),
            column(width = 12, align = "center",
                   #submitButton("Buscar", width = '80%'),
                   actionButton("_BuscarComunas", "Obtener Historico",  width = '80%' ),
                   hidden(
                     actionButton("_BuscarBarrios", "Obtener Historico",  width = '80%' )
                   )
            )
          ),
         mainPanel(
            fluidRow(
              tags$h3(textOutput("titulo"), align = "center"),
              withSpinner(leafletOutput("myMap"), color = "#15a3c6"),
              tags$br(),
              panel_div(class_type = "Info", panel_title = "",
                      content = 
                        list(
                          hidden(tags$div(
                            id = "resultadosTitulo", 
                            class="text-primary", 
                            tags$h3(textOutput("resultado")), 
                                    align = "center")
                          ),
                          tags$br(),
                          withSpinner(DT::dataTableOutput("descriptionTableC"), color = "#15a3c6"),
                          withSpinner(DT::dataTableOutput("descriptionTableB"), color = "#15a3c6")
                        )
              )
            )
         )
      ),
      tabPanel(
        "Prediccion"
      )
    )
)        

 
  


