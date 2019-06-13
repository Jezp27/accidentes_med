library(shiny)
library(leaflet)
library(ggplot2)
library(shinyBS)
library(shinythemes)
library(shinyLP)

dataset <- diamonds

shinyUI(
  navbarPage("********************",
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
        #panel_div(class_type = "primary", panel_title = "Criterios",
         #         content = 
         sidebarPanel(
            selectInput('_Comuna', 'Seleccione una comuna:', comunas),
            selectInput('_Barrio', 'Seleccione un barrio:', iconv(barrios@data$NOMBRE,"UTF-8","ISO_8859-1")),
            sliderInput('_Year', 'Fecha', min=2014, max=2018, value=min(2014, 2018), step=1, round=0),
            #selectInput('x', 'X', names(dataset)),
            #selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
            selectInput('color', 'Color', c('None', names(dataset))),
            checkboxInput('jitter', 'Jitter'),
            checkboxInput('smooth', 'Smooth'),
                        
            selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
            selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
         ), #),
         mainPanel(
            leafletOutput("myMap")
            #plotOutput('plot')
         )
      )
    )
  )
             
  
  #fluidPage(
  


  #tags$head(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),

 
  


