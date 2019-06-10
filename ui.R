library(shiny)
library(leaflet)
library(ggplot2)

dataset <- diamonds

shinyUI(fluidPage(
  
  theme = "bootstrap.css",

  #tags$head(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
            
  #titlePanel("Accidentalidad en Medellin"),
  headerPanel("Accidentalidad en Medellin", "Accidentalidad en Medellin"),
  #sidebarPanel(),

 sidebarPanel(
   
   selectInput('_Comuna', 'Seleccione una comuna:', comunas),
   selectInput('_Barrio', 'Seleccione un barrio:', iconv(barrios@data$NOMBRE,"UTF-8","ISO_8859-1")),
   
    sliderInput('_Year', 'Fecha', min=2014, max=2018,
               value=min(2014, 2018), step=1, round=0),

    #selectInput('x', 'X', names(dataset)),
    #selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    selectInput('color', 'Color', c('None', names(dataset))),

    checkboxInput('jitter', 'Jitter'),
    checkboxInput('smooth', 'Smooth'),

    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
  ),

  mainPanel(
    leafletOutput("myMap")
    #plotOutput('plot')
  )
))