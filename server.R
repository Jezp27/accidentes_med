library(shiny)
library(leaflet)
library(rgdal)
library(readr)
library(ggplot2)
library(raster)

function(input, output) {

  #dataset <- reactive({
   # diamonds[sample(nrow(diamonds), input$sampleSize),]
 # })
  
  #dataset <- readOGR(dsn="./Barrio_Vereda.shp",layer="Barrio_Vereda")
  #dataset <- readOGR(dsn = path.expand("Barrio_Vereda.shp"))
  a2014= readOGR("./Data/a2014/Accidentalidad_georreferenciada_2014.shp")
  x2014<- a2014@coords[which(is.na(a2014@data$BARRIO)),]
 
  #x2<- reactiveValues(input$`_Comuna`)
  #x3<- reactiveValues(input$`_Year`)
  
  output$myMap <- renderLeaflet({
    m = leaflet(barrios)
    m = addPolygons(m, color= "blue")
    m=addTiles(m)
    m=addScaleBar(m)
    #m=addCircleMarkers(m,lng=x2014@coords, lat=x2014@coords,    
     #                  radius = 6,
      #                 color = "Yellow", 
       #                stroke = FALSE, fillOpacity = 0.7,popup = x2014@data$DIRECCION)
    print(m) 
  }) 

 
  
  output$plot <- renderPlot({

    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()

    if (input$color != 'None')
      p <- p + aes_string(color=input$color)

    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)

    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()

    print(p)


  }, height=700)

}