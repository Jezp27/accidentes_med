library(shiny)
library(leaflet)
library(rgdal)
library(readr)
library(ggplot2)
library(raster)
library(shinyjs)
library(lubridate)
library(colorspace)

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
  #hide("_Comunas")
  #hide("_Barrios")
  
 
  output$myMap <- renderLeaflet({
    #m = leaflet(barrios)
    #m = addPolygons(m, color= "blue")
    #m=addTiles(m)
    #m=addScaleBar(m)
    fecha<-mdy("3/3/2011")
    fecha2<-mdy("3/3/2020")
    print(paint_date_range_comunas(comunass,fechai = fecha,fechaf = fecha2)) 
  }) 
  
  observeEvent(input$`_SelComuna`, {
    #cat("Showing", input$`_Comuna`, "rows\n")
    showElement(id = "_Comuna")
    hideElement(id = "_Barrio")
    #help("show")
    #hide(input$`_Barrio`)
    #hide(id = "_Barrio")
  })
  
  observeEvent(input$`_SelBarrio`, {
    #cat("Showing", input$`_Comuna`, "rows\n")
    showElement(id = "_Barrio")
    hideElement(id = "_Comuna")
  })
  

  observeEvent(input$`_Buscar`, {
    cat("fecha", input$`_Fecha`)
    cat("Showing", input$`_Comuna`, "rows\n")
    output$myMap <- renderLeaflet({

      fecha<-mdy("3/3/2014")
      fecha2<-mdy("3/3/2015")
      print(paint_date_range_comunas(comunass,fechai = fecha,fechaf = fecha2)) 
    })
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

  paint_date_range_comunas<-function(data,fechai,fechaf){
    #filtrado de la data por fechas
    graphdata<-subseting_fecha(c,fechai=fechai,fechaf = fechaf)
    
    #inicializo algunas variables(necesario, para que grafique bien)
    summa<-array()
    com<-NA
    coms<-iconv(data$NOMBRE, "UTF-8","ISO_8859-1")
    


    #genero la suma de los accidentes en las fechas indicadas
    for (com in coms) {
      summa[com]<-sum((subset(graphdata,graphdata$COMUNA==com))$Freq)
    }
    #elimino la primer fila que es NA
    summa<-summa[2:17]
    ##vector de colores
    color=NA
    color=sequential_hcl(length(summa),palette = "Heat")[rank(1/summa)]
    #Empieza el mapeado
    m=leaflet(comunass)
    #se agregan lo poligonos de las comunas
    m=addPolygons(m,fillOpacity =0.5,color =color, opacity = 1,
                  popup = summa)
    m=addTiles(m,options = tileOptions(minZoom=11, maxZoom=14))
    #Se agrega la marca de la escala del mapa, opcional
    m=addScaleBar(m)
    return(m)
    #plot(summa, col=color)
  }
  
  subseting_fecha<-function(data,fechai=mdy(0/0/2000),fechaf=mdy(12/31/2020)){
    a<-subset(data,(mdy(data$FECHA))>=fechai)
    return(subset(a,(mdy(a$FECHA))<=fechaf))
  }
  
}

