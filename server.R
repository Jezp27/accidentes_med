library(shiny)
library(leaflet)
library(rgdal)
library(readr)
library(ggplot2)
library(raster)
library(shinyjs)
library(lubridate)
library(colorspace)
library(DT)

function(input, output) {

  #a2014= readOGR("./Data/a2014/Accidentalidad_georreferenciada_2014.shp")
  #x2014<- a2014@coords[which(is.na(a2014@data$BARRIO)),]
  output$titulo <- renderText("Mapa de la ciudad de Medellin segmentado por comunas")
  
  output$error1 <- renderText("Por favor ingrese el rango de fechas completo")
  output$error2 <- renderText("El rango de fechas ingresado es inválido, por favor verifiquelo")
  #values <- reactiveValues()
  #values$show <- FALSE
  
  output$myMap <- renderLeaflet({
    output$descriptionTableC <- NULL
    output$descriptionTableB <- NULL
    fecha<-mdy("1/1/2014")
    fecha2<-mdy("1/1/2015")
    
    print(paint_date_range_comunas(comunass,fechai = fecha,fechaf = fecha2)) 
  }) 

  observeEvent(input$`_SelComuna`, {
    showElement(id = "_Comuna")
    hideElement(id = "_Barrio")
    showElement(id= "_BuscarComunas")
    hideElement(id= "_BuscarBarrios")
  })
  
  observeEvent(input$`_SelBarrio`, {
    showElement(id = "_Barrio")
    hideElement(id = "_Comuna")
    
    hideElement(id= "_BuscarComunas")
    showElement(id= "_BuscarBarrios")
  })
  

  observeEvent(input$`_BuscarComunas`, {
    if(is.na(input$`_Fecha`[1]) || is.na(input$`_Fecha`[2])){
      showElement("msgError1")
    }else if(input$`_Fecha`[1] > input$`_Fecha`[2]){
      showElement("msgError2")
    }else{
      hideElement("msgError1")
      hideElement("msgError2")
      showElement("resultadosTitulo")
      output$titulo <- renderText("Mapa de la ciudad de Medellin segmentado por comunas")
      output$resultado <- renderText({
        paste("Resultados para la comuna ", input$`_Comuna`, "en el rango de fechas", input$`_Fecha`[1], " y ",
              input$`_Fecha`[2])})
      output$myMap <- renderLeaflet({
        fecha<-ymd(input$`_Fecha`[1])
        fecha2<-ymd(input$`_Fecha`[2])
        #fecha<-mdy("3/3/2014")
        #fecha2<-mdy("3/3/2015")
        print(paint_date_range_comunas(comunass,fechai = fecha,fechaf = fecha2)) 
      })
      
      output$descriptionTableC <- DT::renderDataTable({
        DT::datatable(c, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
      })
      
      shinyjs::show("descriptionTableC")
      shinyjs::hide("descriptionTableB")
    }
  })

  observeEvent(input$`_BuscarBarrios`, {
    if(is.na(input$`_Fecha`[1]) || is.na(input$`_Fecha`[2])){
      showElement("msgError1")
      hideElement("msgError2")
    }else if(input$`_Fecha`[1] > input$`_Fecha`[2]){
      showElement("msgError2")
      hideElement("msgError1")
    }else{
      hideElement("msgError1")
      hideElement("msgError2")
      showElement("resultadosTitulo")
      output$titulo <- renderText("Mapa de la ciudad de Medellin segmentado por barrios")
      output$resultado <- renderText({
        paste("Resultados para el barrio ", input$`_Barrio`, "en el rango de fechas", input$`_Fecha`[1], " y ",
              input$`_Fecha`[2])})
      output$myMap <- renderLeaflet({
        fecha<-ymd(input$`_Fecha`[1])
        fecha2<-ymd(input$`_Fecha`[2])
        #fecha<-mdy("3/3/2014")
        #fecha2<-mdy("3/3/2015")
        print(paint_date_range_comunas(comunass,fechai = fecha,fechaf = fecha2)) 
      })
      output$descriptionTableB <- DT::renderDataTable({
        DT::datatable(b, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
      })
  
      shinyjs::hide("descriptionTableC")
      shinyjs::show("descriptionTableB")
    }
  })
  
  observeEvent(input$`_Fecha`,{
    hideElement("resultadosTitulo")
  })

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
    help("leaflet")
    #se agregan lo poligonos de las comunas
    m=addPolygons(m,fillOpacity =0.5,color =color, opacity = 1,
                  popup = paste(iconv(comunass$NOMBRE,"UTF-8","ISO_8859-1"),summa))
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

