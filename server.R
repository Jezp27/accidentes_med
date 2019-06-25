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

  output$titulo <- renderText("Mapa de la ciudad de Medellin segmentado por comunas")
  
  output$error1 <- renderText("Por favor ingrese el rango de fechas completo")
  output$error2 <- renderText("El rango de fechas ingresado es inválido, por favor verifíquelo")
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
      
      fecha<-ymd(input$`_Fecha`[1])
      fecha2<-ymd(input$`_Fecha`[2])
      nombreC <- input$`_Comuna`
      
      output$myMap <- renderLeaflet({
        print(paint_date_range_comunas(comunass,fechai = fecha,fechaf = fecha2)) 
      })
      
      output$descriptionTableC <- DT::renderDataTable({
        DT::datatable( c, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
      })
      
      #output$descriptionTableC <- DT::renderDataTable({
       # if (input$`_escala` == "dia"){
        #  DT::datatable(get_freq_com_dia(c, nombre=nombreC,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        #}else if (input$`_escala` == "semana"){
         # DT::datatable(get_freq_com_semana(c, nombre=nombreC,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        #}else{
        #  DT::datatable(get_freq_com_mes(c, nombre=nombreC,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        #}
      #})
      
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
      
      fecha <- ymd(input$`_Fecha`[1])
      fecha2 <- ymd(input$`_Fecha`[2])
      nombreB <- input$`_Barrio`
      
      output$myMap <- renderLeaflet({
        print(paint_date_range_barrios(barrioss,fechai = fecha,fechaf = fecha2)) 
      })
      
      output$descriptionTableB <- DT::renderDataTable({
        DT::datatable( b, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
      })
      
      #output$descriptionTableB <- DT::renderDataTable({
       # if (input$`_escala` == "dia"){
        #  DT::datatable(get_freq_bar_dia(b, nombre=nombreB,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        #}else if (input$`_escala` == "semana"){
         # DT::datatable(get_freq_bar_semana(b, nombre=nombreB,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        #}else{
        #  DT::datatable(get_freq_bar_mes(b, nombre=nombreB,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        #}
      #})
  
      shinyjs::hide("descriptionTableC")
      shinyjs::show("descriptionTableB")
    }
  })
  
  observeEvent(input$`_Fecha`,{
    hideElement("resultadosTitulo")
  })
  
############################################################################################3

  paint_date_range_comunas<-function(data,fechai,fechaf){
    #filtrado de la data por fechas
    graphdata<-subseting_fecha(c,fechai=fechai,fechaf = fechaf)
    
    #inicializo algunas variables(necesario, para que grafique bien)
    summa<-array()
    com<-NA
    #coms<-iconv(data$NOMBRE, "UTF-8","ISO_8859-1")
    
    #genero la suma de los accidentes en las fechas indicadas
    for (com in data$NOMBRE) {
      #show(com)
      x<-sum((subset(graphdata,graphdata$COMUNA==com))$Freq)
      #show(x)
      summa[com]<-x
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
                  #con UTF e ISO... se pegan los nombres de manera correcta
                  #summa para mostrar número de accidentes en ese período de tiempo
                  popup = paste(comunass$NOMBRE,summa))
    m=addTiles(m,options = tileOptions(minZoom=11, maxZoom=14))
    #Se agrega la marca de la escala del mapa, opcional
    m=addScaleBar(m)
    return(m)
    #plot(summa, col=color)
  }

################################################################################################
  
  paint_date_range_barrios<-function(data,fechai,fechaf){
    graphdata<-subseting_fecha(b,fechai=fechai,fechaf = fechaf)
    summa<-array()
    bar<-NA
    for (bar in data$NOMBRE) {
      #show(bar)
      x<-sum((subset(graphdata,graphdata$BARRIO==bar))$Freq)
      #show(x)
      summa[bar]<-x
    }
    summa<-summa[2:length(summa)]
    color=NA
    color=sequential_hcl(length(summa),palette = "Heat")[rank(1/summa)]
    m=leaflet(barrioss)
    m=addPolygons(m,fillOpacity =0.5,color =color, opacity = 1, 
                  popup = paste(barrioss$NOMBRE,summa))
    m=addTiles(m,options = tileOptions(minZoom=11, maxZoom=15))
    m=addScaleBar(m)
    return(m)
  }
  
  subseting_fecha<-function(data,fechai=mdy(0/0/2000),fechaf=mdy(12/31/2020)){
    a<-subset(data,(mdy(data$FECHA))>=fechai)
    return(subset(a,(mdy(a$FECHA))<=fechaf))
  }
  
################################################################################################
  ###Devuelve el dataframe a mostrar en la pag de un barrio por días
  #get_freq_bar_dia<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
   # bardata<-subset(data,data$BARRIO==nombre)
    #inter<-interval(fechai,fechaf)
    #bardata<-subset(bardata,mdy(bardata$FECHA) %within% inter)
    #nocolumnas<-c("X","SEMANA","AÑO","BARRIO","FESTIVO","MES")
    #bardata<-bardata[ , !names(bardata) %in% nocolumnas]
    #names(bardata)[2]<-"DIA"
    #names(bardata)[3]<-"NUMERO DE ACCIDENTES"
    #return(bardata)
  #}
  
  ##Devuelve el dataframe a mostrar en la pag de un barrio por semanas
  #get_freq_bar_semana<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
   # bardata<-subset(data,data$BARRIO==nombre)
    #inter<-interval(fechai,fechaf)
    #bardata<-subset(bardata,mdy(bardata$FECHA) %within% inter)
    #semanas<-aggregate(Freq ~ (SEMANA+AÑO) , data=bardata, FUN=sum)
    #names(semanas)[3]<-"NUMERO DE ACCIDENTES"
    #return(semanas)
  #}
  
  ##Devuelve el dataframe a mostrar en la pag de un barrio por meses
  #get_freq_bar_mes<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
   # bardata<-subset(data,data$BARRIO==nombre)
    #day(fechai)<-1
    #day(fechaf)<-(days_in_month(fechaf))
    #inter<-interval(fechai,fechaf)
    #bardata<-subset(bardata,mdy(bardata$FECHA) %within% inter)
    #pormes<-aggregate(Freq ~ (MES+AÑO) , data=bardata, FUN=sum)
    #names(pormes)[3]<-"NUMERO DE ACCIDENTES"
    #return(pormes)
  #}
################################################################################################ 
  
  ###Devuelve el dataframe a mostrar en la pag de una comuna por días
  #get_freq_com_dia<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
   # comdata<-subset(data,data$COMUNA==nombre)
    #inter<-interval(fechai,fechaf)
    #comdata<-subset(comdata,mdy(comdata$FECHA) %within% inter)
    #nocolumnas<-c("X","SEMANA","AÑO","COMUNA","FESTIVO","MES")
    #comdata<-comdata[ , !names(comdata) %in% nocolumnas]
    #names(comdata)[2]<-"DIA"
    #names(comdata)[3]<-"NUMERO DE ACCIDENTES"
    #return(comdata)
  #}
  
  #get_freq_com_semana<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
   # comdata<-subset(data,data$COMUNA==nombre)
    #inter<-interval(fechai,fechaf)
    #comdata<-subset(comdata,mdy(comdata$FECHA) %within% inter)
    #semanas<-aggregate(Freq ~ (SEMANA+AÑO) , data=comdata, FUN=sum)
    #names(semanas)[3]<-"NUMERO DE ACCIDENTES"
    #return(semanas)
  #}
  
  #get_freq_com_mes<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
   # comdata<-subset(data,data$COMUNA==nombre)
    #day(fechai)<-1
    #day(fechaf)<-(days_in_month(fechaf))
    #inter<-interval(fechai,fechaf)
    #comdata<-subset(comdata,mdy(comdata$FECHA) %within% inter)
    #pormes<-aggregate(Freq ~ (MES+AÑO) , data=comdata, FUN=sum)
    #names(pormes)[3]<-"NUMERO DE ACCIDENTES"
    #return(pormes)
  #}
}

