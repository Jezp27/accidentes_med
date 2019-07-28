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
library(randomForest)
library(xts)
library(dygraphs)
library(tidyverse)

function(input, output) {

  output$titulo <- renderText("Mapa de la ciudad de Medellin segmentado por comunas")
  output$serie <- renderText("Serie de Tiempo")
  output$error1 <- renderText("Por favor ingrese el rango de fechas completo")
  output$error2 <- renderText("El rango de fechas ingresado es invalido, por favor verifiquelo")

  output$myMap <- renderLeaflet({
    output$descriptionTableC <- NULL
    output$descriptionTableB <- NULL
    output$serieTh <- NULL
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
      
      output$titulo <- renderText(paste("Mapa de la ciudad de Medellin segmentado por comunas en el rango de fechas",
                                  input$`_Fecha`[1], " y ",  input$`_Fecha`[2]))
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
      
      output$descriptionTableC <- DT::renderDataTable({
       if (input$`_escala` == "dia"){
          dataF <- get_freq_com_dia(c, nombre=nombreC,fechai=fecha,fechaf = fecha2)
          DT::datatable(dataF, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else if (input$`_escala` == "semana"){
          dataF <- get_freq_com_semana(c, nombre=nombreC,fechai=fecha,fechaf = fecha2)
          DT::datatable(dataF, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else{
          dataF <- get_freq_com_mes(c, nombre=nombreC,fechai=fecha,fechaf = fecha2)
          DT::datatable(dataF, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }
      })
      
      if (input$`_escala` == "dia"){
        dataF <- get_freq_com_dia(c, nombre=nombreC,fechai=fecha,fechaf = fecha2)
        showElement("serieT")
        serie_tiempo(dataF)
      }else if (input$`_escala` == "semana"){
        dataF <- get_freq_com_semana(c, nombre=nombreC,fechai=fecha,fechaf = fecha2)
        showElement("serieT")
        serie_tiempo(dataF)
      }else{
        dataF <- get_freq_com_mes(c, nombre=nombreC,fechai=fecha,fechaf = fecha2)
        showElement("serieT")
        serie_tiempo(dataF)
      }
      
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
      
      output$titulo <- renderText(paste("Mapa de la ciudad de Medellin segmentado por barrios en el rango de fechas",
                                        input$`_Fecha`[1], " y ",  input$`_Fecha`[2]))
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
      
      output$descriptionTableB <- DT::renderDataTable({
        if (input$`_escala` == "dia"){
          dataF <- get_freq_bar_dia(b, nombre=nombreB,fechai=fecha,fechaf = fecha2)
          DT::datatable(dataF, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else if (input$`_escala` == "semana"){
          dataF <- get_freq_bar_semana(b, nombre=nombreB,fechai=fecha,fechaf = fecha2)
          DT::datatable(dataF, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else{
          dataF <-get_freq_bar_mes(b, nombre=nombreB,fechai=fecha,fechaf = fecha2)
          DT::datatable(dataF, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }
      })
      
      if (input$`_escala` == "dia"){
        dataF <- get_freq_bar_dia(b, nombre=nombreB,fechai=fecha,fechaf = fecha2)
        showElement("serieT")
        serie_tiempo(dataF)
      }else if (input$`_escala` == "semana"){
        dataF <- get_freq_bar_semana(b, nombre=nombreB,fechai=fecha,fechaf = fecha2)
        showElement("serieT")
        serie_tiempo(dataF)
      }else{
        dataF <-get_freq_bar_mes(b, nombre=nombreB,fechai=fecha,fechaf = fecha2)
        showElement("serieT")
        serie_tiempo(dataF)
      }
  
      shinyjs::hide("descriptionTableC")
      shinyjs::show("descriptionTableB")
      
    }
  })
  
  observeEvent(input$`_Fecha`,{
    if(is.na(input$`_Fecha`[1]) || is.na(input$`_Fecha`[2])){
      showElement("msgError1")
      hideElement("msgError2")
    }else if(input$`_Fecha`[1] > input$`_Fecha`[2]){
      showElement("msgError2")
      hideElement("msgError1")
    }else{
      hideElement("msgError1")
      hideElement("msgError2")
      showElement("resultadosTitulo")}
      
    #hideElement("resultadosTitulo")
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
                  #summa para mostrar numero de accidentes en ese periodo de tiempo
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
  ###Devuelve el dataframe a mostrar en la pag de un barrio por dias
  get_freq_bar_dia<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
    bardata<-subset(data,data$BARRIO==nombre)
    inter<-interval(fechai,fechaf)
    bardata<-subset(bardata,mdy(bardata$FECHA) %within% inter)
    nocolumnas<-c("X","SEMANA","ANIO","BARRIO","FESTIVO","MES")
    bardata<-bardata[ , !names(bardata) %in% nocolumnas]
    names(bardata)[2]<-"DIA"
    names(bardata)[3]<-"NUMERO DE ACCIDENTES"
    return(bardata)
  }
  
  ##Devuelve el dataframe a mostrar en la pag de un barrio por semanas
  get_freq_bar_semana<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
    bardata<-subset(data,data$BARRIO==nombre)
    inter<-interval(fechai,fechaf)
    bardata<-subset(bardata,mdy(bardata$FECHA) %within% inter)
    semanas<-aggregate(Freq ~ (SEMANA+ANIO) , data=bardata, FUN=sum)
    names(semanas)[3]<-"NUMERO DE ACCIDENTES"
    return(semanas)
  }
  
  ##Devuelve el dataframe a mostrar en la pag de un barrio por meses
  get_freq_bar_mes<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
    bardata<-subset(data,data$BARRIO==nombre)
    day(fechai)<-1
    day(fechaf)<-(days_in_month(fechaf))
    inter<-interval(fechai,fechaf)
    bardata<-subset(bardata,mdy(bardata$FECHA) %within% inter)
    pormes<-aggregate(Freq ~ (MES+ANIO) , data=bardata, FUN=sum)
    names(pormes)[3]<-"NUMERO DE ACCIDENTES"
    return(pormes)
  }
################################################################################################ 
  ###Serie de tiempo
  serie_tiempo<-function(data){
    output$serieTh <- renderDygraph({
      data<-transform.data.frame(data, FECHA = parse_date_time(FECHA,orders="mdy"))
      #print(data)
      if(is.null(data$NUMERO.DE.ACCIDENTES) || is.null(data$FECHA)) return(NULL)
      don<-xts(x = data$NUMERO.DE.ACCIDENTES ,order.by=data$FECHA)
      dygraph(don) %>%
        dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
    })
  }
  
  ###Devuelve el dataframe a mostrar en la pag de una comuna por dias
  get_freq_com_dia<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
    comdata<-subset(data,data$COMUNA==nombre)
    inter<-interval(fechai,fechaf)
    comdata<-subset(comdata,mdy(comdata$FECHA) %within% inter)
    nocolumnas<-c("X","SEMANA","ANIO","COMUNA","FESTIVO","MES")
    comdata<-comdata[ , !names(comdata) %in% nocolumnas]
    names(comdata)[2]<-"DIA"
    names(comdata)[3]<-"NUMERO DE ACCIDENTES"
    #print(comdata)
    return(comdata)
  }
  
  get_freq_com_semana<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
   comdata<-subset(data,data$COMUNA==nombre)
   inter<-interval(fechai,fechaf)
   comdata<-subset(comdata,mdy(comdata$FECHA) %within% inter)
   semanas<-aggregate(Freq ~ (SEMANA+ANIO) , data=comdata, FUN=sum)
   names(semanas)[3]<-"NUMERO DE ACCIDENTES"
   return(semanas)
  }
  
  get_freq_com_mes<-function(data,nombre="",fechai=mdy("1/1/2012"),fechaf=mdy("1/1/2020")){
    comdata<-subset(data,data$COMUNA==nombre)
    day(fechai)<-1
    day(fechaf)<-(days_in_month(fechaf))
    inter<-interval(fechai,fechaf)
    comdata<-subset(comdata,mdy(comdata$FECHA) %within% inter)
    pormes<-aggregate(Freq ~ (MES+ANIO) , data=comdata, FUN=sum)
    names(pormes)[3]<-"NUMERO DE ACCIDENTES"
    return(pormes)
  }
#-----------------------------nueva funcion prediccion ------------------------------------------
  subseting_fecha<-function(data,fechai=mdy('1/01/2012'),fechaf=mdy('12/31/2020')){
    fechainicial<-mdy(fechai)
    fechafinal<-mdy(fechaf)
    a<-subset(data,(mdy(data$FECHA))>=fechai)
    a<-subset(a,(mdy(a$FECHA))<=fechaf)
    return(a)
  }
  
  pred_barrios<-function(barrio="",fechai=mdy('1/01/2000'),fechaf=mdy('12/31/2020')){
    a<-subseting_fecha(FechasPrediccion,fechai,fechaf)
    DIA<-array()
    MES<-array()
    ANIO<-array()
    NOMBRE_DIA<-array()
    SEMANA<-array()
    SEMANA<-strftime(mdy(a$FECHA),format = "%V")
    NOMBRE_DIA<-wday(mdy(a$FECHA))
    ANIO<-year(mdy(a$FECHA))
    MES<-month(mdy(a$FECHA))
    DIA<-day(mdy(a$FECHA))
    Fecha<-a[1]
    a<-a[-1]
    a<-cbind(a,ANIO,MES,DIA,NOMBRE_DIA,SEMANA)
    a<-transform.data.frame(a, SEMANA = as.numeric(SEMANA))
    a<-a[-6]
    #obteniendo el modelo del barrio ingresado
    modelo<-randomForestBarrios[[barrio]]
    #realizando la predicción
    Accidentes_Predichos<-predict(modelo,a)
    Fecha<-cbind(Fecha,a$NOMBRE_DIA,Accidentes_Predichos)
    names(Fecha)<-c("FECHA","DIA","ACCIDENTES PREDICHOS")
    return(Fecha)
  }
  
  pred_comunas<-function(comuna="",fechai=mdy('1/01/2000'),fechaf=mdy('12/31/2020')){
    a<-subseting_fecha(FechasPrediccion,fechai,fechaf)
    DIA<-array()
    MES<-array()
    ANIO<-array()
    NOMBRE_DIA<-array()
    SEMANA<-array()
    SEMANA<-strftime(mdy(a$FECHA),format = "%V")
    NOMBRE_DIA<-wday(mdy(a$FECHA))
    ANIO<-year(mdy(a$FECHA))
    MES<-month(mdy(a$FECHA))
    DIA<-day(mdy(a$FECHA))
    Fecha<-a[1]
    a<-a[-1]
    a<-cbind(a,ANIO,MES,DIA,NOMBRE_DIA,SEMANA)
    a<-transform.data.frame(a, SEMANA = as.numeric(SEMANA))
    a<-a[-6]
    #obteniendo el modelo del barrio ingresado
    modelo<-randomForestComunas[[comuna]]  
    #realizando la predicción
    Accidentes_Predichos<-predict(modelo,a)
    Fecha<-cbind(Fecha,a$NOMBRE_DIA,Accidentes_Predichos)
    names(Fecha)<-c("FECHA","DIA","ACCIDENTES PREDICHOS")
    return(Fecha)
  }
  
  
################################## # PREDICCION # ###############################################
  output$pred_Error1 <- renderText("Por favor ingrese el rango de fechas completo")
  output$pred_Error2 <- renderText("El rango de fechas ingresado es invalido, por favor verifiquelo")
  output$prediccionTableC <- NULL
  output$prediccionTableB <- NULL 
  
  observeEvent(input$`pred_SelComuna`, {
    shinyjs::show(id = "pred_Comuna")
    shinyjs::hide(id = "pred_Barrio")
    
    showElement(id= "pred_BuscarComunas")
    hideElement(id= "pred_BuscarBarrios")
  })
  
  observeEvent(input$`pred_SelBarrio`, {
    shinyjs::show(id = "pred_Barrio")
    shinyjs::hide(id = "pred_Comuna")
    
    hideElement(id= "pred_BuscarComunas")
    showElement(id= "pred_BuscarBarrios")
  })
  
  observeEvent(input$`pred_BuscarComunas`, {
    if(is.na(input$`pred_Fecha`[1]) || is.na(input$`pred_Fecha`[2])){
      showElement("pred_MsgError1")
    }else if(input$`pred_Fecha`[1] > input$`pred_Fecha`[2]){
      showElement("pred_MsgError2")
    }else{
      hideElement("pred_MsgError1")
      hideElement("pred_MsgError2")
      showElement("resultadosPrediccion")
      
      fecha<-ymd(input$`pred_Fecha`[1])
      fecha2<-ymd(input$`pred_Fecha`[2])
      nombreC <- input$`pred_Comuna`
      
      output$resultadoPred <- renderText({
        paste("Resultados para la comuna ", nombreC, "en el rango de fechas", fecha, " y ",
              fecha2)})
      
      output$prediccionTableC <- DT::renderDataTable({
        DT::datatable( c, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
      })
      
      output$prediccionTableC <- DT::renderDataTable({
        if (input$`pred_Escala` == "dia"){
          DT::datatable(pred_comunas(comuna=nombreC,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else if (input$`pred_Escala` == "semana"){
          DT::datatable(get_freq_com_semana(c, nombre=nombreC,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else{
          DT::datatable(get_freq_com_mes(c, nombre=nombreC,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }
      })
      
      shinyjs::show("prediccionTableC")
      shinyjs::hide("prediccionTableB")
    }
  })
  
  observeEvent(input$`pred_BuscarBarrios`, {
    if(is.na(input$`pred_Fecha`[1]) || is.na(input$`pred_Fecha`[2])){
      showElement("pred_MsgError1")
      hideElement("pred_MsgError2")
    }else if(input$`pred_Fecha`[1] > input$`pred_Fecha`[2]){
      showElement("pred_MsgError2")
      hideElement("pred_MsgError1")
    }else{
      hideElement("pred_MsgError1")
      hideElement("pred_MsgError2")
      showElement("resultadosPrediccion")
      
      fecha <- ymd(input$`pred_Fecha`[1])
      fecha2 <- ymd(input$`pred_Fecha`[2])
      nombreB <- input$`pred_Barrio`
      
      output$resultadoPred <- renderText({
        paste("Resultados para el barrio ", nombreB, "en el rango de fechas", fecha, " y ",
              fecha2)})
      
      output$prediccionTableB <- DT::renderDataTable({
        DT::datatable( b, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
      })
      
      output$prediccionTableB <- DT::renderDataTable({
        if (input$`pred_Escala` == "dia"){
          DT::datatable(pred_barrios(barrio=nombreB,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else if (input$`pred_Escala` == "semana"){
          DT::datatable(get_freq_bar_semana(b, nombre=nombreB,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }else{
          DT::datatable(get_freq_bar_mes(b, nombre=nombreB,fechai=fecha,fechaf = fecha2), options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
        }
      })
      
      shinyjs::hide("prediccionTableC")
      shinyjs::show("prediccionTableB")
    }
  })
  
  observeEvent(input$`pred_Fecha`,{
    if(is.na(input$`pred_Fecha`[1]) || is.na(input$`pred_Fecha`[2])){
      showElement("pred_MsgError1")
      hideElement("pred_MsgError2")
    }else if(input$`pred_Fecha`[1] > input$`pred_Fecha`[2]){
      showElement("pred_MsgError2")
      hideElement("pred_MsgError1")
    }else{
      hideElement("pred_MsgError1")
      hideElement("pred_MsgError2")
      showElement("resultadosTitulo")}
    
    #hideElement("resultadosTitulo")
  })
}

