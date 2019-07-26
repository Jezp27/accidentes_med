FechasPrediccion<-read.csv("fest_etiq.csv")
library(lubridate)
library(randomForest)
#la sgte linea no es necesaria si se carga desde el inicio de la app el archivo
randomForestBarrios<-get(load("modelosRandomForestBarrio2018.RData"))
randomForestComunas<-get(load("modelosRandomForestBarrio2018.RData"))

subseting_fecha<-function(data,fechai=mdy('1/01/2000'),fechaf=mdy('12/31/2020')){
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
#test 
w<-pred_barrios("Altamira",mdy("1/01/2018"),mdy("1/29/2018"))
