Fechas<-read.csv("fest_etiq.csv")
library(lubridate)

subseting_fecha<-function(data,fechai=mdy('1/01/2000'),fechaf=mdy('12/31/2020')){
  a<-subset(data,(mdy(data$FECHA))>=fechai)
  a<-subset(a,(mdy(a$FECHA))<=fechaf)
  return(a)
}

pred_barrios<-function(barrio="",fechai=mdy('1/01/2000'),fechaf=mdy('12/31/2020')){
  a<-subseting_fecha(Fechas,fechai,fechaf)
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
  a<-a[-1]
  a<-cbind(a,ANIO,MES,DIA,NOMBRE_DIA,SEMANA)
  a<-a[-6]
  return(a)
}

atest1<-pred_barrios("",mdy('1/01/2018'),mdy('2/01/2018'))


a<-subseting_fecha(Fechas,mdy('1/01/2018'),mdy('3/01/2018'))
