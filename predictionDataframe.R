Fechas<-read.csv("fest_etiq.csv")
library(lubridate)

subseting_fecha<-function(data,fechai=mdy('1/01/2000'),fechaf=mdy('12/31/2020')){
  a<-subset(data,(mdy(data$FECHA))>=fechai)
  a<-subset(a,(mdy(a$FECHA))<=fechaf)
  return(a)
}

pred_barrios<-function(barrio="",fechai=mdy('1/01/2000'),fechaf=mdy('12/31/2020')){
  a<-subseting_fecha(Fechas,fechai,fechaf)
  daysb<-array()
  monthsb<-array()
  yearsb<-array()
  yearsb<-year(mdy(a$FECHA))
  monthsb<-month(mdy(a$FECHA))
  daysb<-day(mdy(a$FECHA))
  a<-a[-1]
  a<-cbind(a,yearsb,monthsb,daysb)
  return(a)
}

atest1<-pred_barrios("",mdy('1/01/2018'),mdy('2/01/2018'))


a<-subseting_fecha(Fechas,mdy('1/01/2018'),mdy('3/01/2018'))
