library(lubridate)
library(randomForest)
databaseBarrios<-read.csv(file = "Fbarrios1.csv", sep = ",")
Barrios<-unique(databaseBarrios$BARRIO)

DIA<-array()
DIA<-day(mdy(databaseBarrios$FECHA))
databaseBarrios<-cbind.data.frame(databaseBarrios,DIA)
databaseBarrios<-databaseBarrios[,c('X','FECHA','NOMBRE_DIA','FESTIVO','DIA','SEMANA','MES','ANIO','BARRIO','Freq')]


modelosRandomForestComuna<-list()
mseRandomForestComuna<-array()

#for (barrio in Barrios){
  #Para el barrio SELECCIONADO
  aux<-subset.data.frame(databaseBarrios,databaseBarrios$BARRIO=="Aranjuez")
  #organizando el dataframe por las fechas
  aux<-aux[order(as.Date(aux$FECHA, format="%m/%d/%Y")),]
  #dejando solo las etiquetas necesarias ( se eliminan X, comuna)
  aux<-aux[names(aux) %in% c("NOMBRE_DIA","FESTIVO","DIA","SEMANA","MES","ANIO","Freq")]
  #partiendo el dataset por entrenamiento y validación (2018)
  aux_train_2018<-subset.data.frame(aux,aux$ANIO < 2018)
  aux_test_2018<-subset.data.frame(aux,aux$ANIO >= 2018)
  #entrenando el modelo
  mBasico<-randomForest(Freq ~ NOMBRE_DIA + FESTIVO + DIA + SEMANA + MES + ANIO,
                        data=aux_train_2018)
  
  mBasico
  #guardando el modelo del barrio
  modelosRandomForestComuna[[barrio]]<-mBasico
  #prediciendo accidentes a partir del modelo (sobre el conjunto de validación)
  
  
#}