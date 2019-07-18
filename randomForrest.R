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

for (barrio in Barrios){
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
  
  predicted<-predict(mBasico,aux_test_2018)
  #prediciendo accidentes a partir del modelo (sobre el conjunto de validación)
  aux_test_2018<-cbind.data.frame(aux_test_2018,predicted)
  aux_test_2018<-cbind.data.frame(aux_test_2018,residual=aux_test_2018$Freq - aux_test_2018$predicted)
  #MSE sobre validacion
  mse=(sum((aux_test_2018$residual^2)))/nrow(aux_test_2018)
  mean(mBasico$mse)
  #guardando el modelo del barrio
  modelosRandomForestComuna[[barrio]]<-mBasico
  mseRandomForestComuna[barrio]<-mse
  
}
summary(mseRandomForestComuna)
save(modelosRandomForestComuna,file = "modelosRandomForestComuna2018.RData")
rm(modelosRandomForestComuna)
#importando archivos RData
modelosRandomForestComuna<-get(load("modelosRandomForestComuna2018.RData"))
modelosRandomForestComuna<-as.data.frame(modelosRandomForestComuna)
