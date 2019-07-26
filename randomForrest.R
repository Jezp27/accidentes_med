library(lubridate)
library(randomForest)
databaseComunas<-read.csv(file = "Fcomunas1.csv", sep = ",")
Comunas<-unique(databaseComunas$COMUNA)

DIA<-array()
DIA<-day(mdy(databaseComunas$FECHA))
databaseComunas<-cbind.data.frame(databaseComunas,DIA)
databaseComunas<-databaseComunas[,c('X','FECHA','NOMBRE_DIA','FESTIVO','DIA','SEMANA','MES','ANIO','COMUNA','Freq')]


modelosRandomForestComuna2018<-list()
mseRandomForestComuna2018<-array()

for (comuna in Comunas){
  #Para el barrio SELECCIONADO
  aux<-subset.data.frame(databaseComunas,databaseComunas$COMUNA==comuna)
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
  modelosRandomForestComuna2018[[comuna]]<-mBasico
  mseRandomForestComuna2018[comuna]<-mse
  
}

summary(mseRandomForestComuna2018)
mseRandomForestComuna2018
save(modelosRandomForestComuna2018,file = "modelosRandomForestComuna2018.RData")
save(mseRandomForestComuna2018,file = "mseRandomForestComuna2018.RData")
#rm(modelosRandomForestComuna)
#importando archivos RData
modelosRandomForestComuna<-get(load("modelosRandomForestComuna2018.RData"))

