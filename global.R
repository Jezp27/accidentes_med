library(raster)
library(rgdal)


comunas <- c("America",
             "Aranjuez",
             "Belen",
             "Buenos Aires",
             "Candelaria",
             "Castilla",
             "Doce Octubre",
             "Guayabal",
             "Laureles",
             "Manrique",
             "Poblado",
             "Popular",
             "Robledo",
             "San Javier",
             "Santa Cruz",
             "Villa Hermosa")
#barrios <- shapefile("./Data/Barrios de Medellin/Barrio_Vereda.shp")

barriosOGR=readOGR("./Data/Barrios de Medellin/Barrio_Vereda.shp")
comunasOGR=readOGR("./Data/Comunas/Limite_Comuna_Corregimiento.shp")

b<-read.csv("Fbarrios1.csv")
c<-read.csv("./Fcomunas1.csv")
#imports necesarios para prediccion
FechasPrediccion<-read.csv("fest_etiq.csv")
randomForestBarrios<-get(load("modelosRandomForestBarrio2018.RData"))
randomForestComunas<-get(load("modelosRandomForestBarrio2018.RData"))

nb<-as.data.frame(table(b$BARRIO))
nc<-as.data.frame(table(c$COMUNA))
nc<-nc$Var1
nb<-nb$Var1
barrioss<-subset(barriosOGR, barriosOGR$NOMBRE %in% nb)
comunass<-subset(comunasOGR, comunasOGR$NOMBRE %in% nc)


