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
barrios <- shapefile("./Data/Barrios de Medellin/Barrio_Vereda.shp")
b2014= readOGR("./Data/a2014/Accidentalidad_georreferenciada_2014.shp")
y2014<- b2014@coords[which(is.na(b2014@data$BARRIO)),]
barriosOGR=readOGR("./Data/Barrios de Medellin/Barrio_Vereda.shp")
comunasOGR=readOGR("./Data/Comunas/Limite_Comuna_Corregimiento.shp")

b<-read.csv("Fbarrios.csv")
c<-read.csv("./Fcomunas.csv")

nb<-as.data.frame(table(b$BARRIO))
nc<-as.data.frame(table(c$COMUNA))
nc<-nc$Var1
nb<-nb$Var1
barrioss<-subset(barriosOGR,(iconv(barriosOGR$NOMBRE,"UTF-8","ISO_8859-1") %in% nb))
comunass<-subset(comunasOGR,(iconv(comunasOGR$NOMBRE,"UTF-8","ISO_8859-1") %in% nc))


