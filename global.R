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
