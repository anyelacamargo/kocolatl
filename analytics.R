library(rgdal)
library(tmap)# load tmap package (see Section IV)
library(sf)

library(sp)
library(ggplot2)

yield <- read.csv('yield.csv', header = TRUE)
gadmColombia = readOGR(dsn = "COL_adm", layer = "COL_adm1")
#plot(gadmColombia)

departamento <- c('AMAZONAS','ANTIOQUIA','ARAUCA','ATLANTICO','BOLIVAR','BOYACA',
                   'CORDOBA','CALDAS','CAQUETA','CASANARE','CAUCA','CESAR','CHOCO','CUNDINAMARCA','GUAINIA',
'GUAVIARE','HUILA','LA GUAJIRA','MAGDALENA','META','NARINO','NORTE DE SANTANDERl','PUTUMAYO',
'QUINDIO','RISARALDA','SAN ANDRES Y PROVIDENCIA','SANTANDER','SUCRE','TOLIMA', 'VALLE DEL CAUCA',
'VAUPES','VICHADA')

gadmColombia@data = data.frame(gadmColombia@data,
                               departamento = departamento)
gadmColombia@data <- merge(data.frame(gadmColombia@data), yield, by = 'departamento',
                           all.x = TRUE)
#plot(gadmColombia, col=gadmColombia$X2019)
#spplot(gadmColombia, 'X2019')

print(qtm(gadmColombia, format = "World", style = "col_blind"))

tm_shape(World) +
  tm_polygons("HPI", n = 9, palette = "div",
              title = "Happy Planet Index", id = "name") +
  tm_style("gray") +
  tm_format("World")
