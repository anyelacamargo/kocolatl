library(rgdal)
library(tmap)# load tmap package (see Section IV)
library(sf)
library(sp)
library(ggplot2)
library(raster)
library(dplyr)
library(reshape2)

library(caret)
require(gridExtra)


yield <- read.csv('yield.csv', header = TRUE)
r <- read.csv('dep_class.csv', header=T)
gadmColombia <- readOGR(dsn = "COL_adm", layer = "COL_adm1")
#o <- gadmColombia@data
gadmColombiaRoad = readOGR(dsn = "COL_rds", layer = "COL_roads")
gadmColombia@data <- merge(data.frame(gadmColombia@data), yield, by = 'NAME_1',
                           all.x = TRUE)

gadmColombia@data <- merge(data.frame(gadmColombia@data), r[, c(2,4)], by = 'NAME_1',
                           all.x = TRUE)


#write.csv(gadmColombia@data, file='test.csv', row.names = F)
#O <- gadmColombia@data
#plot(gadmColombia, col=gadmColombia@data$Region, border='gray', axes=FALSE)
levels(gadmColombia@data$departamento)
break
tmap_mode('view')

#png('map_region.png', width=1000, res=180)

m <- tm_shape(gadmColombia) +
  tm_polygons(col = "Region") +
  tm_layout(legend.outside = TRUE, legend.text.color = 'red') +
  tm_scale_bar(position=c("center", "center")) +
  tm_compass(type="radar", position=c("center", "top"), show.labels = 3)
tmap_save(m, "fig1_yield.tiff", width=1920, height=1080, asp=0)
dev.off()

# y <- merge(yield[,-2], r[, c(1,4)], by='departamento' )
# 
# 
# f <- reshape(y, idvar = c("departamento", 'Region'),  varying = list(names(y)[c(-1, -13)]),
#              direction = "long", times = names(y)[c(-1, -13)], v.names = 'yield')
# 
# 
# f$time <- gsub('X', '', f$time)
# aggdata <- aggregate(f, by=list(f$Region, f$time),  FUN=mean)
# aggdata <- aggdata[c(1,2,6)]
# colnames(aggdata) <- c('region', 'year', 'yield')
# 
# aggdata$year <- as.numeric(aggdata$year)
# 
# png('yield.png', width=1000, res=180)
# p2 <- ggplot(aggdata, aes(x = year, y = yield, colour = region, shape = region)) +
#   geom_line(size=1) + geom_point(size = 2)  + theme_bw() +
#   ylab("Yield (tons)") + xlab('Year') +
#   scale_x_continuous(breaks = 2009:2019, labels  = unique(aggdata$year)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10))
# dev.off()
# grid.arrange(p, p2, ncol=2, nrow = 1)


m2 <- gadmColombia@data <- mutate(gadmColombia@data, biomass=X2019*(90/100))
tm_shape(gadmColombia) +
  tm_polygons(col = "biomass",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)+
  tm_shape(gadmColombiaRoad) + 
  tm_lines(col = "brown") +
  tm_scale_bar(position=c("center", "center"))
save_tmap(m2, "fig3_biomass.tiff", width=1920, height=1080, asp=0)


# plot(gadmColombia, col=gadmColombia@data$X2019, border='gray', axes=FALSE)
# plot(gadmColombiaRoad, add=TRUE, density=20, lwd=1, col='black')
# # #break
# png('map.png', height=1000, width=1000, res=150)
# rv = list(gadmColombiaRoad, col = "white", lty=0.1, which=2, fill='transparent')
# rv1 = list(gadmColombiaRoad, col = "lightblue", which=2, fill='transparent')
# spplot(gadmColombia, c('X2019', 'ID_0'), sp.layout = list(rv, rv1), 
#       names.attr = c('Cacao yield 2019', 'Transp network'))
break



# 
# g1 = ggplot() + geom_sf(data = gadmColombia, aes(fill = 'region')) +
#    geom_sf(data = gadmColombia) +
#    scale_x_continuous(breaks = c(170, 175))
# 
# tm_shape(gadmColombia) + tm_fill("Region")
# #list(rv, rv1, rv)

cacao_data <- gadmColombia@data
cacao_data <- cacao_data[, 10:22]


f <- reshape(cacao_data, idvar = c("departamento", 'Region'),  
             varying = list(names(cacao_data)[c(-1, -13)]),
             direction = "long", 
             times = names(cacao_data)[c(-1, -13)], v.names = 'yield')


f$time <- gsub('X', '', f$time)
aggdata <- aggregate(yield ~ Region + time, data=f, FUN=sum)
#aggdata <- aggregate(f$yield, by=list(f$Region, f$time),  FUN=sum)
colnames(aggdata) <- c('Region', 'year', 'yield')

aggdata$year <- as.numeric(aggdata$year)
break
tiff('yield.tiff', width=1000, res=180)
ggplot(aggdata, aes(x = year, y = yield, colour = Region, shape = Region)) +
  geom_line(size=1) + geom_point(size = 2)  + theme_bw() +
  ylab("Yield (tons)") + xlab('Year') +
  scale_x_continuous(breaks = 2009:2019, labels  = unique(aggdata$year)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  scale_color_manual(values=c("#66CC99", "#FFFF66", "#9999CC", "#3399CC", "#FF9933"))
dev.off()
