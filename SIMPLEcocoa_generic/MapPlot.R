list.of.packages <- c("RColorBrewer", "viridis","raster","RColorBrewer","rasterVis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(RColorBrewer)
library(viridis)  
library(raster)
library(RColorBrewer)
library(rasterVis)


Transfer_to_raster=function(SummaryData,index){
  newnarry=array(NA,dim = c(360,720))
  newnarry=as.matrix(newnarry)
  dim(newnarry)
  
  for(i in 1:nrow(SummaryData))
  {
    rows=SummaryData$row[i]
    cols=SummaryData$col[i]
    eval(parse(text=paste0("newnarry[rows,cols]<-SummaryData$",index,"[i]")))
  }
  
  newras=raster(newnarry,xmn=-180,xmx=180,ymn=-90,ymx=90)
  proj4string(newras)=CRS("+proj=longlat +datum=WGS84")
  return(newras)
}

MapPlot=function(yieldirra=Res_Summary,index="Yield",WMap,MapExtention,Title="Yield",Unit){
  
  ras=Transfer_to_raster(yieldirra,index = index)
  
  if(!MapExtention=="World"){
    ras <- mask(x=ras, mask=WMap)
    ras<-crop(ras,WMap)}
  b2p1 <- colorRampPalette(c("green","sky blue","blue","purple","yellow","Orange Red","red"))
  #b2p1 <- colorRampPalette(c("grey90","grey40","black"))
  pal8 <-  b2p1(100)
  
  if(MapExtention==c("China")){
    load("H:/NC/China.RData")
    Wapad=WMap
    WMap=boundary
    
  }
  
  
  #######Simply map
  library(rgeos)
  WMap_sim2 <- gSimplify(WMap, tol = .1, 
                               topologyPreserve = TRUE)
  
  mapTheme <- rasterTheme(region=pal8)
  windows(width=16, height=8)
  par(mar=c(4,10,4,10))
  plot(WMap_sim2,axes = TRUE,main=Title,cex.axis=1.3)
  plot(ras,col=b2p1(100),#labels=pretty(0:14000),#main="Yield", axes=FALSE,
       legend.width=2.5, legend.shrink=0.75,
       legend.args=list(text=Unit, side=3, font=1, line=1, cex=1.2),
       smallplot=c(0.91,.93, .2,.8),add=T
  )
  if(MapExtention==c("China")){ plot(Wapad,axes=F,add=T)}
  
  # library(mapview)
  # 
  # leaflet(WMap_sim2) %>%
  #   addPolygons(color = "black", weight = 0.8,smoothFactor = 0.2,fillOpacity = 0,
  #               opacity = 0.5)%>% addTiles() %>%
  #   addRasterImage(ras, colors = pal8, opacity = 0.8) 
}








