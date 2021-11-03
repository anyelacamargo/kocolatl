
#########################SIMPLE MODEL cocoa ###########################

#######  KOKOlatm Lanitoamenrican cocoa to predict the optimal harvest time 
#Updated by Angela Romero Vergel,NIAB,Cocoa project ------  2021-08-11 
##################################################################

library(ggplot2)
library(plyr)
library(grid)
source("Plot.R")
source("Mainfunction.R")
source("Obsfunction.R")

RunModel = function(i){
  source("Mainfunction.R")
  paras <- ParaInput(i)
  res<-tryCatch({SIMPLE(para=paras[c(1:3)],weather=paras$weather,
                          ARID=paras$ARID)},error=function(e){cat("ERROR :", conditionMessage(e),"\n")})
  return(res)
}


#INPUTS from app
# add here region:Santander, Apartado, Arauca, Cali o Caldas
#region= "Caldas" 
#pass date input to DOY
#enter here date of flowering year-month-day
#fecha_floracion <- as.Date("2020-06-23") 
#date <- format(fecha_floracion, format="%Y-%b-%d")
year <- format(fecha_floracion, format="%y")
day <- format(fecha_floracion, format="%j")
doy <- as.numeric(paste(year, day, sep = ''))


organise_data <- function(GridsimulationSwitch){
  
  if(GridsimulationSwitch=='OFF'){
    management<-read.table("./Input/Simulation Management.csv",header=TRUE,sep=","
                           ,stringsAsFactors=FALSE)
    treatment<-read.table("./Input/Treatment.csv",header=TRUE,sep=","
                          ,stringsAsFactors=FALSE);treatment$Species.=tolower(treatment$Species.)
    treatment$SowingDate <- doy #added Angela
    cultivar<-read.table("./Input/Cultivar.csv",header=TRUE,sep=","
                         ,stringsAsFactors=FALSE);cultivar$Species.=tolower(cultivar$Species.)
    
    soil<-read.table("./Input/Soil.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)  
    para_spec<-read.table("./Input/Species parameter.csv",header=TRUE,sep=","
                          ,stringsAsFactors=FALSE);
    para_spec$Species.=tolower(para_spec$Species.)
    
    ####match experiment
    management<-management[management$Label==region,]
    treatment<-merge(treatment,management,by=c("Species.","Exp.","Trt.")
                     , suffixes=c("",".y"))
    treatment<-merge(treatment,soil,by="SoilName.")
    treatment<-merge(treatment,para_spec,by="Species.")
    treatment<-merge(treatment,cultivar,by=c("Species.","Cultivar."))
  }else{
    treatment<-read.table("./Input/Grid_input.csv",header=TRUE,sep=","
                          ,stringsAsFactors=FALSE);treatment$Species.=tolower(treatment$Species.)
                          para_spec<-read.table("./Input/Species parameter.csv"
                                                ,header=TRUE,sep=",",
                                                stringsAsFactors=FALSE);para_spec$Species.=tolower(para_spec$Species.)
                          irri<-read.table("./Input/Irrigation.csv",header=TRUE,sep=","
                                           ,stringsAsFactors=FALSE);irri$Species.=tolower(irri$Species.)
                          treatment<-merge(treatment,para_spec,by="Species.")
                          treatmentsingle<-treatment
                          x=1:length(SimulatingYear)
                          no_cores <- detectCores() - 1
                          cl <- makeCluster(mc <- getOption("cl.cores", no_cores))
                          clusterExport(cl, c("treatment","SimulatingYear"))
                          results <- parLapply(cl,x,Treatmentplusyear) 
                          stopCluster(cl)
                          treatment <- do.call('rbind',results) 
  }
  
  return(list('treatment'=treatment))
  
}


######weather directory for regional simulation###
WeatherDir="./Gridcell Weather/historical/"
WeatherType=c("WTH","CSV","Rdata")[1]
GridsimulationSwitch=c('OFF','ON')[1]  
########1=single point simulation, 2= Grid cell simulation
########## Output option################
DailyOutputforgridcell=c('OFF','ON')[2]
DailyOutputOutput=c("Crop","Exp","Label","Trt","Day","DATE","Tmax","Tmin","Radiation",
                    "TT","fSolar","Biomass","dBiomass","HI","Yield","F_Temp","F_Heat",
                    "F_Water","ARID","I50B","I50A","ETO","MaturityDay")

############  Model starts here  ###########################
bundle <- organise_data(GridsimulationSwitch)
treatment <- bundle$treatment


p <- list("treatment" = treatment,"GridsimulationSwitch" = GridsimulationSwitch,
          "WeatherType" = WeatherType, "WeatherDir" = WeatherDir,
          "DailyOutputOutput" = DailyOutputOutput)

results <- RunModel(p)

Res_daily=as.data.frame(results[['Daily']])
Res_summary=as.data.frame(results[['Summary']])
print(paste("Region:", region))
print(paste("Dia de cosecha:", Res_summary$MaturityDay))
print(paste("Rendimiento potencial:", Res_summary$Yield,"kg.ha-1 año"))


