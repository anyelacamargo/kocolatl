#########################SIMPLE MODEL v1.1###########################

#######  KOKOlatm Lanitoamenrican cocoa to predict the optimal harvest time 
#Updated by Angela Romero Vergel,NIAB,Cocoa project ------  2021-08-11 
##################################################################

#rm(list=ls())   #### cleans memory - needs to saty here
library(ggplot2)
library(plyr)
library(parallel)
#setwd("C:/Users/Angelita/Documents/SIMPLEcocoa_Generic")
source("Plot.R")
source("Mainfunction.R")
source("Obsfunction.R")

RunModel=function(i){
  source("Mainfunction.R")
  paras=ParaInput(i)
  res<-tryCatch({SIMPLE(para=paras[c(1:3)],weather=paras$weather,
                        ARID=paras$ARID)},error=function(e){cat("ERROR :", conditionMessage(e),"\n")})
  return(res)
}

organise_data <- function(GridsimulationSwitch){
  
  if(GridsimulationSwitch=='OFF'){
    management<-read.table("./Input/Simulation Management.csv",header=TRUE,sep=","
                           ,stringsAsFactors=FALSE)
    treatment<-read.table("./Input/Treatment.csv",header=TRUE,sep=","
                          ,stringsAsFactors=FALSE);treatment$Species.=tolower(treatment$Species.)
    
    cultivar<-read.table("./Input/Cultivar.csv",header=TRUE,sep=","
                         ,stringsAsFactors=FALSE);cultivar$Species.=tolower(cultivar$Species.)
    
    irri<-read.table("./Input/Irrigation.csv",header=TRUE,sep=","
                     ,stringsAsFactors=FALSE);irri$Species.=tolower(irri$Species.)
    soil<-read.table("./Input/Soil.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)  
    para_spec<-read.table("./Input/Species parameter.csv",header=TRUE,sep=","
                          ,stringsAsFactors=FALSE);para_spec$Species.=tolower(para_spec$Species.)
    
    ####match experiment
    management<-management[management$ON_Off==1,]
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
  
  return(list('treatment'=treatment, 'irri' = irri))
  
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
irri <- bundle$irri
treatment <- bundle$treatment

########parallel running
#t1=Sys.time() 
#x=1:nrow(treatment)
#no_cores <- detectCores() - 1
cl <- makeCluster(mc <- getOption("cl.cores", no_cores))
clusterExport(cl, c("treatment","irri","GridsimulationSwitch","WeatherType",
                    "WeatherDir","DailyOutputOutput"))
results <- parapply(cl,x,RunModel) 
if(GridsimulationSwitch=='OFF')
{
 
  observations<- parapply(cl,x,ObsInput) 
}

#stopCluster(cl)
#Sys.time()-t1
###########

#########Simulation results reorganization
res.df <- do.call('rbind',results) 
Res_daily=ldply(res.df[,1])
Res_summary=ldply(res.df[,2])

# Call organise_data
if(GridsimulationSwitch=='OFF'){
  obs.df=do.call('rbind',observations)
  Obs_Biomass=ldply(obs.df[,1])
  Obs_FSolar=ldply(obs.df[,2])
  Obs_Yield=ldply(obs.df[,3])
  Obs_Summary=ldply(obs.df[,4])
  Obs_Summary=Obs_Summary[,c('Trt','Exp','Yield')]
  Res_Summary=merge(Res_summary,Obs_Summary,by=c('Trt','Exp'))[,
      c("Crop","Exp","Label","Yield.x","Yield.y","Duration","Trt")]
  names(Res_Summary)[4:5]=c("Sim_Yield","Obs_Yield")
  ##### write the simulations into files
  Yeargap=paste(unique(format(Res_summary$MaturityDay,"%Y")),collapse = "_")
  Speciesgap=paste(unique(Res_summary$Crop),collapse = "_")
  write.table(Res_daily,paste("./Output/Res_daily_",Speciesgap,"_",
         Yeargap,".csv",sep=""), col.names=TRUE,row.name=FALSE,sep=",")
  write.table(Res_summary,paste("./Output/Res_summary_",Speciesgap,"_",Yeargap,
              ".csv",sep=""),col.names=TRUE,row.name=FALSE,sep=",")
  

  gplot(Res_daily,Res_Summary,Obs_Biomass,Obs_FSolar)
}else{
  treatmentsub=treatmentsingle[,c('Exp.','Species.','Trt.','row','col','lat')]
  Res_summary=Res_summary[,c("Exp","SowingDate","Duration","Biomass","Yield",
                             "MaturityDay")]
  Res_Summary=merge(treatmentsub,Res_summary,by.x="Exp.",by.y = "Exp")
  Yeargap=paste(unique(format(Res_Summary$MaturityDay,"%Y")),collapse = "_")
  print(Res_Summary$MaturityDay)
  if(DailyOutputforgridcell=='ON'){
    write.table(Res_daily,paste("./Output/Gridcell_daily_",Res_Summary$Species.[1],
            "_",Yeargap,".csv",sep=""),col.names=TRUE,row.name=FALSE,sep=",")
  }
  Res_Summary$MaturityYear=substr(Res_Summary$MaturityDay,1,4)
  write.csv(Res_Summary,paste0("./Output/Gridcell_summary",Res_Summary$Species.[1],
                               "_",Yeargap,".csv"),row.names = F)
  
  Res_SummaryMap=Res_Summary[Res_Summary$MaturityYear==MapoutputYear,]
  
 }
