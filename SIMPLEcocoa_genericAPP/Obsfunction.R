ObsInput=function(i){
  i=1
  Obsdata <-list()
  ###treatment filter 
  treat<-treatment[i,];#print(paste(treat$Exp[1],treat$Trt[1]))
  #########
  obs<-read.table(paste("./Observation/Obs_",treat$Species,"_",treat$Exp,".csv",sep=""),header=TRUE,sep=",",stringsAsFactors=FALSE)
  obs<-obs[obs$Trt. %in% (treat$Trt.),]
  obs$Exp=treat$Exp.
  obs$Label=treat$Label
  obs$Trt=obs$Trt.  
  #source("Mainfunction.R")
  sowingDate=DOYtoDate(treat$SowingDate)
  obs$Date=sowingDate+obs$DAP
  Summary=obs[nrow(obs),]
  obs$Crop=treat$Species.
  
  Obsdata$Biomass=obs[complete.cases(obs$Biomass),c("Date","DAP","Biomass","Trt","Exp","Crop","Label")]
  
  Obsdata$FSolar=obs[complete.cases(obs$FSolar),c("Date","DAP","FSolar","Trt","Exp","Crop","Label")]
  
  Obsdata$Yield=obs[complete.cases(obs$Yield),c("Date","DAP","Yield","Trt","Exp","Crop","Label")]
  
  Obsdata$Summary=Summary[,c("Date","DAP","Biomass","FSolar","Yield","Trt","Exp","Label")]
  
  
  return(Obsdata)
 
}

DOYtoDate=function(DATE)
{
  DATE=sprintf("%05d",as.numeric(DATE))
  YEAR<-as.numeric(substring(DATE,1,2))
  YEAR[YEAR>20]<-YEAR[YEAR>20]+1900;YEAR[YEAR<=20]<-YEAR[YEAR<=20]+2000
  DOY=as.numeric(substring(DATE,3,5))
  DATE=as.Date(DOY-1, origin = paste0(YEAR,"-01-01"))
  return(DATE)
}

