#########################  Main model  ###################################################################################################################
# ********************* Reference for a developer *****************************************
# ET model can be selected using "petmodel". 
# The value of petmodel is assigned by PET_OPTION, which is defined in the top section.
# no change is needed here to selet a ET model. 
##################################################################################################################


SIMPLE<-function(para,weather,ARID)
{
  #para=paras[c(1:3)];weather=paras$weather;ARID=paras$ARID
#
  #############  read crop and cv parameter
  Trt=para$Species$Trt.
  Tbase<-para$Species$Tbase
  Topt<-para$Species$Topt
  RUE<-para$Species$RUE
  I50maxH<-para$Species$I50maxH
  I50maxW<-para$Species$I50maxW
  maxT<-para$Species$MaxT
  extremeT<-para$Species$ExtremeT
  S_Water<-para$Species$S_Water 
  CO2_RUE<-para$Species$CO2_RUE
  
  Tsum<-para$Cultivar$Tsum
  HIp<-para$Cultivar$HI
  I50A<-para$Cultivar$I50A
  I50B<-para$Cultivar$I50B
  
  inifsolar<-para$treatment$InitialFsolar
  Fsolarmax<-ifelse(is.na(para$treatment$MaxIntercept),0.95,para$treatment$MaxIntercept)
  sowingDate<-para$treatment$SowingDate
  harvestDate<-para$treatment$HarvestDate
  CO2<-para$treatment$CO2
  water<-para$treatment$Water
  FHarv<-400
  
  
  if(is.na(harvestDate)){
    DUA=NA
  }else {
    DUA<-as.numeric(harvestDate-sowingDate)+1
    weather<-weather[1:DUA,]
    ARID1<-ARID[1:DUA,]
  }
  
  stopday<-ifelse(nrow(weather)>FHarv,FHarv,nrow(weather))
  weather<-weather[1:stopday,]
  ARID<-ARID[1:stopday,]
  MaturityDay2<-stopday
  
  
  Result=data.frame(Day = c(1:stopday),DATE=weather$IDATE[1:stopday],
                    ETO=rep(0,stopday), TT=rep(0,stopday),Biomass=rep(0,stopday),
                    Tmax=weather$TMAX[1:stopday],Tmin=weather$TMIN[1:stopday],
                    Radiation=weather$SRAD[1:stopday], HI=rep(HIp,stopday), 
                    ARID=ARID[1:stopday,2], Trt=rep(Trt,stopday))
  
  Result$F_Water=mapply(Water_Response,ARID$ARID,S_Water);if(!water){Result$F_Water=1}
  Result$dETO<-ARID$ETO
  Result$F_Heat<-mapply(Heat_Response,weather$TMAX,maxT,extremeT)
  Result$Tmean<-(weather$TMIN+weather$TMAX)/2
  Result$F_Temp<-mapply(T_Response,Result$Tmean,Tbase,Topt)
  Result$F_CO2<-rep(CO2_Response(CO2,CO2_RUE),stopday)
  Result$dTT <- mapply(DTTfunction,Result$Tmean,Tbase)
  Result$TT[2:stopday]<-cumsum(Result$dTT)[1:stopday-1]
  Result$TT<-Result$TT+para$treatment$InitialTT
  Result$Yield<-rep(0,stopday) 
  
  ## dI50A I50B
  Result$I50A<-rep(I50A,stopday)
  Result$I50B<-rep(I50B,stopday)
  
  ##f fSolar_water
  fSolar_water<-Result$F_Water
  fSolar_water[fSolar_water>0.1]<-1
  fSolar_water[fSolar_water<=0.1]<-fSolar_water[fSolar_water<0.1]+0.9
  Result$fSolar_water=fSolar_water
  Result$fSolar<-rep(inifsolar,stopday)
  
  ##fSolar I50B
  for(day in 2:stopday)
  {
    fSolar1<-min(1,Fsolarmax/(1+exp(-0.01*(Result$TT[day]-Result$I50A[day-1]))))
    fSolar2<-min(1,Fsolarmax/(1+exp(0.01*(Result$TT[day]-(Tsum-Result$I50B[day-1])))))
    
    Result$fSolar[day]<-min(fSolar1,fSolar2)*min(Result$fSolar_water[day-1],1)
    
    dI50B1<-Result$I50B[day-1]+I50maxW*(1-Result$F_Water[day-1])
    dI50B2<-Result$I50B[day-1]+I50maxH*(1-Result$F_Heat[day-1]) 
    Result$I50B[day]=max(max(0,dI50B1),max(0,dI50B2))
    
    if(Result$fSolar[day]<Result$fSolar[day-1] && Result$fSolar[day]<=0.005)
    {	
      #print("Crop maturity due to senescence!!!!!!!!!!");
      MaturityDay2=day
      break
    }
    
  }
  
   ###### Stop day
  MaturityDay1<-nrow(Result[Result$TT<=Tsum,])
  if(MaturityDay1<MaturityDay2){
    MaturityDay<-MaturityDay1
    #print("Crop maturity!!!!!!!!!!")
  }else{
    MaturityDay=MaturityDay2
  print("Crop maturity due to senescence!!!!!!!!!!")
  }
  
  Result[['SowingDate']] <-sowingDate
  Result$MaturityDay<-MaturityDay+sowingDate
  Result=Result[1:MaturityDay,]
  
  Result$ETO[2:MaturityDay]<-cumsum(ARID$ETO)[1:MaturityDay-1]
  Result$dBiomass <-mapply(Biomassfunction,Result$fSolar,Result$Radiation,Result$F_CO2,Result$F_Temp,Result$F_Water,Result$F_Heat,RUE)
  Result$Biomass[2:MaturityDay]<-cumsum(Result$dBiomass)[1:MaturityDay-1]
  Result$Biomass<- round(Result$Biomass+para$treatment$InitialBio,3)
  Result$Yield<-round(Result$Biomass*HIp,3)
  Result$Crop<-para$Species$Species.
  Result$Exp<-para$treatment$Exp.
  Result$Label<-para$treatment$Label
  
  
  res=list()
  res$Daily=Result[1:MaturityDay,DailyOutputOutput]
  res$Summary=data.frame(Crop=para$Species$Species.,Exp=para$treatment$Exp.,Label=para$treatment$Label,Trt=para$Species$Trt.,
                         SowingDate=round(Result$SowingDate[nrow(Result)],0),
                         Duration=nrow(Result),Biomass=round(Result$Biomass[nrow(Result)],0),
                         Yield=round(Result$Yield[nrow(Result)],0),MaturityDay=round(Result$MaturityDay[nrow(Result)],0),
                         stringsAsFactors=FALSE)
  
  
  return(res) 
  #return(Result[1:MaturityDay,DailyOutputOutput])
}


#########caculate degree day
DTTfunction=function(Tmean,Tbase)
{
  dTT<-max(Tmean-Tbase,0)
  return(dTT)
}

Biomassfunction=function(fSolar,SRAD,F_CO2,F_Temp,F_Water,F_Heat,RUE)
{
  dBiomass <-10*RUE*fSolar*SRAD*F_CO2*F_Temp*min(F_Water,F_Heat)
  return(dBiomass)
}


T_Response<-function(Tmean,Tbase,Topt)
{
  if(Tmean>=Topt){return(1)}
  else
  {		
    return(max((Tmean-Tbase)/(Topt-Tbase),0))
  }
}

CO2_Response<-function(CO2,CO2_RUE)
{
  if(CO2>=700){return(1+CO2_RUE*350/100)}
  else
  {
    return(max((CO2_RUE*CO2*0.01+1-0.01*350*CO2_RUE),1))
  }
  
}

Water_Response<-function(arid,s_water)
{
  water<-max(0,1-s_water*arid)
  return (water)
}

Heat_Response<-function(tmax,maxT,extremeT)
{
  if(tmax<=maxT){return(1)}
  if(tmax>extremeT){return(0)}
  else{
    return(max(1-(tmax-maxT)/(extremeT-maxT),0))
  }
}

######multiyear treatment
Treatmentplusyear=function(i)
{
  HarvestDatefun=function(SowingDate,HarvestDate)
  {
    HarvestDate=sprintf("%2s%03d",substr(Year,3,4),HarvestDate)
    if(HarvestDate<SowingDate){HarvestDate=sprintf("%2s%03d",substr(Year+1,3,4),HarvestDate)}
    return(HarvestDate)
  }
  Year=SimulatingYear[i]
  treatment$SowingDate=sprintf("%2s%03d",substr(Year,3,4),treatment$SowingDate)
  treatment$HarvestDate=mapply(HarvestDatefun,treatment$SowingDate,treatment$HarvestDate)
  return(treatment)
}




########input function
########input function
ParaInput=function(i)
{
  para <-list()
  ###treatment filter 
  #treat <- i$treatment[i,];
  treat <- i$treatment
  #FIXME Added Anyela
  #print(paste(treat$Exp[1],treat$Trt[1]))
  
  #i$#i$###parameter input
  ################## [Species Parameter Selection] 	 ##################
  para[['Species']] = treat[,c("Trt.","Species.","Tbase" , "Topt", "RUE", "I50maxH","I50maxW", "MaxT" , "ExtremeT","CO2_RUE" ,"S_Water" )]
  
  ################## [culitvar Parameter Selection] 	 ##################
  para[['Cultivar']] =treat[,c("Trt.","Tsum","HI" , "I50A", "I50B")]
  
  ################## [management Parameter Selection] 	 ##################
  para[['treatment']] =treat[,c("Trt.","Exp.","Label","CO2","SowingDate","HarvestDate","MaxIntercept","Water","InitialBio","InitialTT","InitialFsolar")]
  
  
  ################## [irrigation Selection] 	 ##################
  #irrigation<-irri[irri$Species.==treat$Species. &irri$Exp.==treat$Exp. & irri$Trt.==treat$IrrigationTrt,]#print(irrigation);
  
  
  ###weather name
  
  weaName=treat$Weather[1]
  
  #source("Mainfunction.R")
  ###Add irrigation amounts to rainfall
  #FIXME Added Anyela
  
  weather = WeatherFunction(weaName,irrigation = NA, GridsimulationSwitch,WeatherType,WeatherDir)
  #weather=WeatherFunction(weaName,irrigation,GridsimulationSwitch,WeatherType,WeatherDir)
  ###read Lat and Elev from weather
  if(GridsimulationSwitch=='OFF'){
    weaheader=readLines(paste0("./Weather/",weaName,".WTH"),n=4)[4]
    Lat=as.numeric(substr(weaheader,8,16))
    Elev=as.numeric(substr(weaheader,26,31))}else{
      Lat=treat$lat
      Elev=0
    }
  
  
  sowingDate=para$treatment$SowingDate=DOYtoDate(para$treatment$SowingDate)
  if(!is.na(para$treatment$HarvestDate)){para$treatment$HarvestDate=DOYtoDate(para$treatment$HarvestDate)}
  
  para$weather<-weather[which(weather$IDATE==sowingDate):nrow(weather),]
  
  
  
  soil<-list(AWC=treat$AWC[1],RCN=treat$RCN[1],DDC=treat$DDC[1],WUC=0.096,RZD=treat$RZD[1])
  
  ###Calculate ARID
  para$ARID<-ARIDFunction(weather=para$weather,soil=soil,Lat,Elev)
  
  
  ####water stress switch
  para$treatment$Water=ifelse(treat$Water[1]=="yes" || treat$Water[1]==1,TRUE,FALSE)
  
  
  return(para)
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

WeatherFunction=function(weatherName,irrigation=NA,GridsimulationSwitch,WeatherType='WTH',WeatherDir)
{
  
  #############  read weather 
  if(WeatherType=='WTH'){
    if(GridsimulationSwitch=='OFF'){
      weather<-read.table(paste0('./weather/',weatherName,".WTH"),header=TRUE,skip=4)
    } else {
      weather<-read.table(paste0(WeatherDir,weatherName,".WTH"),header=TRUE,skip=3)
    }
  }else if(WeatherType=='CSV'){
    weather<-read.table(paste(weatherName,".csv",sep=""),header=TRUE,sep=",",stringsAsFactors=FALSE)[,c(1:5)]}else if(WeatherType=='Rdata'){
      eval(parse(text=paste0("load('",WeatherDir,weatherName,".RData')")))
      eval(parse(text=paste0("weather=",weatherName)))
      eval(parse(text=paste0("rm(",weatherName,")")))
    }
  #############  this makes sure there are 5 characters for Date in weather data
  if(colnames(weather)[1]!="DATE"){colnames(weather)[1]="DATE"}
  
  #tranfer DOY to DATE
  weather$DATE=sprintf("%05d",as.numeric(weather$DATE))
  weather$IDATE=DOYtoDate(weather$DATE)
  # FIXME Anyela
  #if(!is.na(irrigation[1,1])){irrigation$IrrDate=DOYtoDate(irrigation$IrrDate)}
  if(!is.na(irrigation)){irrigation$IrrDate=DOYtoDate(irrigation$IrrDate)}
  
  #############    remove letter if weather includes letter
  Dropletter=function(weather)
  {
    if(!is.numeric(weather)){
      if(!require(stringr)) {install.packages("stringr")}
      library(stringr)
      weather=lapply(str_extract_all(weather, "[^[:alpha:]]"),paste,collapse = "")%>%as.numeric(.) }
    return(weather)
  }
  
  weather$TMAX<-Dropletter(weather$TMAX)
  weather$TMIN<-Dropletter(weather$TMIN)
  weather$SRAD<-Dropletter(weather$SRAD)
  weather$RAIN<-Dropletter(weather$RAIN)
  
  #############  add irrigation to rainfall  
  # FIXME Anyela
  #if(nrow(irrigation)>0)
  if(!is.na(irrigation))
  {
    for(i in 1:nrow(irrigation))
    {
      dateIrri<-irrigation$IrrDate[i]
      
      weather[weather$IDATE==dateIrri,"RAIN"]=weather[weather$IDATE==dateIrri,"RAIN"]+irrigation$IrrAmount[i]
    }
  }
  
  return(weather)
}

############## Priestley Taylor Evapotranspiration ###
petpt<-function(msalb, srad, tmax, tmin, xhlai)
{
  td<- 0.6 * tmax + 0.4 * tmin
  
  if (xhlai <= 0) {
    albedo<-msalb
  } else {
    albedo<- 0.23- (0.23-msalb) * exp(-0.75* xhlai)
  }
  
  slang <- srad * 23.923
  eeq <- slang * (2.04E-4 - 1.83E-4 * albedo) * (td + 29.0)
  
  pt <- eeq * 1.1
  
  if (tmax > 35) {
    pt <- eeq * ((tmax - 35.0) * 0.05 + 1.1)
  } else if (tmax < 5.0) {
    pt <- eeq * 0.01 * exp(0.18 * (tmax + 20))
  }
  
  if (pt < 0.0001) {
    pt <- 0.0001
  }
  
  return (pt)
}

##############   ARID  #############################
##############   ARID  #############################
ARIDFunction<-function(weather,soil,Lat,Elev)
{
  
  Elev=ifelse(Elev==-99||is.na(Elev),0,Elev)
  #########  Note: Make sure that your dataset has the same columns as above. Otherwise, change codes accordingly.
  petmodel = 'PT'
  dat<-weather
  dat<-data.frame(SRAD=dat$SRAD,TMAX=dat$TMAX,TMIN=dat$TMIN,DEWPOINT=dat$TMIN,RAIN=dat$RAIN,
                  WINDSPEED=rep(1,nrow(dat)),DOY=as.numeric(substring(dat$DATE,3,5)),YEAR=as.numeric(format(dat$IDATE,"%Y")));
  
  rain <- dat[,5]                                                   # The 5th column of the data is PRECIPITATION
  
  ######## 3. set up Constants for ARID
  
  latitude <-Lat                                               # LATITUDE of the weather station (degree)
  elevation <-Elev                                               # ELEVATION of the weather station (m)
  #print(soil)
  
  #######  4. set up Parameters for ARID: The following parameter values are default, can be overwritten from Inputs.
  AWC <- 0.13
  DDC <- 0.55
  RCN <- 65
  RZD <- 400
  WUC <- 0.096
  
  AWC <- soil$AWC          #####  available water capacity (Vol/Vol)
  DDC <- soil$DDC          #####  deep drainage coefficient (-)
  RCN <- soil$RCN          #####  runoff curve number (-)
  RZD <- soil$RZD          #####  rootzone depth (mm)     
  WUC <- soil$WUC          #####  water uptake coefficient (-)    
  
  #######  5. ARID Computation (computes a vector of ARID using the dataset)
  ETO <- 0    #########  evapotranspiration
  WBD <- 0    #########  ?
  WAT <- 0    #########  ?
  ARID<- 0    #########  ARID index
  lat <- latitude*pi/180 #######  latitude
  PSC <- 0.665*10^-3*101.3*((293-0.0065*elevation)/293)^5.26   ############  ?
  for(i in 1:length(dat[,1])) {
    
    ##########  5.1 First, compute ETo (using the FAO-56 method)
    if(dat[i,8]%%4==0) {
      days <- 366
    }
    else days <- 365
    ws2 <- dat[i,6]*4.87/log(67.8*10-5.42)
    es <- ((0.6108*exp(17.27*dat[i,2]/(dat[i,2]+237.3)))+(0.6108*exp(17.27*dat[i,3]/(dat[i,3]+237.3))))/2
    slope <- (0.6108*exp(17.27*((dat[i,2]+dat[i,3])/2)/(((dat[i,2]+dat[i,3])/2)+237.3))*4098)/((dat[i,2]+dat[i,3])/2+237.3)^2
    SWR <- (1-0.23)*dat[i,1]
    IRDES <- 1+0.033*cos(2*pi*dat[i,7]/days)
    SD <- 0.409*sin(2*pi*dat[i,7]/days-1.39)
    SSA <- acos(-tan(lat)*tan(SD))
    extra <- 24*60*0.082/pi*IRDES*(SSA*sin(lat)*sin(SD)+cos(lat)*cos(SD)*sin(SSA))
    CSR <- (0.75+2*10^-5*elevation)*extra
    RRAD <- dat[i,1]/CSR
    ea <- 0.6108*exp(17.27*dat[i,4]/(dat[i,4]+237.3))
    LWR <- 4.903*10^-9*((dat[i,2]+273.16)^4+(dat[i,3]+273.16)^4)/2*(0.34-0.14*sqrt(ea))*(1.35*RRAD-0.35)
    NRAD <- SWR-LWR
    ETO[i] <- (0.408*slope*NRAD+PSC*(900/((dat[i,2]+dat[i,3])/2+273))*ws2*(es-ea))/(slope+PSC*(1+0.34*ws2))
    
    if (petmodel == 'PT') {
      albedo<-0.23
      xhlai<- -99
      ETO[i] <- petpt(albedo, dat[i, 1], dat[i, 2], dat[i, 3], xhlai)
    }
    
    #########  5.2 Then, compute ARID
    if(rain[i]>0.2*(25400/RCN-254)) {
      RO <- (rain[i]-0.2*(25400/RCN-254))^2/(rain[i]+0.8*(25400/RCN-254))
    }
    else RO <- 0
    CWBD <- rain[i]- RO
    if(i==1) {
      W_AT <- RZD*AWC                 ############  initial value for 'Water after transpiration', which is assumed to be AWC (0.13)
    }
    else W_AT <- WAT[i-1]
    WBD[i] <- CWBD + W_AT
    if(WBD[i]/RZD > AWC) {
      DR <- RZD*DDC*(WBD[i]/RZD - AWC)
    }
    else DR <- 0
    WAD <- WBD[i] - DR
    TR <- min(WUC*RZD*WAD/RZD,ETO[i])
    WAT[i] <- WAD - TR
    ARID[i] <- 1-TR/ETO[i]
  }
  return(data.frame(DATE=weather$IDATE,ARID=ARID,ETO=ETO))
}


