#####Plot modified by A Romero


Plot_theme <-function(..., bg='transparent')
{
  require(grid)
  theme_bw(...) +
    theme( rect=element_rect(fill=bg),
           legend.position="right",  
           legend.text=element_text(size=12,colour="black"), 
           legend.key=element_blank(), 
           plot.title=element_text(size=12, colour="black",face="bold",hjust=0.5, vjust=0), 
           plot.margin=unit(rep(0.5,4), 'lines'), 
           panel.background=element_rect(fill='transparent', color='transparent'),
           panel.grid=element_blank(),
           axis.title=element_text(size=15,colour="black",  face="bold",vjust=0.1),
           axis.line = element_blank(),
           axis.ticks = element_line(colour = "black", size = 1, linetype = 1),
           axis.text=element_text(size=12,colour="black"), 
           axis.text.x = element_text(margin=margin(15,15,10,5,"pt"),face="bold",angle=0), 
           axis.text.y = element_text(margin=margin(15,15,10,5,"pt"),face="bold"),
           strip.text=element_text(size=12,colour="black"), 
           strip.background=element_blank()   
    )
  
}
theme_set(Plot_theme()) #####set theme by Liujun

####mutiplot 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

gplot=function(Res_Daily,Res_Summary,Obs_Biomass,Obs_FSolar){
    Res_Daily$Trt=paste(Res_Daily$Exp,"_Trt_",Res_Daily$Trt,sep="")
    Res_Daily$LableCrop=paste0(Res_Daily$Crop,"_",Res_Daily$Label)
    
    # P1=ggplot()+geom_line(data=Res_Daily,aes(x=Day,y=Biomass,colour=LableCrop),size=1)+
    #   scale_x_continuous(limits=c(0,(max(Res_Summary$Duration)+10)))+ 
    #   labs(title='(b) Biomass',x="Day after planting (d)",y="Biomass (kg/ha)")+
    #   theme(legend.text=element_text(size=10),
    #         legend.title=element_blank(),legend.justification=c(0,1),legend.position=c(0,1),
    #         plot.title=element_text(size=14, hjust=0, vjust=0))
    # 
    # 
    # P2=ggplot()+geom_line(data=Res_Daily,aes(x=Day,y=fSolar,colour=LableCrop),size=1)+
    #   scale_x_continuous(limits=c(0,(max(Res_Summary$Duration)+10)))+
    #   labs(title='(c) FSolar',x="Day after planting (d)")+
    #   theme(legend.position = "none",
    #         plot.title=element_text(size=14, hjust=0, vjust=0))
    
    cosecha=Res_daily$MaturityDay[1]# FIXME Angela
    R_summary<-Res_Summary[,c(-6,-7)] 
    R_summary<-R_summary[order(R_summary$Crop),]
    R_summary2<-Res_Summary[,c(-5,-6,-7)] # FIXME Angela
    print(R_summary2)# FIXME Angela
    
    ##### calculate RMSE and RRMSE for yield
    R_summary<-R_summary[!is.na(R_summary$Obs_Yield),]
    RMSE_yield<-sqrt(mean((R_summary$Sim_Yield-R_summary$Obs_Yield)^2))
    RRMSE_yield<-RMSE_yield/mean(R_summary$Obs_Yield)
    
    #print(paste("RMSE for yield is: ",round(RMSE_yield,0),"kg.ha-1"))
    #print(paste("Error RRMSE for yield is: ",100*round(RRMSE_yield,3),"%",sep=""))
    print(paste("Dia de cosecha:", cosecha))
    print(paste("Rendimiento potencial:", Res_summary$Yield,"kg.ha-1 año"))
    
    Obs_Biomass$LableCrop=paste0(Obs_Biomass$Crop,"_",Obs_Biomass$Label)
    Obs_FSolar$LableCrop=paste0(Obs_FSolar$Crop,"_",Obs_FSolar$Label)
    # P3=P1+geom_point(data=Obs_Biomass,aes(x=DAP, y=Biomass, colour=LableCrop),size=3)  #change to Label by Liujun
    # P4=P2+geom_point(data=Obs_FSolar,aes(x=DAP, y=FSolar, colour=LableCrop),size=3)  #change to Label by Liujun
    # 
    
    
    ###########plot bar graph by Liujun########################
    #install.packages("tidyr")
    # library(tidyr)
    # R_summaryplot=R_summary%>%gather(yield,yieldvalue,Obs_Yield,Sim_Yield)
    R_summaryr1=data.frame(Crop=R_summary$Crop,Label=R_summary$Label,yield="Obs",yieldvalue=R_summary$Obs_Yield)
    R_summaryr2=data.frame(Crop=R_summary$Crop,Label=R_summary$Label,yield="Sim",yieldvalue=R_summary$Sim_Yield)
    R_summaryplot=rbind(R_summaryr1,R_summaryr2)
    R_summaryplot$LableCrop=paste0(R_summaryplot$Crop,"_",R_summaryplot$Label)
    
    maxvalue=max(R_summaryplot$yieldvalue)*1.21
    cbPalette <- c("black", "white")
    cbstroke <- c("black","black")
    
    annotateText <- paste("RRMSE = ",100*round(RRMSE_yield,3),"%",sep="")
    
    
    # P5=ggplot(data=R_summaryplot,aes(x=LableCrop,y=yieldvalue))+
    #   geom_bar(stat="identity", width=.5, aes(fill=yield,colour=yield), position="dodge")+
    #   labs(title='(a) Yield', 
    #        x="Treatment", 
    #        y="Yield (kg/ha)")+ 
    #   scale_y_continuous(limits = c(0,maxvalue),breaks = seq(0,maxvalue,2000))+
    #   scale_fill_manual(values=cbPalette)+
    #   scale_colour_manual(values=cbstroke)+
    #   geom_text(aes(x=Inf,y=Inf,hjust=1.1,vjust=1.5,label=annotateText))+
    #   theme(axis.text.x = element_text(angle=35, vjust=1,hjust=1),
    #         plot.subtitle=element_text( colour="black",hjust=0.5, vjust=-1),
    #         legend.justification=c(0,1),legend.position=c(0,1),
    #         legend.background = element_blank(), legend.title = element_blank(),
    #         plot.title=element_text(size=14, hjust=0, vjust=0))
    # 
    # 
    # windows(width=16, height=8)
    # multiplot(P5, P3, P4, cols=3)
}


