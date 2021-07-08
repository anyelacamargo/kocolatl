#' Anyela V Camargo R anyelavcamargo@gmail.com

library(ggplot2)
library(dplyr)
library(tidyr)
#source('../aquacropr/R/Aqua_library.R')



transform_weather <- function(fname_in, fname_out){
  
  
  
  #fname_in <- 'weather SIMPLE/Original POWER_SinglePoint_Daily_20180101_.csv'
  weather_raw <- read.csv(fname_in, skip = 20, header = TRUE)
  
 
  DATE <- paste(paste(strsplit(as.character(weather_raw$YEAR), split ='')[[1]][3:4], 
                      collapse = ''), sprintf("%03d", weather_raw$DOY), sep = '')
  
  weather_filter <- data.frame(cbind(DATE, sRAD = weather_raw[['ALLSKY_SFC_SW_DWN']],
                                     TMAX = weather_raw[['T2M_MAX']],
                                     TMIN = weather_raw[['T2M_MIN']],
                                     RAIN = weather_raw[['PRECTOT']]))
  
  write.table(weather_filter, file = fname_out, sep=' ',
              quote = FALSE, row.names = FALSE)
  
  
  
  
}


plot_data <- function(sdata){
  
  
  
  
  #yy$Date <- droplevels(yy$Date)
  # pdf('climate_plots.pdf', width=20)
  # plot(y$Date, y$flowering, col='black', lwd=6, cex.axis=0.8, xaxt = "n", 
  #      ylim = c(0, 110))
  # lines(y$Date, y$rain,  col='red')
  # lines(y$Date, y$RH,  col='blue')
  # lines(y$Date, y$SRAD,  col='purple')
  # lines(y$Date, y$WS2M*150,  col='brown')
  # axis(1, at = seq(1,nrow(y), 15), labels= s$Date[seq(1, nrow(y), by=15)],
  #      las=2, cex.axis=0.8)
  # legend(1, 75, legend=c("Peso.cosechado.seco..kg.","rain","RH",
  #                        "SRAD","WS2M"),
  #        col=c("black","green", "red", "blue", "purple","brown"), lty=1:2, cex=0.8,
  #        lwd=3)
  # dev.off()
  
}


fname_in <- 'weather SIMPLE/Original POWER_SinglePoint_Daily_20180101_.csv'
fname_out <- 'weather SIMPLE/dummy_weather.WTH'

transform_weather(fname_in, fname_out)




climate_caldas <- read.csv('climate_caldas.csv', header=TRUE)
climate_caldas <- climate_caldas[, 2:ncol(climate_caldas)]

climate_caldas <- data.frame(climate_caldas, 
                             year=sapply(climate_caldas$Date, 
                                         function(x) 
                                           strsplit(as.character(x), 
                                                    '\\/')[[1]][3]))
climate_caldas$year <- as.numeric(as.character(climate_caldas$year))
climate_caldas <- filter(climate_caldas, year == '2019' | year == '2020')
climate_caldas$Date <- droplevels(climate_caldas$Date)
climate_caldas$Date <- factor(climate_caldas$Date, levels = climate_caldas$Date)

cacao_data <- read.csv('Copy of Cacao_datos_fedecacao.csv', header = TRUE)

cacao_data <- data.frame(cacao_data, flowering = cacao_data$Peso.cosechado.seco..kg.)

y <- merge(climate_caldas, cacao_data, by.x='Date', by.y = 'Fecha.Inicio.de.cosecha',
           all.x = TRUE)

s <- aggregate(y, by = list(y$month, y$year), FUN=mean, na.rm=TRUE)
s <- mutate(s, Date = paste(month, as.numeric(year), sep='_'))
s$Date  <- as.factor(s$Date)
s$Date <- factor(s$Date, levels = s$Date)


yf <- merge(climate_caldas, cacao_data, by.x='Date', by.y = 'Fecha.de.Floracion',
           all.x = TRUE)
#l <- which(yf$flowering != 100)
#yf$flowering[-l] = 1
sf <- aggregate(yf, by = list(yf$month, yf$year), FUN=mean, na.rm=TRUE)
sf <- mutate(sf, Date = paste(month, year, sep='_'))
sf$Date  <- as.factor(sf$Date)
sf$Date <- factor(sf$Date, levels = sf$Date)

#' Search fertilization days
#' 

i <- c(which(s$month == 4), which(s$month == 9))

pdf('plots_average.pdf')
plot(s$Date, s$Peso.cosechado.seco..kg., col='black', lwd=2, cex.axis=0.8, 
     ylim = c(0, 110), las=2, pch=0)
points(sf$Date, sf$flowering,  col='green', lwd=6)
lines(s$Date, s$rain,  col='red')
lines(s$Date, s$RH,  col='blue')
lines(s$Date, s$WS2M*150,  col='brown')
abline(v=c(i), col='purple', lwd=1, lty=2)
legend(0, 75, legend=c("Peso.cosechado.seco..kg.",'flowering',"rain","RH","WS2M", 
                       'fert'),
       col=c("black", 'green', "red", "blue", "brown", 'purple'), lty=1:2, cex=0.8,
       lwd=3)


dev.off()