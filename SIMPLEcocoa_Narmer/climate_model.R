### Script to transform weather data from NASA POWER to WTH files
### for SIMPLE model
#' Anyela V Camargo R anyelavcamargo@gmail.com
#' Update by Angela Romero V ---2021-08-11
library(ggplot2)
library(dplyr)
library(tidyr)

# copia el directotrio de la carpeta que contiene este codigo
# el la dirección usar / y no \
my_path <- "C:/Users/Angelita/Documents/R/Cacao"
setwd(my_path)


transform_weather <- function(fname_in, fname_out){
  
  
  fname_in <- 'POWER_SinglePoint_DailySantander.csv'
  weather_raw <- read.csv(fname_in, skip = 17, header = TRUE)
 
  #dat <- data.frame(sprintf('%02d', weather_raw$YEAR  %% 100))                
  
  
  DATE <- paste(paste(sprintf('%02d', weather_raw$YEAR  %% 100), 
                      sprintf("%03d", weather_raw$DOY), sep = ''))
  
  weather_filter <- data.frame(cbind(DATE, SRAD = weather_raw[['ALLSKY_SFC_SW_DWN']],
                                     TMAX = weather_raw[['T2M_MAX']],
                                     TMIN = weather_raw[['T2M_MIN']],
                                     RAIN = weather_raw[['PRECTOT']]))
  
 
  
 write.table(weather_filter, file = fname_out, sep='  ',
              quote = FALSE, row.names = FALSE)
  
}

fname_in <- 'weather_filter'
fname_out <- 'KOKOS_weather.WTH'
transform_weather(fname_in, fname_out)
#-------------------------
  
  
  
 
