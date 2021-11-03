
#######  KOKOlatm Lanitoamenrican cocoa to predict the optimal harvest time 
# Updated by Angela Romero Vergel,Specialist Crop modeller 
# NIAB,Cocoa Latam project  ------  2021-08-11 
##################################################################
# para correr debe seleccionar todo y dar click en RUN
# Regiones disponibles:Santander, Apartado, Arauca, Cali o Caldas
# si el cultivo esta en otra region diferente a las 5 anteriores:
# escribir "otro"
region <- "Caldas" 
#enter here date of flowering year-month-day
fecha_floracion <- as.Date("2020-06-23") 

source("SimpleB_cocoa.R")