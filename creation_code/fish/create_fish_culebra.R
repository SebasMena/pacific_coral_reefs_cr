##
##
## Objetivo: Modificar el formato de los datos de Bahia Culebra, para que sean aptos para incluirse en la base de datos de Costa Rica

## Método: 1. se llaman las bases de datos y se estandariza el formato 
##         1.1 Se cargan los datos de uvc
##         1.2 se cambian los datos de formato ancho a largo
##         1.3 se seleccionan las columnas de interes
##         1.4 se quitan las filas con NA en abundancia
##         1.5 se determina la clase correcta para las columnas
##         1.6 se define la region
##         1.7 se corrigen los nombres de los sitios para que coincidan en formato con los datos regionales y con su base de sitios
##         2. se unen las bases de datos
##         3. Se guarda el archivo en formato .rda
##
## Fecha:2020-08-15 
##
## Creado por: Tatiana Araya-Arce
## Modificado por: Andrea Arriaga-Madrigal
## 
## 


# Cargar bases peces y fondo limpia

raw_locale <- "data_raw/fish/"
datarawfish <-read.csv(paste0(raw_locale,"Culebra/20-01-24_Tati_Gustavo_BASE DATOS PECES_Bahia Culebra (hasta junio19).csv"))

setwd("C:/Users/Tatiana Araya/Documents/GitHub/pacific_coral_reefs_cr/data_raw/sessile/Culebra")
environment <- read.csv("20-01-24_Tati_Gustavo_Carolina Salas_BASE DATOS FONDO_Bah?a Culebra (hasta julio2019).csv")

head(datarawfish)
head(environment)
str(datarawfish)
str(environment)
colnames(datarawfish)
colnames(environment)

# Eliminar columnas poco importantes y datos de los 90's
data <- datarawfish[,-c(1,2,3,5,7,8,10,15,16,19:23)]
data <- data[!data$A?o < 2013,]

# Cambiar los NA's en las abundancias por ceros
data[is.na(data)] <- 0

# Arreglar los formatos
data$site <- as.factor(data$Sitio)
data$diver <- as.factor(data$Buzo)
data$year <- as.factor(data$A?o)
data$transect <- as.factor(data$Transecto..codSitio.Fecha.)
data$t.area <- as.numeric(data$?rea.transecto..m.)
data$n <- as.numeric(data$Total)
data$date <- data$Fecha
data$date <- as.POSIXlt(strptime(as.character(data$date), "%d/%m/%Y"))
data$date <- as.POSIXct(data$date, origin = "1960/01/01", tz = "UTC")
data$date2 <- as.Date(data$date, format = "%Y/%m/%d")
data$month <- months.POSIXt(data$date)

# C?lculo de biomasa desde cero para eliminar posibles errores al sumar
data$biomass <- ((data$a*2.5^data$b)*data$X2.5)+((data$a*7.5^data$b)*data$X7.5)+
  ((data$a*12.5^data$b)*data$X12.5)+((data$a*17.5^data$b)*data$X17.5)+((data$a*25^data$b)*data$X25)+
  ((data$a*35^data$b)*data$X35)+((data$a*45^data$b)*data$X45)+((data$a*55^data$b)*data$X55)+
  ((data$a*65^data$b)*data$X65)+((data$a*75^data$b)*data$X75)+((data$a*85^data$b)*data$X85)+
  ((data$a*95^data$b)*data$X95)+((data$a*112.5^data$b)*data$X112.5)+((data$a*137.5^data$b)*data$X137.5)+
  ((data$a*162.5^data$b)*data$X162.5)+((data$a*187.5^data$b)*data$X187.5)+((data$a*225^data$b)*data$X225)+
  ((data$a*275^data$b)*data$X275)+((data$a*325^data$b)*data$X325)+((data$a*700^data$b)*data$X700)

data$biomass_g.m2 <- data$biomass/data$t.area
data$biomass_kg.ha <- data$biomass_g.m2 * 10
data$biomass_tn.ha <- data$biomass_kg.ha / 1000
data$density <- data$Total / data$t.area

str(data)
colnames(data)

# Base procesada con informacion necesaria
data <- data[, -c(1:7,35:42)]

colnames(data)

data <- data[,c(32,38,37,30,31,33,34,1,2,3,4,5,26,27,28,29,39:43,6:25)]
names(data) <- c("year","month","date","site","diver", "transect",
                 "t.area", "Profundidad","Categor?a.profundidad",
                 "C?digo.sp.","Especie","Total","a","b","GF..Quimbayo.Roff.","IUCN","biomass",
                 "biomass_g.m2","biomass_kg.ha","biomass_tn.ha",
                 "density","2.5","7.5","12.5", "17.5", "25", "35","45","55","65","75","85","95",
                 "112.5","137.5","162.5","187.5","225","275","325","700")
library(reshape2)
datafish <- reshape2:::melt.data.frame(data, c("year","month","date","site","diver", "transect",
                                               "t.area", "Profundidad","Categor?a.profundidad",
                                               "C?digo.sp.","Especie","Total","a","b",
                                               "GF..Quimbayo.Roff.","IUCN","biomass",
                                               "biomass_g.m2","biomass_kg.ha","biomass_tn.ha",
                                               "density"), 
                                       c(22:length(data)),
                                       variable.name="size", value.name="abd")
head(datafish)
datafish <- datafish[datafish$abd > 0,]  
