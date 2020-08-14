options(max.print=999999)
options("width"=200)

library(ggplot2)
library(plyr)
library(cowplot)
library(car)
library(ggpubr)
library(lattice)
library(multcomp)
library(effects)
library(psych)
library(ResourceSelection)
library(MuMIn)
library("cowplot")
library(plyr)
library(lme4)


#Cargar bases peces y fondo limpias
setwd("C:/Users/Tatiana Araya/Documents/Análisis/analisisculebra")
datarawfish<-read.csv("20-01-24_Tati_Gustavo_BASE DATOS PECES_Bahía Culebra (hasta junio19).csv")
environment <- read.csv("20-01-24_Tati_Gustavo_Carolina Salas_BASE DATOS FONDO_Bahía Culebra (hasta julio2019).csv")

head(datarawfish)
head(environment)
str(datarawfish)
str(environment)
colnames(datarawfish)
colnames(environment)

# Eliminar columnas poco importantes y datos de los 90's
data <- datarawfish[,-c(1,2,3,5,7,8,10,15,16,19:23)]
data <- data[!data$Año < 2013,]

# Cambiar los NA's en las abundancias por ceros
data[is.na(data)] <- 0

# Arreglar los formatos
data$site <- as.factor(data$Sitio)
data$diver <- as.factor(data$Buzo)
data$year <- as.factor(data$Año)
data$transect <- as.factor(data$Transecto..codSitio.Fecha.)
data$t.area <- as.numeric(data$Área.transecto..m.)
data$n <- as.numeric(data$Total)
data$date <- data$Fecha
data$date <- as.POSIXlt(strptime(as.character(data$date), "%d/%m/%Y"))
data$date <- as.POSIXct(data$date, origin = "1960/01/01", tz = "UTC")
data$date2 <- as.Date(data$date, format = "%Y/%m/%d")
data$month <- months.POSIXt(data$date)

# Cálculo de biomasa desde cero para eliminar posibles errores al sumar
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
                 "t.area", "Profundidad","Categoría.profundidad",
                 "Código.sp.","Especie","Total","a","b","GF..Quimbayo.Roff.","IUCN","biomass",
                 "biomass_g.m2","biomass_kg.ha","biomass_tn.ha",
                 "density","2.5","7.5","12.5", "17.5", "25", "35","45","55","65","75","85","95",
                 "112.5","137.5","162.5","187.5","225","275","325","700")

#datafish <- reshape2:::melt.data.frame(data, c("year","month","date2","site","diver", "transect",
 #                                              "t.area", "Profundidad","Categoría.profundidad",
  #                                             "Código.sp.","Especie","Total","a","b",
   #                                            "GF..Quimbayo.Roff.","IUCN","biomass",
    #                                           "biomass_g.m2","biomass_kg.ha","biomass_tn.ha",
     #                                          "density"), 
      #                                 c(22:length(data)),
       #                                variable.name="size", value.name="abd")
#head(datafish)
#datafish <- datafish[datafish$abd > 0,]  

# Arreglar frmatos de base de fondo
str(environment)
environment$year<-as.factor(environment$year)
environment$transect<-as.factor(environment$transect)
environment$Crustose_algae <- as.numeric(environment$Crustose_algae)
environment$Sand <- as.numeric(environment$Sand)
environment$Basalts <- as.numeric(environment$Basalts)
environment$Rubble <- as.numeric(environment$Rubble)
environment$C._sertularioides <- as.numeric(environment$C._sertularioides)
environment$CCA <- as.numeric(environment$CCA)
environment$coral <- as.numeric(environment$coral)
environment$Bleached_coral <- as.numeric(environment$Bleached_coral)
environment$Macroalgae <- as.numeric(environment$Macroalgae)
environment$Other <- as.numeric(environment$Other)
environment$Rhodoliths <- as.numeric(environment$Rhodoliths)
environment$Turf <- as.numeric(environment$Turf)
environment$date <- as.POSIXlt(strptime(as.character(environment$date), "%m/%d/%Y"))
environment$date <- as.POSIXct(environment$date, origin = "1960/01/01", tz = "UTC")
environment$date2 <- as.Date(environment$date, format = "%Y/%m/%d")
head(environment)
str(environment)

# Seleccionar las columnas mas necesarias y hacer calculos basicos de n, biomasa y densidad sobre ellas
data1<- ddply(data, .(year, site,date, month, diver, 
                      transect, GF..Quimbayo.Roff.), summarize,
              n = sum(Total),
              biomass_tn.ha = sum(biomass_tn.ha),
              density = sum(density))

levels(data1$site)
levels(environment$site)

levels(data1$site) <- c("Bajo Argentina", "Bajo Mero", "Cabeza de mono", "Cabeza de Mono", "Cacique", "Esmeralda",
                        "Güiri-Güiri", "Jicaral", "Jícaro", "Matapalo", "Palmitas", "Pelonas", "Playa Blanca",
                        "Playa Viradores", "Punta Cacique", "Güiri-Güiri", "Güiri-Güiri")

colnames(data1)
colnames(environment)

# Unir bases depuradas de peces y fondo
db<-merge(data1,environment,by=c("year","site","date","transect"))
db <- as.data.frame(lapply(db, function(x) if(is.factor(x)) factor(x) else x))
db <- db[!( db$GF..Quimbayo.Roff. == "#N/A"), ]

#library(gtools)
#db$month.number <- as.numeric(factor(db$year, levels=unique(mixedsort(db$year))))


str(db)
head(db)


#write.csv(db, "basemergedversion2.csv", row.names = F)


library(xlsx)
#write.xlsx(data, "base peces horizontal hasta julio2019.xlsx")
#write.xlsx(environment, "base fondo hasta julio2019.xlsx")
#write.xlsx(db, "base peces+fondo hasta julio2019.xlsx")


## Gráfico de biomasa promedio para cada grupo trófico por año
db$year2 <- as.numeric(db$year)

plot.all.fish <- ggplot(data = db) + 
  geom_smooth(se=T, mapping = aes(x = year2, y = biomass_tn.ha, color = GF..Quimbayo.Roff.),method="loess")+
  scale_color_manual(values=c('limegreen','navy','yellow','red','black',
                              'orange'))+
  scale_x_continuous(limit=c(1,6),breaks=c(1,2,3,4,5,6),
                     labels=c("2014", "2015", "2016", "2017", "2018", "2019"))+
  labs(x = "Month (2014-2019)", y = "Fish biomass (tn ha-1)")+
  theme_bw()+
  theme_classic()+
  theme(axis.line.x = element_line(color="black", size = .9),
        axis.line.y = element_line(color="black", size = .9)) +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90),
        axis.text.y=element_text(colour="black",size=12),
        axis.title=element_text(size=12)) +
  theme(legend.text = element_text(size=12, color="black"))+
  guides(color=guide_legend("Trophic group"))+
  scale_y_continuous(limit=c(-0.1,0.5))


plot.all.fish


# Para análsis de Porcentajes promedio de cobertura de cada sustrato para los diferentes años
library(reshape2)
colnames(environment)

# acomodar las coberturas para poder hacer el gráfico
fondo <- reshape2:::melt.data.frame(environment, c("year","site","month","date","diver.bottom",
                                            "transect"), 
                                    c(7:length(environment)),
                                    variable.name="substrate", value.name="coverage")

env<- ddply(fondo, .(year,substrate), summarize,
              coverage = mean(coverage))

####
env<-env[env$substrate == "Coral" | env$substrate == "CCA" | env$substrate == "C._sertularioides" | 
             env$substrate == "Macroalgae" |env$substrate == "Turf" | 
             env$substrate == "Rubble" | env$substrate == "Sand", ]

env <- as.data.frame(lapply(env, function(x) if(is.factor(x)) factor(x) else x))
str(env)

env$substrate <- factor(env$substrate, 
                          levels = c("C._sertularioides","Turf","Sand","Macroalgae",
                                     "Coral","Rubble","CCA"))
env$substrate <- as.factor(env$substrate)
levels(env$substrate)

# gráfico cobertura por año por tipo de sustrato
env$year2 <- as.numeric(env$year)

plot.env <- ggplot(data = env) + 
  geom_smooth(se=T, mapping = aes(x = year2, y = coverage, color = substrate),size=1.2, method="loess")+
  scale_color_manual(values=c('limegreen','seagreen','navy','darkcyan','yellow',
                              'black','blue'))+
  scale_x_continuous(limit=c(1,6),breaks=c(1,2,3,4,5,6),
                     labels=c("2014", "2015", "2016", "2017", "2018", "2019"))+
  labs(x = "Month (2014-2019)", y = "Coverage (%)")+
  theme_bw()+
  theme_classic()+
  theme(axis.line.x = element_line(color="black", size = .9),
        axis.line.y = element_line(color="black", size = .9)) +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90),
        axis.text.y=element_text(colour="black",size=12),
        axis.title=element_text(size=12)) +
  theme(legend.title = element_text(size=12))+
  scale_y_continuous(limit=c(-0.1,100))+
  theme(legend.text = element_text(size=12, color="black"))+
  guides(color=guide_legend("Substrate"))+
  theme(axis.title.x = element_text(vjust = -1))
plot.env

## Poner ambos gráficos en una página y exportarlos como PNG
library(cowplot)

dev.off()

#png(paste(Sys.Date(),"_","fishes substrate year.png", sep=""), 
 #   width = 10, height = 8, units = "in", res = 300)

#cowplot::plot_grid(plot.all.fish, plot.env, scale = 0.95,
 #                  labels = c("Fishes", "Environment"), ncol = 1, nrow = 2)

dev.off()



########## Modelos para herbívoros

herb <- db[db$GF..Quimbayo.Roff. == "Macroalgae feeders",]


##########    Homocedasticidad  ###########

par(mfcol = c(2, 2))
par(mar = c(2, 2, 2, 2), oma=c(3, 3, 0, 0)) 
plot(biomass_tn.ha ~ year, data = herb)
mtext("año", 1, line = 1.9) 
plot(biomass_tn.ha ~ Turf, data = herb)
mtext("turf", 1, line = 1.9)
plot(biomass_tn.ha ~ Macroalgae, data = herb)
mtext("macros", 1, line = 1.9)
plot(biomass_tn.ha ~ CCA, data = herb)
mtext("CCA", 1, line = 1.9)
mtext("biomass tn/ha", 2, outer = T, line = 1)


par(mfcol = c(2, 2))
par(mar = c(2, 2, 2, 2), oma=c(3, 3, 0, 0)) 
plot(log(biomass_tn.ha)~ year, data = herb)
mtext("año", 1, line = 1.9) 
plot(log(biomass_tn.ha) ~ Turf, data = herb)
mtext("turf", 1, line = 1.9)
plot(log(biomass_tn.ha) ~ Macroalgae, data = herb)
mtext("macros", 1, line = 1.9)
plot(log(biomass_tn.ha) ~ CCA, data = herb)
mtext("CCA", 1, line = 1.9)
mtext("log(biomass_tn.ha)", 2, outer = T, line = 1)

bartlett.test(biomass_tn.ha~year, herb)
bartlett.test(log(biomass_tn.ha)~year, herb)

###########        Normalidad         #############
shapiro.test(herb$biomass_tn.ha)
shapiro.test(log(herb$biomass_tn.ha))


#### Crear un modelo inicial
modgaus<- glm(log(biomass_tn.ha) ~ year + Turf + Macroalgae + CCA + C._sertularioides, 
              family = "gaussian", herb)
anova(modgaus, test= "Chisq")
summary(modgaus)

qqnorm(residuals(modgaus), col = "gray", pch = 20, cex = 3)
qqline(residuals(modgaus), col = "red2", lwd = 2.85)

# Hosmer and Lemeshow goodness of fit >0.05
hl <- hoslem.test(modgaus$y, fitted(modgaus), g = 10)
hl

# Prueba de deviance >0.05
1 - pchisq(modgaus$deviance, modgaus$df.resid)

crPlots(modgaus, ask = F)

options(na.action = "na.fail")
dredge(modgaus, rank="AICc") 

m0<- glm(log(biomass_tn.ha) ~ 1, 
         family = "gaussian", herb)
m1<- glm(log(biomass_tn.ha) ~ Macroalgae + Turf + CCA,
         family = "gaussian", herb)
m2<- glm(log(biomass_tn.ha) ~ Turf,
         family = "gaussian", herb)
m3<- glm(log(biomass_tn.ha) ~ C._sertularioides + Turf,
         family = "gaussian", herb)
m4<- glm(log(biomass_tn.ha) ~ C._sertularioides + Turf + C._sertularioides:Turf, 
         family = "gaussian", herb)
m5<- glm(log(biomass_tn.ha) ~ Turf + CCA, 
         family = "gaussian", herb)
m6<- glm(log(biomass_tn.ha) ~ Turf + year, 
        family = "gaussian", herb)
m7<- glm(log(biomass_tn.ha) ~ Turf + Macroalgae, 
        family = "gaussian", herb)
m8<- glm(log(biomass_tn.ha) ~ Turf + Macroalgae+ year, 
        family = "gaussian", herb)
m9<- glm(log(biomass_tn.ha) ~ Turf + C._sertularioides + year, 
        family = "gaussian", herb)

AIC<- AIC(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9)
AIC

anova(m2, m6, test = "Chi")

mejormodelo<- glm(log(biomass_tn.ha) ~ Turf,
                  family = "gaussian", herb)

anova(mejormodelo, test = "Chi")
summary(mejormodelo)

### Que tan fuerte es la relacion R^2
1 - (mejormodelo$dev/mejormodelo$null)

resid.dev <- 425.43
resid.DF <- 121 

# Si esta proporción es cercana a 1, no hay sobre-dispersión
resid.dev / resid.DF

####  Normalida de los residuales
qqnorm(residuals(mejormodelo), col = "gray", pch = 20, cex = 3)
qqline(residuals(mejormodelo), col = "red2", lwd = 2.85)

######################### Gráfico
#dev.off()
#################################################################################

library("MuMIn")
options(na.action = "na.fail")
mf.dredge.full<-dredge(mejormodelo)
mf.dredge.full

newdata <- expand.grid(Turf=rep(seq(from=min(herb$Turf), to=max(herb$Turf), by=1)))

newdata2 <- cbind(newdata, predict(mejormodelo, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))

fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Turf, fit, LL, UL)
names(newdata3) <- c("Turf","fit", "LL", "UL")
head(newdata3)

newdata3$count<-exp(newdata3$fit)
newdata3$LL2<-exp(newdata3$LL)
newdata3$UL2<-exp(newdata3$UL)

head(newdata3)

png(paste(Sys.Date(),"_","herbbiomassmodel.png", sep=""), 
   width = 10, height = 8, units = "in", res = 300)

mf.plot <- ggplot(newdata3, aes(Turf, count)) +
  geom_ribbon(aes(ymin = LL2, ymax = UL2), alpha = .40) +
  geom_line( aes(),size = 1) +
  ggtitle("") +
  labs(x=expression(Turf~coverage~" "~"(%)"),
       y=expression(Macroalgae~feeders~biomass~t~ha^{-1})) +
  theme_classic() +
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = .9),
        axis.line.y = element_line(color="black", size = .9)) +
  theme(axis.text.x=element_text(colour="black",size=12),
        axis.text.y=element_text(colour="black",size=12),
        axis.title=element_text(size=14)) +
  theme(text = element_text(size=12))+
  geom_text(x=80, y=1.3, label="2014-2019", size=5)

mf.plot

dev.off()




###################### Modelos para piscivoros

pis <- db[db$GF..Quimbayo.Roff. == "Piscivores",]


##########    Homocedasticidad  ###########

par(mfcol = c(2, 2))
par(mar = c(2, 2, 2, 2), oma=c(3, 3, 0, 0)) 
plot(biomass_tn.ha ~ year, data = pis)
mtext("año", 1, line = 1.9) 
plot(biomass_tn.ha ~ Turf, data = pis)
mtext("turf", 1, line = 1.9)
plot(biomass_tn.ha ~ Macroalgae, data = pis)
mtext("macros", 1, line = 1.9)
plot(biomass_tn.ha ~ CCA, data = pis)
mtext("CCA", 1, line = 1.9)
mtext("biomass tn/ha", 2, outer = T, line = 1)


par(mfcol = c(2, 2))
par(mar = c(2, 2, 2, 2), oma=c(3, 3, 0, 0)) 
plot(log(biomass_tn.ha)~ year, data = pis)
mtext("año", 1, line = 1.9) 
plot(log(biomass_tn.ha) ~ Turf, data = pis)
mtext("turf", 1, line = 1.9)
plot(log(biomass_tn.ha) ~ Macroalgae, data = pis)
mtext("macros", 1, line = 1.9)
plot(log(biomass_tn.ha) ~ CCA, data = pis)
mtext("CCA", 1, line = 1.9)
mtext("log(biomass_tn.ha)", 2, outer = T, line = 1)

bartlett.test(biomass_tn.ha~year, pis)
bartlett.test(log(biomass_tn.ha)~year, pis)

###########        Normalidad         #############
shapiro.test(pis$biomass_tn.ha)
shapiro.test(log(pis$biomass_tn.ha))


#### Crear un modelo inicial
modgaus<- glm(log(biomass_tn.ha) ~ year + Turf + Macroalgae + CCA + C._sertularioides, 
              family = "gaussian", pis)
anova(modgaus, test= "Chisq")
summary(modgaus)

qqnorm(residuals(modgaus), col = "gray", pch = 20, cex = 3)
qqline(residuals(modgaus), col = "red2", lwd = 2.85)

# Hosmer and Lemeshow goodness of fit >0.05
hl <- hoslem.test(modgaus$y, fitted(modgaus), g = 10)
hl

# Prueba de deviance >0.05
1 - pchisq(modgaus$deviance, modgaus$df.resid)

crPlots(modgaus, ask = F)

options(na.action = "na.fail")
dredge(modgaus, rank="AICc") 

m0<- glm(log(biomass_tn.ha) ~ 1, 
         family = "gaussian", pis)
m1<- glm(log(biomass_tn.ha) ~ year + Macroalgae + Turf + CCA + C._sertularioides,
         family = "gaussian", pis)
m2<- glm(log(biomass_tn.ha) ~ Turf + CCA + year,
         family = "gaussian", pis)
m3<- glm(log(biomass_tn.ha) ~ Turf + CCA + year + C._sertularioides,
         family = "gaussian", pis)
m4<- glm(log(biomass_tn.ha) ~ C._sertularioides + CCA + year, 
         family = "gaussian", pis)
m5<- glm(log(biomass_tn.ha) ~ Turf + CCA + Macroalgae + year, 
         family = "gaussian", pis)
m6<- glm(log(biomass_tn.ha) ~ year + Macroalgae + CCA + C._sertularioides, 
         family = "gaussian", pis)
m7<- glm(log(biomass_tn.ha) ~ year + CCA, 
         family = "gaussian", pis)
m8<- glm(log(biomass_tn.ha) ~ Turf + C._sertularioides + year, 
         family = "gaussian", pis)

AIC<- AIC(m0,m1,m2,m3,m4,m5,m6,m7,m8)
AIC

anova(m2, m3, test = "Chi")

mejormodelo<- glm(log(biomass_tn.ha) ~ Turf + CCA + year,
                  family = "gaussian", pis)

anova(mejormodelo, test = "Chi")
summary(mejormodelo)

### Que tan fuerte es la relacion R^2
1 - (mejormodelo$dev/mejormodelo$null)

resid.dev <- 908.38
resid.DF <- 193

# Si esta proporción es cercana a 1, no hay sobre-dispersión
resid.dev / resid.DF

####  Normalida de los residuales
qqnorm(residuals(mejormodelo), col = "gray", pch = 20, cex = 3)
qqline(residuals(mejormodelo), col = "red2", lwd = 2.85)


# Gráficos
####### Gráfico Turf
modturf<- glm(log(biomass_tn.ha) ~ Turf,
                  family = "gaussian", pis)
my.data <- data.frame(Turf = seq(min(pis$Turf), max(pis$Turf))
                                 )

# predicción del modelo sobre nuestro set de datos
G <- predict(modturf, newdata = my.data, type = "link", se = T)

# Extrar valores predecidos por el modelo
fit <- exp(G$fit)

# Calcular intervalos de confianza
upperCI <- exp(G$fit + 1.96 * G$se.fit)
lowerCI <- exp(G$fit - 1.96 * G$se.fit)

# Gráfico turf
dev.off()
plot(biomass_tn.ha~ Turf, data = pis, pch = 16, col = "darkgray",
     xlab = "Turf",
     ylab = "biomasa tn/ha", axes = F)

axis(1)
axis(2, las = 1)

lines(my.data$Turf, fit, lty = 1, lwd = 2, col = "red")
lines(my.data$Turf, upperCI, lty = 2)
lines(my.data$Turf, lowerCI, lty = 2)

box(bty="l")

####### Gráfico CCA
modCCA<- glm(log(biomass_tn.ha) ~ CCA,
              family = "gaussian", pis)
my.data <- data.frame(CCA = seq(min(pis$CCA), max(pis$CCA))
                      )

# predicción del modelo sobre nuestro set de datos
G <- predict(modCCA, newdata = my.data, type = "link", se = T)

# Extrar valores predecidos por el modelo
fit <- exp(G$fit)

# Calcular intervalos de confianza
upperCI <- exp(G$fit + 1.96 * G$se.fit)
lowerCI <- exp(G$fit - 1.96 * G$se.fit)

# Gráfico
plot(biomass_tn.ha~ CCA, data = pis,
     xlab = "CCA",
     ylab = "biomasa tn/ha", axes = F)

axis(1)
axis(2, las = 1)

lines(my.data$CCA, fit, lty = 1, lwd = 2, col = "red")
lines(my.data$CCA, upperCI, lty = 2)
lines(my.data$CCA, lowerCI, lty = 2)

box(bty="l")

############
####### Gráfico year
modyear<- glm(log(biomass_tn.ha) ~ year,
             family = "gaussian", pis)
my.data <- data.frame(year = levels(pis$year))

# predicción del modelo sobre nuestro set de datos
G <- predict(modyear, newdata = my.data, type = "link", se = T)

# Extrar valores predecidos por el modelo
fit <- exp(G$fit)

# Calcular intervalos de confianza
upperCI <- exp(G$fit + 1.96 * G$se.fit)
lowerCI <- exp(G$fit - 1.96 * G$se.fit)

# Gráfico
plot.year.pis <- plot(biomass_tn.ha~ year, data = pis, pch = 16, col = "gray",
     xlab = "year",
     ylab = "biomasa tn/ha", axes = T) +

lines(my.data$year, fit, lty = 1, lwd = 2, col = "red") +
lines(my.data$year, upperCI, lty = 2, lwd = 2) +
lines(my.data$year, lowerCI, lty = 2, lwd = 2) +

box(bty="l")

#dev.off()


##########################  Graficos de TAVO #################################################
options(na.action = "na.fail")
mf.dredge.full<-dredge(modturf)
mf.dredge.full

newdata <- expand.grid(Turf=rep(seq(from=min(pis$Turf), to=max(pis$Turf), by=1)))

newdata2 <- cbind(newdata, predict(modturf, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))

fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Turf, fit, LL, UL)
names(newdata3) <- c("Turf","fit", "LL", "UL")
head(newdata3)

newdata3$count<-exp(newdata3$fit)
newdata3$LL2<-exp(newdata3$LL)
newdata3$UL2<-exp(newdata3$UL)

head(newdata3)

pis.turf.plot <- ggplot(newdata3, aes(Turf, count)) +
  geom_ribbon(aes(ymin = LL2, ymax = UL2), alpha = .40) +
  geom_line( aes(),size = 1) +
  ggtitle("") +
  labs(x=expression(Turf~coverage~" "~"(%)"),
       y=expression(Piscivores~biomass~t~ha^{-1})) +
  theme_classic() +
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = .9),
        axis.line.y = element_line(color="black", size = .9)) +
  theme(axis.text.x=element_text(colour="black",size=12),
        axis.text.y=element_text(colour="black",size=12),
        axis.title=element_text(size=14)) +
  theme(text = element_text(size=12))+
  geom_text(x=80, y=1.3, label="2014-2019", size=5)

pis.turf.plot

##############
options(na.action = "na.fail")
mf.dredge.full<-dredge(modCCA)
mf.dredge.full

newdata <- expand.grid(CCA=rep(seq(from=min(pis$CCA), to=max(pis$CCA), by=1)))

newdata2 <- cbind(newdata, predict(modCCA, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))

fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$CCA, fit, LL, UL)
names(newdata3) <- c("CCA","fit", "LL", "UL")
head(newdata3)

newdata3$count<-exp(newdata3$fit)
newdata3$LL2<-exp(newdata3$LL)
newdata3$UL2<-exp(newdata3$UL)

head(newdata3)

pis.CCA.plot <- ggplot(newdata3, aes(CCA, count)) +
  geom_ribbon(aes(ymin = LL2, ymax = UL2), alpha = .40) +
  geom_line( aes(),size = 1) +
  ggtitle("") +
  labs(x=expression(CCA~coverage~" "~"(%)"),
       y=expression(Piscivores~biomass~t~ha^{-1})) +
  theme_classic() +
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = .9),
        axis.line.y = element_line(color="black", size = .9)) +
  theme(axis.text.x=element_text(colour="black",size=12),
        axis.text.y=element_text(colour="black",size=12),
        axis.title=element_text(size=14)) +
  theme(text = element_text(size=12))+
  geom_text(x=80, y=1.3, label="2014-2019", size=5)

pis.CCA.plot


###############################

pis.year.plot <- ggplot(pis, aes(year, biomass_tn.ha, colour = year)) + geom_boxplot()


dev.off()

png(paste(Sys.Date(),"_","pis.model.png", sep=""), 
    width = 15, height = 5, units = "in", res = 300)

cowplot::plot_grid(pis.CCA.plot,pis.turf.plot,pis.year.plot, scale = 0.95,
                   labels = c("CCA", "Turf", "Año"), ncol = 2, nrow = 2)

dev.off()
