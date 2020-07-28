#Instalamos la libreria para leer archivos excel
install.packages("Readxl")
install.packages("xlsx")
install.packages("lubridate")
install.packages("rjson")
install.packages("tidyverse")
install.packages("VIM")
install.packages("partykit")
install.packages("rpart")
install.packages("caret")
install.packages("corrplot")
install.packages("cluster")
install.packages("PerformanceAnalytics")
install.packages("psych")
install.packages("factoextra")

#Utilizamos las librerias
library(readxl)
library(lubridate)
library(rjson)
library(tidyverse)
library(VIM)
library(partykit)
library(rpart)
library(caret)
library(corrplot)
library(cluster)
library(PerformanceAnalytics)
library(psych)
library(factoextra)

# Entendimiento de la data - 24 Datos
# numeroparte: Identificador de reporte
# fechaparte: Fecha de incidente
# codigotipoemergencia: Tipo de emergencia por codigo
# tipoemergencia: Categoricas (ACCIDENTE VEHICULAR, DESASTRE NATURALES, EMERGENCIA MEDICA, INCENDIO, MATERIALES PELIGROSOS, RESCATE, SERVICIO ESPECIAL)
# tipoemergenciacompleto: Descripcion de la data
# numerodireccion: Referencia de accidente
# direccion: Direccion de accidente
# distrito: Distrito del accidente
# ubigeo: Ubigeo del accidente
# codigomodulo: Categoricas (PCMOD01 - PCMOD07)
# informante: Quien dio el reporte (BORRAR)
# codigogradomagnitud: Dato Nulo (BORRAR)
# codigousuario: Usuario que vio el pedido (BORRAR)
# referenciaemergencia: Referencia del acidente
# descripcionemergencia: Reporte de la emergencia
# observacion: Observaciones de la emergencia
# apoyoinstitucion: Apoyo (BORRAR)
# codigobomberomando: Bombero a cargo de la emergencia (BORRAR)
# codigoestado: Estado categorica (90100, 90200, 90300 , ..., 91200)
# estadoregistro: Estado de atencion - Categorica ( A, C) (BORRAR)
# color	: Rango de colores (BORRAR)
# latitud: Latitud de Ubicacion de accidente
# longitud: Longitud de Ubicacion de accidente
# vehiculos: Movilidad que atendió la emergencia

#Leemos la data
bomberos <- read_excel("bomberos.xlsx")
View(bomberos)
head(bomberos)
str(bomberos)
dim(bomberos)
summary(bomberos)

## Viendo cantidad de datos Nulos
nulos=aggr(bomberos)
summary(nulos)

## ANALISIS DESCRIPTIVO
## PATRONES DE COMPORTAMIENTO
## -> SI SE DA, SE PUEDE HACER CLUSTERING
## LIBRERIA 'MAPS' CONVERTIR COORDENADAS A LUGAR
## UTILIZAR KMEANS (GENERAR GRUPOS)
## CONSTRUIR TARGET -> A PARTIR DEL TIPO DE EMERGENCIA
## CONSTRUIR MODELOS PARA CADA EMERGENCIA (9)

#No se observan datos nulos

## COERSION

# Obtenemos las variable fechaparte para convertirlo a fecha
# Eliminamos todos los puntos dentro de cada variable
bomberos$fechaparte = gsub("[.]", "\\1", bomberos$fechaparte)
#convertimos a fecha con zona horaria de peru
bomberos$fechaparte <- dmy_hms(bomberos$fechaparte, tz = "America/Lima")
#Verificamos la clase de tipo fecha - "POSIXct" "POSIXt"
class(bomberos$fechaparte)
summary(bomberos$fechaparte)

## TODOS LOS REGISTROS X DIA
bomberos %>%
  ggplot(aes(fechaparte)) +
  geom_freqpoly(binwidth = 86400) # 86400 segundos = 1 día

## TODOS LOS REGISTROS X SEMANA
bomberos %>%
  ggplot(aes(fechaparte)) +
  geom_freqpoly(binwidth = 604800) # 604800 segundos = 7 días

## TODOS LOS REGISTROS POR DIA DE LA SEMANA
bomberos %>%
  mutate(dia_semana = wday(fechaparte, label = TRUE)) %>%
  ggplot(aes(x = dia_semana)) +
  geom_bar()

#Convertimos a factor
bomberos$codigotipoemergencia <- as.factor(bomberos$codigotipoemergencia)
summary(bomberos$codigotipoemergencia)
str(bomberos$codigotipoemergencia)
par(mfrow=c(2,1))
boxplot(bomberos$codigotipoemergencia, horizontal=TRUE, main = "Códigos por tipo de emergencia")
hist(bomberos$codigotipoemergencia, main = "")

#Convertimos a factor
bomberos$tipoemergencia <- as.factor(bomberos$tipoemergencia)
summary(bomberos$tipoemergencia)
str(bomberos$tipoemergencia)

tipoemergencia_bars<- ggplot(bomberos, aes(x="", fill=tipoemergencia))+
  geom_bar(width = 1, stat = "identity")
tipoemergencia_bars

tipoemergencia_circular <- bp + coord_polar("y", start=0)
tipoemergencia_circular + scale_fill_brewer("Tipos de emergencia") + 
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = tipoemergencia/7 + c(0, cumsum(tipoemergencia)[-length(tipoemergencia)]), 
                label = percent(tipoemergencia/100)), size=5)

#Convertimos a factor
bomberos$tipoemergenciacompleto <- as.factor(bomberos$tipoemergenciacompleto)
summary(bomberos$tipoemergenciacompleto)
str(bomberos$tipoemergenciacompleto)

#Convertimos a factor  
bomberos$distrito <- as.factor(bomberos$distrito)
summary(bomberos$distrito)
str(bomberos$distrito)

#Convertimos a factor
bomberos$ubigeo <- as.factor(bomberos$ubigeo)
summary(bomberos$ubigeo)
str(bomberos$ubigeo)

#Convertimos a factor
bomberos$codigoestado <- as.factor(bomberos$codigoestado)
summary(bomberos$codigoestado)
str(bomberos$codigoestado)

#Convertimos a factor
bomberos$codigomodulo <- as.factor(bomberos$codigomodulo)
summary(bomberos$codigomodulo)
str(bomberos$codigomodulo)

#Convertimos a factor
bomberos$estadoregistro <- as.factor(bomberos$estadoregistro)
summary(bomberos$estadoregistro)
str(bomberos$estadoregistro)

#Convertimos a números de coordenadas aceptados añadiendo un punto "." a todos los números despues de 2 dígitos
bomberos$longitud <- str_c(substr(bomberos$longitud, 1, stop = 3), substr(bomberos$longitud, 4, stop = nchar(bomberos$longitud)), sep = ".")
#convertimos a tipo numerico
bomberos$longitud <- as.numeric(bomberos$longitud)
bomberos$longitud
summary(bomberos$longitud)

#Convertimos a números de coordenadas aceptados añadiendo un punto "." a todos los números despues de 2 dígitos
bomberos$latitud <- str_c(substr(bomberos$latitud, 1, stop = 3), substr(bomberos$latitud, 4, stop = nchar(bomberos$latitud)), sep = ".")
#Convertimos a tipo numerico
bomberos$latitud = as.numeric(bomberos$latitud)
bomberos$latitud
summary(bomberos$latitud)

# Convertimos a array
bomberos$vehiculos <- as.array(bomberos$vehiculos)
summary(bomberos$vehiculos)
str(bomberos$vehiculos)

## Separamos la variable 'vehiculos'
bomberos=bomberos[1:11]

summary(bomberos)

#####
#data= 70%
#train=30%
#####
#tranversal, longitudinal <= se diferencia a traves del tiempo

bomberos1 <- bomberos[,1:8]
summary(bomberos1)
dim(bomberos1)

## PRIMER DIA
bomberos %>%
  filter(fechaparte < ymd(20190408)) %>%
  ggplot(aes(fechaparte)) +
  geom_freqpoly(binwidth = 3600) # 600 segundos = 10 minutos



## plot data
plot(bomberos)
summary(bomberos)

library(cluster)
wss=numeric()
for (k in 2:10){
  set.seed(111)
  agrupamiento=kmeans(bomberos$codigotipoemergencia, k)
  wss[k-1]=agrupamiento$tot.withinss
}

plot(2:10, wss,type="b")

wss2=numeric()
for (k in 2:10){
  set.seed(111)
  agrupamiento=kmeans(bomberos$ubigeo, k)
  wss2[k-1]=agrupamiento$tot.withinss
}

plot(2:10, wss2,type="b")

fviz_nbclust(bomberos, FUNcluster=kmeans, method="silhouette")+theme_classic()





