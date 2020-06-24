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

#Utilizamos las librerias
library(readxl)
library(lubridate)
library(rjson)
library(tidyverse)
library(VIM)
library(partykit)
library(rpart)
library(caret)

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

#No se observan datos nulos

## COERSION

# Obtenemos las variable fechaparte para convertirlo a fecha
# Eliminamos todos los puntos dentro de cada variable
bomberos$fechaparte = gsub("[.]", "\\1", bomberos$fechaparte)
#convertimos a fecha con zona horaria de peru
bomberos$fechaparte <- dmy_hms(bomberos$fechaparte, tz = "America/Lima")
#Verificamos la clase de tipo fecha - "POSIXct" "POSIXt"
class(bomberos$fechaparte)

#Convertimos a factor
bomberos$codigotipoemergencia <- as.factor(bomberos$codigotipoemergencia)

#Convertimos a factor
bomberos$tipoemergencia <- as.factor(bomberos$tipoemergencia)

#Convertimos a factor
bomberos$tipoemergenciacompleto <- as.factor(bomberos$tipoemergenciacompleto)

#Convertimos a factor  
bomberos$distrito <- as.factor(bomberos$distrito)

#Convertimos a factor
bomberos$ubigeo <- as.factor(bomberos$ubigeo)

#Convertimos a factor
bomberos$codigoestado <- as.factor(bomberos$codigoestado)

#Convertimos a factor
bomberos$codigomodulo <- as.factor(bomberos$codigomodulo)

#Convertimos a factor
bomberos$estadoregistro <- as.factor(bomberos$estadoregistro)

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

bomberos$vehiculos <- as.array(bomberos$vehiculos)

bomberos=bomberos[1:11]

summary(bomberos)

#####
#data= 70%
#train=30%
#####
#tranversal, longitudinal <= se diferencia a traves del tiempo

bomberos1 <- bomberos 
particion = createDataPartition(y = bomberos1$tipoemergencia, p = 0.7, list = FALSE, times = 1)
train = bomberos1[particion,]
test = bomberos1[-particion,]
dim(train)
dim(test)

## DECISION TREE MODEL

modelo1 = rpart(Mora~.,data=train, method = "class", minsplit=0, cp=0.0019)









