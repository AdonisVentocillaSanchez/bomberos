
bomb <- bomberos

bomberos$fechaparte -> as.Date(as.character(bomberos$fechaparte, format= "%d%m%Y "))

ex1 = -770060789632872

nchar(ex1)
substr(ex1, 1, stop = 3)
substr(ex1, 4, stop = nchar(ex1))
str_c(substr(ex1, 1, stop = 3), substr(ex1, 4, stop = nchar(ex1)), sep = ".")

longitud = str_c(substr(bomberos$longitud, 1, stop = 3), substr(bomberos$longitud, 4, stop = nchar(bomberos$longitud)), sep = ".")


if(nchar(bomberos$longitud)>0) {
  longitud = str_c(substr(bomberos$longitud, 1, stop = 3), substr(bomberos$longitud, 4, stop = nchar(bomberos$longitud)), sep = ".")
}


class(bomb$latitud)
nchar(bomb$latitud[10])
substr(bomb$longitud[10], 1, stop = 3)

bomb$latitud <- as.numeric(bomb$longitud)

class(bomb$longitud)


long1 = str_c(substr(bomb$longitud, 1, stop = 3), substr(bomb$longitud, 4, stop = nchar(bomb$longitud)), sep = ".")



long1<-as.numeric(long1)

long1
long1 <- as.integer(long1)

longitud = as.integer(longitud)

dt2 <- gsub("[.]", "\\1", bomb$fechaparte)
dt2 <- dmy_hms(dt2, tz = "America/Lima")
dt5 <- hms(dt2, tz = "America/Lima")
dt3 <- as.Date.character(dt2)
dt4 <- as.POSIXct(dt2)

hora <- make_datetime(hour = hour(dt2), min = minute(dt2), sec = second(dt2))


day(dt4)

vehiculos = paste(bomb$vehiculos, collapse = " ")
vehiculos = fromJSON(vehiculos)



str(dt4)


wday(as.Date(dt4), label= TRUE)
plot(wday(as.Date(dt4), label= TRUE), day(dt4))