# Lectura de archivos de aceleracion ----

#Una vez que hicimos el experimento de movimiento del accelerometro lo volvemos a conectar y copiamos los archivos que se encuentran en la carpeta "GCDC". Si el aparato estuvo encendido mucho tiempo encontrara varios archivos

# Primero abro los primeros y ultimos archivos y borro cualquier linea que sea distinta, por ej la ultima linea del ultimo archivo cuando el aparato se apaga al consumir toda la energia dice "stopping logging: shutdown: low battery"

# List into a df ----
rm(list=ls())
setwd("C:/Users/Javi/Desktop/campo_2023/analisis datos/C31A/GCDC")
#setwd("D:/Desktop_CENPAT/brasil18/campo_2023/analsis datos/prueba")

options("digits.secs" = 9)

library(chron)

# Reading csv files into a list
directory <- "C:/Users/Javi/Desktop/campo_2023/analisis datos/C31A/GCDC"
  files <- list.files(directory, full.names=TRUE)
files

#saco la tercer linea con datos de fecha y hora y la guardo en el objeto tmp0
tmp0 <- lapply(files, read.csv, colClasses= rep("character",3), skip=2, nrows =1, header=F) 
head(tmp0[[1]])

#genero una funcion que da formato posix
library(chron)
fn0 <- function(x) 
  as.POSIXct(strptime(paste(x[2], x[3]),'%Y-%m-%d %H:%M:%OS')) 

# corro la funcion a los datos de fecha y hora
tmp0 <- lapply(tmp0, fn0 )
head(tmp0[[1]])

# leo los csv y pego en una list
tmp1 <- lapply(files, read.csv, colClasses=c("numeric", "integer", "integer", "integer"), skip=7)
head(tmp1[[1]])

tmp1 <- lapply(tmp1, function(x) {names(x)[1] <- "Time"; x}) #renombro columna Time
head(tmp1[[1]])

fn1 <- function(i)  {
  tmp1[[i]]$Time <-  tmp0[[i]]+ tmp1[[i]]$Time
  tmp1[[i]]
}
#genero una funcion para corregir la hora, porque el tiempo es guardado como segundos despues que inicio el aparato

tmp1 <- lapply(1:length(tmp1), fn1) #corro la funcion fn1
head(tmp1[[1]])


# En caso de varias horas de grabaci?n ser?n varias archivos que tendremos que juntarlos en nuestro caso no haria falta porque es un solo archivo csv

tmp3 <- do.call(rbind, tmp1)#pego todos los csv

dim(tmp3)

#Transformo los valores a g
#Las medidas estan a 16 bits (2^16= 65536) el aparato cubre el rango 
#de +/-16g por lo tanto cada conteo discreto equivale a  32/65536=2048 conteos/g.

correccion <- 2048
tmp3$Ax <-tmp3$Ax/correccion
tmp3$Ay <-tmp3$Ay/correccion
tmp3$Az <-tmp3$Az/correccion
head(tmp3)
str(tmp3)
summary(tmp3)
# OUtput como .RData
save(tmp3, file="C31A_14_3_23.RData")

