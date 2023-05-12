# 9/5/18 Clase práctica analisis de dato de aceleracion para monitorear actividad en animales salvajes

#Generación y selección de variables para clasificación comportamientos########

#Quito cualquier mugre por ahi
rm(list=ls())
# Le digo el directorio de trabajo
setwd("C:/Users/Javi/Desktop/campo_2023/analisis datos/C31A") 

#amplio la memoria utilizada por R
memory.limit(size=6000)

#agrego mas decimales
options("digits.secs" = 9)

#install.packages(chron)
library (chron)

#Cargo  el archivo limpio y ordenado

load("C31A_14_3_23.RData")

class(tmp3)

head(tmp3)

str(tmp3)


#Genero las variables derivadas

#Calculo de OBDA, VeBA, Pitch, Roll, jerk y VarRoll
FR <- 25

tamventana <- FR * 2  # defino el tamaño de mi ventana para la running mean, varia entre sp y tamaños de organismos, en este caso dos segundos


#install.packages("caTools")
library ("caTools")
#Calculo la aceleración estatica (g)
tmp3$rmAx <-runmean(tmp3$Ax,tamventana)
tmp3$rmAy <-runmean(tmp3$Ay,tamventana)
tmp3$rmAz <-runmean(tmp3$Az,tamventana)

# Le resto al dinamic el estatic en absolutos
tmp3$diAx <- abs(tmp3$Ax-tmp3$rmAx)
tmp3$diAy <- abs(tmp3$Ay-tmp3$rmAy)
tmp3$diAz <- abs(tmp3$Az-tmp3$rmAz)


#Calculo el ODBA
tmp3$odba <- tmp3$diAx + tmp3$diAy + tmp3$diAz
summary(tmp3$odba)

#Calculo el VeDBA
tmp3$vedba <- ((tmp3$diAx)^2 + (tmp3$diAy)^2 + (tmp3$diAz)^2)^(1/2)

#Tengo que quitar los NA
tmp3<-tmp3[!is.na(tmp3$Az),]
tmp3<-tmp3[!is.na(tmp3$Ax),]

#miro para mis aparatos cual es cada eje
# en los ham de carlos es
# sway (lado-lado) = Z negativo  hacia  bateria
# heave (espalda-panza) roll = Y negativo hacia esplada del pez
# surge (cabeza-cola) pitch= X positivo hacia puerto usb

# Calculo de pitch roll 1 y roll 2
tmp3$pitch <- (asin(tmp3$rmAx))*180/pi
tmp3$roll <- (asin(tmp3$rmAz))*180/pi

#quito los rolidos NA
tmp3<-tmp3[!is.na(tmp3$roll),]

windows()
par(mfrow = c(2,1))
hist(tmp3$pitch)
hist(tmp3$roll)

#### Calculo la varianza del surge, heave, sway, roll ####
#install.packages("TTR")
library(xts)
library(TTR)
RM <- 2 * FR
tmp3$variAx <- runVar(tmp3$Ax, n=RM)
tmp3$variAy <- runVar(tmp3$Ay, n=RM)
tmp3$variAz <- runVar(tmp3$Az, n=RM)
tmp3$varroll <- runVar(tmp3$roll, n=RM)

#HAY QUE DESPLAZAR CADA VALOR 24 DATOS MAS ATRAS ya que esta funcion produce un corrimiento del efecto que queremos ver.
tmp3$variAx<-tmp3$variAx[((RM/2)+1):(length(tmp3$variAx)+(RM/2))]
tmp3$variAy<-tmp3$variAy[((RM/2)+1):(length(tmp3$variAy)+(RM/2))]
tmp3$variAz<-tmp3$variAz[((RM/2)+1):(length(tmp3$variAz)+(RM/2))]
tmp3$varroll<-tmp3$varroll[((RM/2)+1):(length(tmp3$varroll)+(RM/2))]


#Me quedo con las variables que me interesan
final <- tmp3[,c(1:4,8:18)]
head(final)
tail(final)
summary(final)
str(final)

windows ()
plot(final$varroll)

save(final, file="C31A_14_3_23_clean")




