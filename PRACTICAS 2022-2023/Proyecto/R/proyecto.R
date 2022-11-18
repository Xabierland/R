setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Proyecto/R/")

#### TABLA DE FRECUENCIAS ######################################################
#X
datos_x<-read.table(file="datos_x.txt",header=T)
attach(datos_x)
frame_x<-as.data.frame(table(datos_x));values_x<-frame_x$edadPueblos;freq_abs_x<-frame_x$Freq;freq_abs_acum_x<-cumsum(frame_x$Freq);freq_rel_x<-freq_abs_x/length(edadPueblos);freq_rel_acum_x<-cumsum(freq_rel_x);data.frame(values_x,freq_abs_x,freq_abs_acum_x,freq_rel_x,freq_rel_acum_x)
#Y
datos_y<-read.table(file="datos_y.txt",header=T)
attach(datos_y)
frame_y<-as.data.frame(table(datos_y));values_y<-frame_y$edadCiudad;freq_abs_y<-frame_y$Freq;freq_abs_acum_y<-cumsum(frame_y$Freq);freq_rel_y<-freq_abs_y/length(edadCiudad);freq_rel_acum_y<-cumsum(freq_rel_y);data.frame(values_y,freq_abs_y,freq_abs_acum_y,freq_rel_y,freq_rel_acum_y)
#### GRAFICOS ##################################################################
##BARPLOT
#X
barplot(table(datos_x),xlab="Edad")
#Y
barplot(table(datos_y),xlab="Edad")
##BOXPLOT
#X
boxplot(edadPueblos)
#Y
boxplot(edadCiudad)
##CURVE
#X
curve(dnorm(x,mean(edadPueblos),sqrt(var(edadPueblos)*(length(edadPueblos)-1)/length(edadPueblos))),0,30,ylab="f(x)",xlab="Edad en los pueblos")
#Y
curve(dnorm(x,mean(edadCiudad),sqrt(var(edadCiudad)*(length(edadCiudad)-1)/length(edadCiudad))),0,30,ylab="f(y)",xlab="Edad en las ciudades")
#### ESTADISTICOS DESCRIPTIVOS #################################################
###TENDENCIA
##MEDIA
#X
mean(edadPueblos)
#Y
mean(edadCiudad)
#MEDIANA
#X
median(edadPueblos)
#Y
median(edadCiudad)
#MODA
#X
names(table(edadPueblos))[which(table(edadPueblos)==max(table(edadPueblos)))]
#Y
names(table(edadCiudad))[which(table(edadCiudad)==max(table(edadCiudad)))]
###DISPESION
##RANGO
#X
max(edadPueblos)-min(edadPueblos)
#Y
max(edadCiudad)-min(edadCiudad)
##Rango intercuartilico
#X
IQR(edadPueblos)
#Y
IQR(edadCiudad)
##Varianza
#X
var(edadPueblos)*(length(edadPueblos)-1)/length(edadPueblos)
#Y
var(edadCiudad)*(length(edadCiudad)-1)/length(edadCiudad)
##Desviacion tipica
#X
sqrt(var(edadPueblos)*(length(edadPueblos)-1)/length(edadPueblos))
#Y
sqrt(var(edadCiudad)*(length(edadCiudad)-1)/length(edadCiudad))
##Coeficionete de variacion
#X
sqrt(var(edadPueblos)*(length(edadPueblos)-1)/length(edadPueblos))/mean(edadPueblos)
#Y
sqrt(var(edadCiudad)*(length(edadCiudad)-1)/length(edadCiudad))/mean(edadCiudad)
###POSICION
##Cuartiles
#X
quantile(edadPueblos)
#Y
quantile(edadCiudad)
##Deciles
#X
quantile(edadPueblos,probs=seq(0,1,0.1))
#Y
quantile(edadCiudad,probs=seq(0,1,0.1))
##Percentiles
#X
quantile(edadPueblos,probs=seq(0,1,0.01))
#Y
quantile(edadCiudad,probs=seq(0,1,0.01))


#### ESTIMACIONES DE PARAMETROS POBLACIONALES ##################################
var.test(edadPueblos,edadCiudad)$conf
#No pasa por el 1 luego las varianzas son distintas
t.test(edadPueblos,edadCiudad,var.equal = F)$conf
