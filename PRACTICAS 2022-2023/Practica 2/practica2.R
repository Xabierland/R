setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Practica 2/")
#===============================================================================EJEMPLOS
#====
#EJEMPLO 1
grietas<-c(50,68,84,86,64,67,78,87,110,85,52,65,52,93,72,70,105,85,30,42,74,30,70,65,49)#Introducir datos
limites<-c(30,40,50,60,70,75,85,90,110,Inf)#Límite de cada intervalo
grietas.limite<-cut(grietas,limites,right=F)#Como los intervalos han de ser abiertos por la derecha: right=F
grietas.limite
table(grietas.limite)
a<-as.data.frame(table(grietas.limite))#Hemos convertido la tabla en un marco de datos. Puede usarse solo "data.frame (...)"
a
Intervalos<-a$grietas.limite #Lo único que se ha hecho es crear un vector a partir de la primera columna del marco de datos con el nombre de "Intervalos".
Frecuencia.abs<-a$Freq #Lo único que se ha hecho es crear un vector a partir de la segunda columna del marco de datos con el nombre de "Frecuencia.abs".
sum(Frecuencia.abs)#Suma de todos los datos
Frecuencia.rel<-Frecuencia.abs/25#Cálculo de la frecuencia relativa (recordad que "25" es el número de datos de este ejercicio)
Frecuencia.abs.acum<-cumsum(Frecuencia.abs)#Suma acumulada de las frecuencias absolutas
Frecuencia.rel.acum<-cumsum(Frecuencia.rel)#Suma acumulada de las frecuencias relativas
data.frame(Intervalos,Frecuencia.abs,Frecuencia.abs.acum,Frecuencia.rel,Frecuencia.rel.acum)
#EJEMPLO 2
datos<-c(1,1,1,2,3,3,1,2,2,1,3,1,1)
a <- table(datos) 
a
barplot(a)
pie(a) 
hist(datos)
#EJEMPLO 3
stem(grietas, scale=2)
#EJEMPLO 4
longitudes<- c(115,232,181,161,155,137,165,171,139,130,406)
hist(longitudes)
hist(longitudes,main="Histograma longitudes", xlab="Longitudes (cm)",ylab="Densidad", breaks=c(100,150,200,450), col="ORANGE")
#EJEMPLO 5
datos <- c(1,1,1,2,3,3,1,2,2,1,3,1,1)
summary(datos) # Resumen de los principales estadísticos del vector "datos" 
mean(datos)#Media
median(datos)#Mediana
table(datos)
Moda<-names(table(datos))[which(table(datos)==max(table(datos)))]; Moda
Rango<-max(datos)-min(datos);Rango
IQR(datos, type=2)
quantile(datos,0.75,type=2)-quantile(datos,0.25,type=2)
var(datos)
N <- length(datos)
varianza <- var(datos)* (N-1)/N; varianza
des.tip <- sqrt(varianza); des.tip
CV<-des.tip/mean(datos); CV
quantile(datos,type=2)
quantile(datos,probs=seq(0,1,0.1),type=2)
quantile(datos,probs=seq(0,1,0.01),type=2)
quantile(datos,0.4,type=2)
#====
#===============================================================================EJERCICIO1
#====
#EJERCICIO 1
#a)
tornillos<-c(1,2,3,3,2,1,2,5,2,4,4,4,5,3,2,5,3,4,1,4,2,3,1,1,2,5,3,4,1,3)
table(tornillos)
a<-data.frame(table(tornillos));a
#b)
barplot(table(tornillos))
#c)
pie(table(tornillos), labels = c("muy fino", "fino", "mediano", "grueso", "muy grueso"))
#====
#===============================================================================EJERCICIO2
#====
#EJERCICIO 2
#a)
tension_rotura <- read.table(file="Tensión_rotura.txt",header=TRUE,dec=",")
#b)
tension_rotura
datos2<-tension_rotura$Tension_rotura.Tn.cm2.
#c)
stem(datos2, scale = 1)
#d)
#seq(min(datos2),max(datos2),0.2)
hist(datos2, breaks = 10)
#e)
mean(datos2)
median(datos2)
moda<-names(table(datos2))[which(table(datos2)==max(table(datos2)))];moda
#f)
rango<-max(datos2)-min(datos2);rango
IQR(datos2, type=2)
varianza<-var(datos2)*(length(datos2)-1)/length(datos2);varianza
des.tip<-sqrt(varianza);des.tip
CV<-des.tip/mean(datos2);CV
#g)
quantile(datos2, type=2)
quantile(datos2, probs=seq(0,1,0.1), type=2)
quantile(datos2, probs=seq(0,1,0.01), type=2)
#====
#===============================================================================EJERCICIO3
#====
#EJERCICIO 3
#a)
torsion<-c(33,21,32,44,35,22,40,36,22,37,20,37,42,31,23,44,32,30,44,44,42,35,40,36,32,31,36,43,24,40,25,30,26,35,33,41,25,44,36,27);torsion
limites3<-c(20,24,28,32,36,40,44)
torsion.limite<-cut(torsion, limites3, right = F); torsion.limite
table(torsion.limite)
#b)
hist(torsion)
fre_acum<-data.frame(table(torsion.limite))
hist(fre_acum$Freq)
#c)
mean(torsion)
moda<-names(table(torsion))[which(table(torsion)==max(table(torsion)))];moda
median(torsion)
varianza<-var(torsion)*(length(torsion)-1)/length(torsion);varianza
des.tip<-sqrt(varianza);des.tip