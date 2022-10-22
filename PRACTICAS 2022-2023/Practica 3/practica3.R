setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Practica 3/")

####EJEMPLOS
##a)
datos <- c(115,232,181,161,155,137,165,171,139,130,406)
boxplot(datos)
boxplot(datos, horizontal=T , col="gold", type=2)
##b)
boxplot.stats(datos)
##c)
datos <- c(1,1,1,2,3,3,1,2,2,1,3,1,1)
library(e1071)
skewness(datos)
kurtosis(datos)

####EJERCICIOS
###1
datos1 <- c(0, 2, 3, 2, 4, 1, 2, 3, 3, 0, 2, 6, 2, 1, 2, 3, 1, 2, 3, 1, 2, 7, 2, 1, 4, 2, 3, 3, 1, 0)
##a) Construya la tabla de frecuencias

##b) Represente el diagrama de barras y el diagrama de frecuencias acumuladas.
barplot(table(datos1))
pie(table(datos1))
##c) Calcule las medidas de tendencia central: moda, mediana y media aritmética
#MODA
moda<-names(table(datos1))[which(table(datos1)==max(table(datos1)))];moda
#MEDIANA
mean(datos1)
#MEDIA
median(datos1)
##d) Calcule las medidas de dispersión: rango, rango intercuartílico, varianza, desviación típica, coeficiente de variación.
#RANGO
rango<-max(datos1)-min(datos1);rango
#IQR
IQR(datos1, type=2)
#VARIANZA
varianza<-var(datos1)*(length(datos1)-1)/length(datos1);varianza
#Cuasivarianza
cvar<-var(datos1)
#DESVIACION TIPICA
des.tip<-sqrt(varianza);des.tip
#Cuasivarianza desviacion tipica
cdes.tip<-sqrt(cvar)
#CV
CV<-des.tip/mean(datos1);CV
##e) Calcule las medidas de posición: Q2, D1, D4, D9, P30 y P85. Explique su significado.
#Q2
quantile(datos1, 0.5, type=2)
#D1
quantile(datos1, 0.1, type=2)
#D4
quantile(datos1, 0.3, type=2)
#D9
quantile(datos1, 0.9, type=2)
#P30
quantile(datos1, 0.3, type=2)
#P85
quantile(datos1, 0.85, type=2)
##f) Calcule las medidas de forma: coeficientes de asimetría y curtosis.
skewness(datos1)
kurtosis(datos1)
##g) Represente el diagrama de cajas y bigotes y compruebe la existencia de datos atípicos.
boxplot(datos1, horizontal=T , col="gold", type=2)

###4
datos2 <- c(2.1, 3.3, 4.4, 3.0, 4.0, 5.0, 2.7, 2.6, 4.8, 4.7, 2.8, 4.8, 3.9, 2.3, 3.8, 2.8, 3.0, 3.7, 3.3, 4.4, 3.1, 4.0, 3.7, 2.5, 2.7, 5.1, 4.7)
##a) Construya la tabla de frecuencias y represente el histograma.

##b) Calcule la media, la mediana, la moda, la desviación típica y el coeficiente de variación.
median(datos2)
mean(datos2)
moda<-names(table(datos1))[which(table(datos1)==max(table(datos1)))];moda
##c) Calcule los cuartiles Q1 y Q3, el rango y el rango intercuartílico.
#Q1
quantile(datos1, 0.25, type=2)
#Q3
quantile(datos1, 0.75, type=2)
#RANGO
rango<-max(datos2)-min(datos2);rango
#RANGO INTERCUARTILICO
IQR(datos1, type=2)
##d) Calcule el coeficiente de asimetría de Pearson y la curtosis.
skewness(datos2)
kurtosis(datos2)
