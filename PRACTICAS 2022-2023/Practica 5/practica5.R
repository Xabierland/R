setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Practica 5/")

#### EJEMPLOS DE ESTIMACION

###1. En una planta de potabilización se han medido durante 10 días los niveles de cloro en las
###aguas. Los resultados obtenidos son: 2.2, 1.9, 1.7, 1.6, 1.7, 1.8, 1.7, 1.9, 2.0, 2.0. La
###distribución del nivel de cloro se considera normal.
##a) X = “Nivel de cloro”. Calcula la estimación puntual de  y  del nivel de cloro.
nivelcloro <- c(2.2,1.9,1.7,1.6,1.7,1.8,1.7,1.9,2.0,2.0);nivelcloro
x <- mean(nivelcloro);x #Estimación puntual de la media
var(nivelcloro) #Estimación puntual de la varianza
S <- sd(nivelcloro);S #Estimación puntual de la desviación típica
##b) El intervalo de confianza de la media del nivel de cloro, al nivel de confianza del 95%.
t.test(nivelcloro)$conf
##c) El intervalo de confianza de la media del nivel de cloro, al nivel de confianza del 99%.
t.test(nivelcloro,conf.level = 0.99)$conf
##d) El intervalo de confianza de la varianza del nivel de cloro con un nivel de confianza del
##95%
ICsigma95<-c(9*var(nivelcloro)/qchisq(0.975,9),9*var(nivelcloro)/qchisq(0.025,9));ICsigma95
##e) El intervalo de confianza de la varianza con un nivel de confianza del %99
ICsigma99<-c(9*var(nivelcloro)/qchisq(0.995,9),9*var(nivelcloro)/qchisq(0.005,9));ICsigma99

###2. Realizar la estimación puntual y estimar mediante un intervalo de confianza a un 95% de
###nivel de confianza, la proporción de piezas defectuosas de un lote que sigue una
###distribución binomial. Para ello se escogen al azar 150 piezas, observándose que 12 de
###ellas son defectuosas.
#X: “Número de piezas defectuosas”
12/150 #estimación puntual de p
prop.test(12,150)$conf #R emplea la distribución c2 (en nuestro caso emplearemos la distribución nomal)
p<-0.08;q<-0.92
P95 <-c(p-qnorm(0.025,0,1,lower.tail=F)*sqrt(p*q/150), p+qnorm(0.025,0,1,lower.tail=F)*sqrt(p*q/150));P95

###3. Se quiere saber si existen diferencias significativas en la facturación de dos tiendas de
###joyería de una misma cadena. Para ello se eligieron al azar 11 días en los que se
###contabilizaron las ventas en la joyería A y otros 10 días en la joyería B. Los datos se
###encuentran en el archivo Ventas.txt. X: “Ventas de la joyería A” , Y: “Ventas de la joyería B”
##a) Estimación puntual de la media y la varianza de las ventas en las joyerías A y B.
datos <- read.table("Ventas.txt",header=T) #Lectura de datos
attach(datos);VentasA;VentasB
mean(VentasA)
mean(VentasB,na.rm=T) #Para no tener en cuenta los datos perdidos (NA)
var(VentasA)
var(VentasB,na.rm=T) #Para no tener en cuenta los datos perdidos (NA)
##b)El intervalo de confianza de la división de las varianzas de las ventas de las joyerías con
##un nivel de confianza del 95%
var.test(VentasA,VentasB)$conf
##c)El intervalo de confianza de la diferencia de medias de las ventas de las joyerías con un
##nivel de confianza del 95%
t.test(VentasA,VentasB,var.equal=F)$conf

###4. Genere una muestra aleatoria de tamaño 100 de una variable aleatoria N(10,2).
valores <- rnorm(100,10,2)
##a) Calcule el intervalo de confianza de la media con un nivel de confianza del 95%.
t.test(valores)$conf
##b) ¿Cuántos valores se quedan fuera del intervalo?
N1<-length(which(valores<9.786608));N1
N2<-length(which(valores>10.508501));N2
N1+N2

#### EJERCICIO ESTIMACION
###1. Con el fin de comparar las ganancias de dos joyerías ubicadas en Bilbao, se resumen en la
###siguiente tabla las ganancias de las dos joyerías en varios días elegidos al azar. 
A <- c(1320,1495,990,1250,12900,1900,1500,1100,1250,1100,1930)
B <- c(1110,1405,985,1290,1300,1705,1200,1105,1150,1210,NA)
mean(A)
mean(B,na.rm=T) #Para no tener en cuenta los datos perdidos (NA)
var(A)
var(B,na.rm=T) #Para no tener en cuenta los datos perdidos (NA)
##a)Calcule el intervalo de confianza de la media de las ganancias de la joyería A con un nivel
##de confianza del 95%.
t.test(A)$conf
##b)Calcule el intervalo de confianza de la media de las ganancias de la joyería A con un nivel
##de confianza del 99%. 
t.test(A,conf.level = 0.99)$conf
##c) Calcule el intervalo de confianza para la diferencia de medias de las ganancias de las
##joyerías A y B con un nivel de confianza del 99%. Suponga que las varianzas son
##distintas.
t.test(A,B,conf.level = 0.99,var.equal = F)$conf
##d) Calcule el intervalo de confianza para la diferencia de medias de las ganancias de las
##joyerías A y B con un nivel de confianza del 95%. Suponga que las varianzas son
##iguales
t.test(A,B,var.equal = T)$conf
##e) Calcule el intervalo de confianza de la varianza de las ganancias de la joyería A con un
##nivel de confianza del 95%
n<-length(A);n
ICsigma95<-c((n-1)*var(A)/qchisq(0.975,(n-1)),(n-1)*var(A)/qchisq(0.025,(n-1)));ICsigma95
##f) Calcule el intervalo de confianza para la división de las varianzas de las ganancias de
##las joyerías A y B con un nivel de confianza del 95%
var.test(A,B)$conf
##g) Observando el resultado del apartado anterior, ¿qué conclusión obtiene respecto a las
##varianzas de las ganancias de ambas joyerías?
#Son distintas ya que no incluye al 1.
