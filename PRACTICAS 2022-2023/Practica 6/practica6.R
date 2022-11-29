setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Practica 6/")

####EJEMPLO DE CONTRASTE DE HIPOTESIS
###1. En una planta de potabilizacion se han medido durante 10 dias los niveles de cloro en las aguas.
###Los resultados obtnidos son: 2.2, 1.9, 1.7, 1.6, 1.7, 1.8, 1.7, 1.9, 2.0, 2.0. La distribucion se considera normal.
##a)Con un nivel de significación del 5%, contrastar si se puede aceptar la hipótesis de que
##provienen de una población normal de media 1.9. (X: “Nivel de cloro”)
nivelcloro <- c(2.2,1.9,1.7,1.6,1.7,1.8,1.7,1.9,2.0,2.0)
t.test(nivelcloro, mu=1.9)

##b) Con un nivel de significación del 5%, contrastar si se puede aceptar la hipótesis de que la
##media de la población es mayor que 1.9. (X: “Nivel de cloro”)
t.test(nivelcloro, mu=1.9, alternative="greater")

##c) Con un nivel de significación del 5%, contrastar si se puede aceptar la hipótesis de que la
##media de la población es menor que 1.9. (X: “Nivel de cloro”)
t.test(nivelcloro, mu=1.9, alternative="less")

##d) Con un nivel de significación del 1%, contrastar si se puede aceptar la hipótesis de que
##provienen de una población normal de media 1.9. (X: “Nivel de cloro”)
t.test(nivelcloro, mu=1.9, conf.level = 0.99)

###3. En una encuesta sobre el TAV han tomado parte 110 personas de las cuales 48 están a
###favor del mismo. Con un nivel de significación del 5%, ¿se puede aceptar la hipótesis de
###que el 50% de la población está a favor del TAV?
prop.test(48,110,p=0.5)

####EJERCICIOS
###Se quiere saber si existen diferencias significativas en la facturación de dos tiendas de joyería de
###una misma cadena. Para ello se eligieron al azar 11 días en los que se contabilizaron las ventas
###en la joyería A y otros 10 días en la joyería B. Los datos obtenidos se encuentran en el archivo
###“Ventas.txt”.
##a) Con un nivel de significación del 5%, ¿se puede afirmar que las desviaciones típicas de
##las ventas son iguales?
ventas <- read.table("Ventas.txt",header=T) #Lectura de datos
attach(ventas)

var.test(VentasA, VentasB) #COMPROBAMOS SI SON IGUALES

t.test(VentasA, VentasB, var.equal = F)
##b) Con un nivel de significación del 1%, ¿se puede afirmar que las medias de las ventas son
##iguales? 
t.test(VentasA, VentasB, conf.level = 0.99, var.equal = F)

#p<0.05 -> hipotesis alternativa
#p>0.05 -> hipotesis nula
