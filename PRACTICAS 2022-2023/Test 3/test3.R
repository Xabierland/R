setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Test 3/")

#1
temperatura <- read.table("Temperatura.txt",header=T, dec=".", sep="\t")
attach(temperatura)

#2
mean(Samorc)

#3
sd(Gamotegui)

#4 Creo que se ha colado un uno en la solucion.
t.test(Gamotegui, conf.level = 0.99)$conf

#5 p>=1500 - H0= mu1 >= 1500 | Ha= mu1 < 1500
t.test(Samorc, mu=1500, alternative="less", conf.level = 0.95)

#6 
var.test(Ramacint, Gamotegui, conf.level = 0.9) #Contiene el 1 luego son iguales
t.test(Ramacint, Gamotegui, alternative="less", conf.level = 0.9, var.equal = T)
