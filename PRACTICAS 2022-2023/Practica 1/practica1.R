#EJERCICIOS DE CLASE
#INTRODUCCION DE DATOS
x<-c(1.2,2.7,3.2,4.5,3.3,4.4,5.6,7,9,2.3,5,4.3,8.1,3.4,5.1,6.8,1.9,1.8,3,2.5)
x
sort(x)
length(x) #longitud
y<-exp(x)
y
plot(x,y)
hist(x)
sqrt(3)
x<-5

#LECTURA DE DATOS DE UN ARCHIVO
setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Practica 1/")
getwd()
costesdatatxt <- read.table(file="coste.txt",header=TRUE,dec=".",sep="\t")
costesdatatxt
view(costesdatatxt)
View(costesdatatxt)
attach(costesdatatxt)
names(costesdatatxt)
mean(Costo.mat)
View(costesdatatxt)
Costo.mat
class(Costo.mat)
install.packages("openxlsx")
library(readxl)
library(openxlsx)
costedataxlsx <- read_excel("coste.xlsx")
View(costedataxlsx)


#DATA FRAME
num<-c(1,2,3,4,5,6,7)
numcua<-num^2
numcub<-num^3
marco.datos<-data.frame(num,numcua,numcub)
marco.datos
num
mean(numcua)

#SELECCION DE ELEMENTOS DE UN OBJETO
str(marco.datos)
marco.datos$num
numcua
numcua[5]
numcua[numcua<4]
numcub[numcub==7]
#DEVUELVEN LOS VALORES ^
#DEVUELVEN LAS POSICIONES V
which(numcua>10)
which(numcub>0)
which(numcub>10)
which(numcub==7)

numcua <- numcua[-c(1,3,5,7)]
numcua

a<-1:10
a
b<-c(1,3,7,15,20)
b[b%in%a]

f1<-function(x) 2*x^2+1
f1(-5)

f2<-function(x) sin(exp(x))
f2(-1)
f2(0)
f2(1)
plot(f2,-2,2)
