setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Test 1/")

##Test1
datos <- read.table("edificios2.txt", header = T, dec = ".", sep = "\t")
attach(datos)
quantile(altura, 0.2)
varianza<-var(altura)*(length(altura)-1)/length(altura);varianza
IQR(altura)
IQR(anchura)
sum(altura >= 70)
