#                   Practica 1

#         Ejercicio 1
#b)
getwd()

#         Ejercicio 2
#a)
((1+2)/(3+4))^2
#b)
e<-2.7
sqrt(e^2+log2(3))
#c)
factorial(21)
prod(1:21)
#d)
x<-c(-1,0,1)
y<-2*x^2-1;y


#         Ejercicio 3
#a)
orden<-c(1:10);orden
#b)
peso<-seq(71,77,length.out = 10);peso
#c)
edad<-seq(18,by = 0.75,length.out = 10);edad
#d)
grado<-rep(c("M","EA","E"),c(5,4,1));grado
#e)
grado.f<-as.factor(grado)
#f)
str(grado)
str(grado.f)
summary(grado)
summary(grado.f)


#         Ejercicio 4
#a)
v<-scan();v
#b)
v[7:8]<-c(NA,1);v
#c)
sum(v, na.rm = TRUE)
#d)
is.na(v)
which(is.na(v))
#e)
v2<-(is.na(v));v2

#         Ejercicio 6
df<-data.frame(edad,peso,grado);df
str(df)
colnames(df)=c("Edad","Peso","Grado");df
df$Edad
attach(df)
Edad
detach(df)
#busqueda logia en data frames
df
mec<-subset(df,Grado=="M");mec
subset(mec, Edad>=20 & Peso<72)
subset(mec, Edad>=20 || Peso<72)
