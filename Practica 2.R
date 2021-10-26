datos<-read.table("datos.txt", header=TRUE, sep = "\t", quote = "\"'", dec = ".")

#Ejercicio 2
#a)
fabs<-table(datos$Grado);fabs #Frecuencia Absoluta
frel<-prop.table(fabs);frel #Frecuencia Relativa
frel<-as.vector(frel);frel #Quitamos el nombre de las columnas de frecuencia relativa
tabla.f<-data.frame(fabs,frel);tabla.f #Creamos una tabla de frecuencias
colnames(tabla.f)<-c("Grado","Fr.Abs","Fr.Rel");tabla.f #Cambiamos sus nombres
#b)
tabla.f$Fr.Abs.Acum<-cumsum(tabla.f$Fr.Abs);tabla.f #Sumo consecutivamente las frecuencias absolutas y las guardo en una nueva variable
tabla.f$Fr.Rel.Acum<-cumsum(tabla.f$Fr.Rel);tabla.f #Sumo consecutivamente las frecuencias relativas y las guardo en una nueva variable
#c)
barplot(tabla.f$Fr.Abs) #Forma mas basica de hacer un diagrama de barras
barplot(tabla.f$Fr.Abs,xlab="Grado",ylab="Frecuencia Absoluta", main = "Diagrama de barras", col = "LightBlue", border = "Black",names.arg = c("EAut","Elec","Mec"))
text(c(0.7,1.9,3.1),tabla.f$Fr.Abs/2,labels=tabla.f$Fr.Abs,col = "red")
#FALTA FR.REL
#e)
pie(fabs,main="Diagrama de sectores")
#f)
plot(fabs, type="l",col="red",lwd=3,xlab="Grado",ylab="Frecunecia absoluta",main="Poligono de frecuencias")
plot(fabs, type="b",col="red",lwd=3,xlab="Grado",ylab="Frecunecia absoluta",main="Poligono de frecuencias")
text(c(1,2,3),tabla.f$Fr.Abs-5,labels=tabla.f$Fr.Abs,col = "red")

#EJERCICIO 3
#a)
n<-length(datos$Altura);n
round(sqrt(n))
nclass.Sturges(datos$Altura)
#Hay que entender los dos ultimos valores como recomendaciones
k<-10
L<-min(datos$Altura);H<-max(datos$Altura);L;H;A<-(H-L)/k;A #CALCULAMOS LOS INTERVALOS
diff(range(datos$Altura))/k #Otra Opcion

#b)
lim<-seq(L,H,A);lim #Los limites puestos como un Array
cut(datos$Altura,lim,include.lowest = TRUE,right = FALSE) 
int.clase<-cut(sort(datos$Altura),lim,include.lowest = TRUE,right = FALSE);int.clase #Ordena
fabsA=table(int.clase);fabsA    #Frecuencia Absoluta
frelA=prop.table(fabsA);frelA   #Frecuencia Relativa
fabsAcum=cumsum(fabsA);fabsAcum #Frecuencia Absoluta Acumulada
frelAcum=cumsum(frelA);frelAcum #Frecuencia Relativa Acumulada

intervalos=levels(int.clase);intervalos
t.frec<-data.frame(intervalos,fabsA,frelA,fabsAcum,frelAcum);t.frec

#c)
stem(datos$Altura)
stem(datos$Altura,2)
stem(datos$Altura,0.25,85)

#d)
h=hist(datos$Altura,lim,include.lowest=TRUE, right=TRUE, plot=FALSE)
str(h)
hist(datos$Altura,breaks=lim,xaxt="n",right=TRUE,freq=TRUE,col="light blue",main="Histograma frecuencias absolutas", xlab = "Intervalos de clase", ylab="Frecuencias absolutas")
axis(1,at=lim)
