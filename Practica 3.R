datos<-read.table("datos.txt", header=TRUE, sep = "\t", quote = "\"'", dec = ".")
str(datos)
# EJERCICIO 4. ESTADISTICOS DE POSICION CENTRAL

#a) OBTENER LAS MEDIAS DE LAS VARIABLES
#Guardamos todos los valores del df en una variable
altura=datos$Altura;altura 
peso=datos$Peso
edad=datos$Edad
# Hacemos la media aritmetica
mean(altura, na.rm=TRUE) #MEDIA DE ALTURA
mean(peso, na.rm=TRUE) #MEDIA DE PESO
mean(edad, na.rm=TRUE) #MEDIA DE EDAD
# Redondeamos con round() en caso de necesario

#b) REPETIR EL APARTADO ANTERIOR HACIENDO USO DE LA FUNCION APPLY()
apply(datos[1:3],2,FUN=mean,na.rm=TRUE)

#c) CALCULAR DE NUEVO LAS TRES MEDIAS ANTERIORES ELIMINANDO EL 5% DE LOS VALORES 
#MAS EXTREMOS DE CADA UNO DE LAS VARIABLES
apply(datos[1:3],2,FUN=mean,trim=0.05,na.rm=TRUE)

#d) OBTENER LAS MEADIANAS DE LAS VARIABLES
median(altura,na.rm=TRUE)

#e) REPETIR EL APARTADO ANTERIOR HACIENDO USO DE LA FUNCION APPLY()
apply(datos[1:3],2,FUN=median,na.rm=TRUE)

#f) OBTENER LA MODA DE LAS VARIABLES ALTURA (CUANTITATIVA) Y POBLACION (CUALITATIVA)
as.numeric(names(which.max(table(altura)))) 



#EJERCICIO 5. ESTADISTICOS DE DISPERSION

# a) OBTENER EL RECORRIDO DE LAS VARIABLES
#METODO1
diff(range(altura))
diff(range(peso))
diff(range(edad))

# b) CALCULAR SU VARIANZA
cvar=apply(datos[1:3],2,FUN=var,na.rm=TRUE);cvar #ESTO ES LA QUASIVARIANZA QUE LA PROPIA VARIANZA
n=length(altura)
var=((n-1)/n)*cvar;var #ESTO SI ES LA VARIANZA


# c) DETERMINAR SU DESVIACION TIPICA
desviacion=sqrt(var);desviacion
c.desviacion=sqrt(cvar);c.desviacion

# d) CALCULAR EL COEFICIENTE DE VARIACION DE PEARSON, ¿CUAL DE LAS TRES VARIABLES PRESENTA MAYOR DISPERSION?
cv=function(x)
{
  n=length(x)
  sd.x=sqrt((n-1)/n)*sd(x)
  sd.x/abs(mean(x))*100
}
cv(altura)

#EJERCICIO 6. ESTADISTICOS DE POSICION NO CENTRAL

#a) CALCULAR LOS SIGUIENTES CUANTILES: C0.10, C0.25, C0.50, C0.75
quantile(altura,0.10,na.rm=TRUE) #CUANTIL 0.10
quantile(altura,0.25,na.rm=TRUE) #CUANTIL 0.25
quantile(altura,0.50,na.rm=TRUE) #CUANTIL 0.50
quantile(altura,0.75,na.rm=TRUE) #CUANTIL 0.75

#b) COMPRAR LOS RESULTADOS OBTENIDOS CON LA TABLA DE FRECUENCIAS DE LA VARIABLE
cumsum(prop.table(table(altura)))

#c) OBTENER TODOS LOS DECILES
quantile(altura,seq(0,1,0.10),na.rm=TRUE) #DECILES (10)

#d) HALLAR EL RANGO INTERCUARTILICO
IQR(altura)

#EJERCICIO 7. NUMEROS RESUMEN

#a) DETERMINAR LOS CINCO NUMEROS RESUMEN DE LAS VARIABLES JUNTO CON SU MEDIA
fivenum(altura)
summary(altura)

#b) OBTENER LOS NUMEROS RESUMEN DE LA VARIABLE ALTRUA CLASIFICADOS SEGUN LOS NIVELES DE LA VARIABLE GRADO
by(altura, datos$Grado,FUN=summary)

# EJERCICIO 8. DIAGRAMAS DE CAJA

#a) TRAZAR UN DIAGRAMA DE CAJA BASICO
boxplot(altura)

#b) REPETIR EL APARTADO ANTERIOR DANDO COLOR A LA CAJA Y AÑADIENDO ALGUN TITUTLO AL GRAFICO
boxplot(altura,main="Boxplot de Altura",ylab="Metros",col="Light Blue", notch=TRUE)

#c) ETIQUETAR EN EL GRAFICO LOS VALORES REPRESENTADOS EN EL DIAGRAMA (HACER USO DE LA INFORMACION ADICIONAL QUE FACILITA BOXPLOT)
b<-boxplot(altura,main="Boxplot de Altura",ylab="Metros",col="Light Blue", notch=FALSE)
b$stats #NOS DA LOS CUANTILES Y LOS EXTREMOS DE LOS BIGOTES
b$out #NOS DA LOS DATOS ATIPICOS


#d) REPRESENTAR EL BOXPLOT EN FORMA HORIZOTAL
boxplot(altura,horizontal=TRUE,main="Boxplot de Altura",ylab="Metros",col="Light Blue", notch=FALSE)

#e) TRAZAR LOS DIAGRAMAS DE CAJA PARA LA VARIABLE NUMERICA ALTURA CLASIFICADOS SEGUN LOS NIVELES DEL FACTOR GRADO
boxplot(altura~datos$Grado,horizontal=TRUE,main="Boxplot de Altura",ylab="Metros",col="Light Blue", notch=FALSE)

# EJERCICIO 9. ESTADISTICOS DE FORMA

#a) DETERMINAR LA SIMETRIA DE SUS DISTRIBUCIONES
skewness(altura) #ASIMETRIA

#b) OBTENER LA CURTOSIS DE SUS DISTRIBUCIONES
kurtosis(altura) #CURTOSIS

#c) COMPRAR LOS RESULTADOS ANTERIORES CON LAS GRAFICAS DE DENSIDAD DE LAS VARIABLES