setwd("C:/Users/xabie/Documents/GitHub/R/PRACTICAS 2022-2023/Practica 4/")

#### EJEMPLOS DISCRETAS
###1. Considere una variable aleatoria que sigue una distribucion binomial con parametros n=10, p=0,3. Calcule:
##a) La probabilidad de que la variable tome el valor de 4. Es decir, P(X=4).
dbinom(4,10,0.3)
##b) La probabilidad acumulada hasta llegar al valor de 4. Es decir, P(X<=4).
pbinom(4,10,0.3)

###2. Considere una variable aleatoria que sigue una distribucion geometrica con parametro p=0.45. Calcule:
##a) La probabilidad de que la variable tome el valor de 3. Es decir, P(X=3).
dgeom(3,0.45)
##b) La probabilidad acumulada hasta llegar al valor de 3. Es decir, P(X<=3)
pgeom(3,0.45)

###3. Considere una variable aleatroia que sigue una distribucion de Poisson con parametro lam=3
##a) La probabilidad de que la variable aleatroia tome el valor de 8. Es decir, P(X=8).
dpois(8,3)
##b) La probabilidad acumulada hasta llegar al valor de 8. Esto es, P(X<=8)
ppois(8,3)

###4. Una variable aleatroia que sigue una distribucion binomial negativa. En un laboratorio, la probabilidad de que una medida se registre de forma correcta es de 0.78
##a) Cual es la probabilidad de tener que realizar 15 mediciones para registrar 10 correctas
dnbinom(5,10,0.78)
##b) Cual es la probabilidad de tener que realizar al menos 15 mediciones para registrar 10 mediciones correctas
1-pnbinom(4,10,0.78)

###5. Una variable aleatoria que sigue una distribucion hipergeometrica. Se tiene 10 bolas en una urna, 6 negras y 4 blancas. Se sacan 3 bolas sin reposicion.
##a) Cual es la probabilidad de obtener 2 bolas blancas
dhyper(2,4,6,3)
##b) Cual es la probabilidad de obtener como maximo 2 bolas blancas?
phyper(2,4,6,3)
##c) Cual es la probabilidad de obtener al menos 2 bolas blancas?
1-phyper(1,4,6,3)
###6. Lanzamiento de 5 dados y contabilizar el numero de 1 o 2 obtenidos. Es decir, X: Numero de 1 o 2 logrados en el lanzamiento de 5 dados
x<-0:5
plot(x,dbinom(x,5,2/6),type="h",ylab="p(x)")
plot(x,pbinom(x,5,2/6),type="s",ylab="F(x)")

####EJEMPLOS CONTINUAS
###1. Una variable aleatoria que sigue una distribucion uniforme. Para metros a=10, b=40
##a) P(x<30)
punif(30,10,40)
###2. Una variable aleatoria que sigue una distribucion exponencial. Parametro B=5. Calcule:
##a) P(4<=x<=6)
pexp(6,1/5)-pexp(4,1/5)
###3. Una variable aleatoria que sigue una distribucion normal. Parametros u=65.6 o=14.74. Calcule:
##a) P(X<60)
pnorm(60,65.6,14.74)
##b) Que valor de x deja el 12,1 de la distribucion a su derecha?
qnorm(0.121,65.6,14.47,lower.tail = F)
##c) P(X>45)
1-pnorm(45,65.6,14.74)
###4. Una variable aleatroia que sigue una distribucion exponencial con B=3. Represente la funcion de densidad
curve(dexp(x,1/3),from=0, to=10, ylab="f(X)")
###5. Una variable aleatoria que sigue una distribucion exponencial con B=3. Represente la funcion de distribucion.
curve(pexp(x,1/3),0,20,ylab="F(x)")
###6. Una variable aleatoria que sigue una distribucion normal con u=4 y o=1. Represente la funcion de densidad.
curve(dnorm(x,4,1),0,8,ylab="f(x)")

####EJERCICIOS
###1. Al año una consultoria de promedio da consejo a 1200 personas. En un momento al azar, calcule:
##a) La probabilidad de dar consejo a mas de 1085 personas.
#P(X>=1085)->1-P(X<1085)->1-P(X<=1084)
1-dpois(1084,1200)
##b) La probabilidad de dar consejos entre 1200 y 1300 personas.
#P(1200<=X<=1300)->P(X<=1300)-(1-P(X<=1199))
ppois(1300,1200)-(1-ppois(1199,1200))

###4. Supongase que un sistema se compone de 9 elementos y que para que el sistema funcione de forma correcta al menos seis de ellos deben estar operativos
##a) Si la probabilidad de que un elemento esta operativo es de 0.95, calcule la probabilidad de que todo el sistema funcione de forma correcta.
#P(x>=6)->1-P(X<6)=1-P(X<=5)
1-pbinom(5,9,0.95)
##b) Represente la funcion de probabilidad.
x<-0:9
plot(x,dbinom(x,9,0.95),type="h",ylab="p(x)")
