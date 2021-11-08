##################################################EJEMPLO VARIABLE DISCRETA
#BINOMIAL: X~B(n=10, p=0.50)
n=10;p=0.5
#MUESTRA ALEATROIA
mB=rbinom(150,n,p);mB
#MASA DE PROBABILIDAD
dbinom(2,n,p)
x=0:10
fmpB=dbinom(x,n,p);fmpB
#Probabilidad acumulada
pbinom(2,n,p)
fdpB=pbinom(x,n,p);fdpB
#Tabla
tabla.prob=data.frame(x,fmpB,fdpB);tabla.prob
#Cuantiles
qbinom(0.85,n,p) #Cuantil 0.85
qbinom(seq(0,1,0.25),n,p)

graf.masa=plot(x,fmpB,type="h",xlab="valores de x", ylab="f.masa prob.", ylim=c(0,max(fmpB)))
graf.dist=plot(x,fdpB,type="s",xlab="valores de x", ylab="f.distribucion prob.", ylim=c(0,max(fdpB)))

##################################################EJEMPLO VARIABLE CONTINUA
mu=0;sigma=1
#Muestra aleatoria
mN=rnorm(150,mu,sigma);mN
#Funcion de distribucion de probabilidad
dnorm(2,mu,sigma) #EL VALOR DE LA CAMPANA DE GAUS
#LA PROBABILIDAD DE QUE TOME UN VALOR EXACTO ES 0
pnorm(2,mu,sigma) #FUNCION
#CUARTILES
qnorm(0.85,mu,sigma)
qnorm(seq(0,1,0.25),mu,sigma)
#GRAFICA
curve(dnorm(x,mu,sigma),from=-4,to=4)
curve(pnorm(x,mu,sigma),from=-4,to=4)









########################################################EJERCICIOS
#EJERCICIO 1
X="nº personas asesoradas al año"
landa=1200 #TAMBIEN ES LA MEDIA
#a)
ppois(1085,landa, lower.tail = FALSE)
#b)
ppois(1300,landa)-ppois(1199,landa) #INCLUIDOS
ppois(1299,landa)-ppois(1200,landa) #SIN INCLUIR

#EJERCICIO 2
X="puntuacion en el test"
mi=60
tau=10
#a)
a=pnorm(70,mi,tau,lower.tail = FALSE)
a
#b)
b=pnorm(80,mi,tau)-pnorm(39,mi,tau)
b
#c)
c=pnorm(80,mi,tau)-pnorm(40,mi,tau)
c
#d)
d=1-c
d
#e)
e=a*200
e

#EJERCICIO 3
#a)
pexp(45,1/45,lower.tail = FALSE)

#b)
curve(dexp(x,1/40),from=0,to=60)
