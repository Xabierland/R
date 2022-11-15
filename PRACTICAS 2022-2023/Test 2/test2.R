###1
curve(dnorm(x,30,3),0,60,ylab="f(x)")
###2
##X:"Numero de baldosas defectuosas en 500m"~P(25)
#a) A:"Numero de baldosas defectuosas en 100m"~P(50)
#P(X>40)->1-P(X<=40)
p<-1-ppois(40,50)
##b) B:"Numero de zonas a arreglar entre 100"~B(100,0.91393)
#P(X<90)->P(X<=89)
pbinom(89,100,p)
##c) C:"Numero de zonas a arreglar entre 10"~B(10,0.91393)
#P(X>7)->1-P(X<=7)
1-pbinom(7,10,p)
