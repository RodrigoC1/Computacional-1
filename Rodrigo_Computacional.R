

#Exercicio 1
set.seed(1)


#Gerando função base do exercício
f=function(x){
  280*x^4*(1-x)^3
}

#Descobrindo C conforme minha função
c=f(4/7)

#Plotando curva de f,mas não nescessária 
curve(f)


#Condição de parada para o loop 
aceite=0

#Observe o n
n=5000
x=NULL

#Estrutura de repetição até que aceite seja igual a n que no caso do exercicio é 5000
while (aceite<=n) {

#Dentro da estrutura gerando uma uniforme 
y=runif(1)
#Gerando outra uniforme para fazer o loop funcionar 
u=runif(1,min = 0,max = 1)

#Razão que será testada ate que a condição seja satisfeita
razao=f(y)/cy

#Estrutura de condição com o aceite para parada
if(  u < razao){ 
  aceite=aceite+1
  x[aceite] = y

}
}
#Gerando o Histogram depois de satisfeita a parada
hist(x,freq = FALSE)
#Adicionando curva da minha função base 
curve(f(x),add = TRUE)








#Exercicio 2

#a inicial arbitrário
a=1



f=function(x)2*dnorm(x)*pnorm(a*x)



g=function(x)dnorm(x)

y=rnorm(1)
g=dnorm(1)


#argumento da razao pra decobrir C
razao=function(x)f(x)/g(x)




curve(razao(x),-3,3)



c=3
n=10000
aceite=0
x=NULL

#Estrutura da passada
while (aceite<=n) {
  #Atenção na função adquirida agora 
  y=rnorm(1)
  u=runif(1,min = 0,max = 1)#Sempre usando método de uniforme
  r=razao(y)/c#razão lembre-se que usou o nome razão no experimento passado 
  
  #mesmo método do passado 
  if(  u < r){ 
    aceite=aceite+1
    x[aceite] = y
    
  }
  
  
  
}

#plotando os gráficos
hist(x,freq = FALSE)
#Curva para plotar a linha em cuma da curva
curve(f(x),add = TRUE)





#Exercicio 3



f=function(x) x*exp(-x)

lambda=1


g=function(x) lambda*exp(lambda*x)






razao=function(x)f(x)/g(x)


curve(razao(x),0,5)






#exemplo 

fx <- function(x) (3/16)*(x^2+.5*x^3)
curve( fx(x),-2,2, lwd = 2)


lines(c(-2,-1.5),c(0.5,0.15))
lines(c(-1,-0.5),c(0.2,0.05))








