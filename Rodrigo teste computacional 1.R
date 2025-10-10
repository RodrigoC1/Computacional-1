

#Gerando uniformes

u=NULL

for (i in 1:500000){
  u[i]=sum(runif(2))
  
  
}


#plotando gráfico de u
hist(u)



#Exercício 1 gastando todo o poder computacional 

uni=runif(12)


#passo 1 
k=NULL
y=NULL
a=Sys.time()

for (i in 1:500000){
  k=mean(runif(12))
  y[i]=12*(k-1/2)
  
  
  
  
}

Sys.time()-a

hist(y)



#Ganhando poder computacional

a=Sys.time()

x=matrix(runif(12*500000),500000,12)
z=colMeans(x)
m=12*(z-0.5)


Sys.time()-a


hist(m)





#algoritimo box miller
u1=runif(2500)
u2=runif(2500)

x1=sqrt(-2*log(u1))*cos(2*pi*u2)
x2=sqrt(-2*log(u1))*sin(2*pi*u2)

hist(x1)
hist(x2)

#Gráfico de densidade 
plot(x1,x2,asp = 1)



#Algoritmo de metodo da inversão



op2=rexp(5000)


#método da inversão

#Gerando uniformes
u=runif(5000)


#a equação equivalente a inversa
xn=-1/3*log(1-u)

#gerando o histogram
g1=hist(xn)
g2=op2


#exemplo 2


j=3
k=3

#gerando amostras uniforme aleatorias
ab=runif(5000)

#inversa da weibull
xm=j*(-log(1-ab))**1/k



#histogram aplicado 
hist(xm)



#exemplo discreto






#Probabilidades
p1=0.50
p2=0.25
p3=0.15
p4=0.08
p5=0.02

x=c()
for(i in 1:5000){

  j=1
  sair=FALSE
  u=runif(1)
  s=borel(1)
  while (sair==FALSE) {
    if(u<=s){
      
      x[i]=j
      sair=TRUE
      
    }else{
      j=j+1
      s=s+borel(j)
      
      
      
    }
    
    
    
    
  }
  

  x
  
}

hist(x)
###






j=1
sair=FALSE

nu=c(p1,p2,p3,p4,p5)

u=runif(1)
s=borel(1)

while (sair==FALSE) {
  if(u<=s){
    
    x=j
    sair=TRUE
    
  }else{
    j=j+1
    s=s+borel(j)
    
    
    
  }
  
  
  
  
}






# Exermplo de Borel 

a=1/2


borel=function(x){
  ab15=(exp(-a*x)*(a*x)**(x-1))/factorial(x)
  return(ab15)
  
}

borel(1)





