#Exercício 

n_1=function(x){
  
  n=x**2-x-1
  n
  
}


#intervalo
a=1
b=2
#erro
erro_1=0.00003

#Observe a condição 
while (abs(b-a)>erro_1) {
  ab_1=(a+b)/2
  
  
  if(n_1(ab_1)*n_1(a)<0){
    b=ab_1
    
    
  }else{
    
    a=ab_1
  }
     
  
}

print(ab_1)



#exercício 2

#
x_1=2

#ep_1 vai ser determinado pelo problema a seu criterio.
ep_1=0.00001


#Observe que a condição tem que ser um número muito grande.
cond=10000


#
while (cond>ep_1) {
  x_m=x_1-(x_1^2-2)/(2*x_1)
  #valor absoluto até a condição ser satisfeita.
  cond=abs(x_m-x_1)
  #Comparação entre x_1=x_m
  x_1=x_m
  
}

print(x_1)
print(sqrt(2))








#exercício 3 metodo da secante (obs)



fun_1=function(x){
  ab=exp(-x)-x
  ab
  
}

x_n1=1
x_n2=0


erro_g=10000

erro_1=0.00003

#
while (erro_g>erro_1) {
  xm=x_n1-fun_1(x_n1)*(x_n1-x_n2)/(fun_1(x_n1)-fun_1(x_n2))
  
  erro_g=abs(xm-x_n1)
  x_n2=x_n1
  x_n1=xm
  
  
  
}


print(xm)


#pacote para o metodo da bisecção
#Automatizando a função.
uniroot(fun_1,c(0,1))

install.packages("pacman")



fun_2=function(x){
  
  n=2*x*exp(-x)-x^2*exp(-x)
  
  
}


#Observe que estou p
curve(fun_2(x),0,4)

abline(h=0)

uniroot(fun_2,c(1.5,2.2))




fun_test=function(x){
  
  n=sin(x)+cos(x*sqrt(2))+sin(pi*x)
  n
  
  
}


#Observe que foi feito o zoom do gráfico até achar o ponto mínimo da função
#usando a função

curve(fun_test(x),-3,-2)


#Usando a derivada aplicando na função e depois de aproximada a curva 
fun_3=function(x){
  
  n=cos(x)-sin(x*sqrt(2))*sqrt(2)+cos(pi*x)*pi
  
  
}

curve(fun_3(x),-10,10)

abline(h=0)

#Procurando a raiz do problema .
uniroot(fun_3,c(-2.6,-2.2))



#Algoritmo Newton rapsom 


Erro_newton=2000


#função para o vetor gadiente 
der_fun_2=function(x,y){
  
  
  gradiente_1=matrix(c(4*x^3-4*y,8*y^3-4*x),ncol = 1,nrow = 2)
  
  
}


#função para a matriz Heisianna
der_fun=function(x,y){
  
  inversa_1=matrix(c(24*y^2,4,4,12*x^2),ncol = 2,nrow = 2)*1/(288*x^2*y^2-16)
  
  
}



x0=c(2,1)
erro=5000


#estrutura de repetição
n_1=while(erro>0.001){
  
  xi=x0-der_fun(x0[1],x0[2]) %*% der_fun_2(x0[1],x0[2])
  
  erro=sqrt(sum((xi-x0)^2))
  
  x0=xi
  
}


print(xi)

