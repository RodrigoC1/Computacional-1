#Exercicio aula 29/08

#primeiro observe que 

#o n pode ser pre determinado por você
n=98


#Estabelecendo a sequencia 
x0=seq(0,1, length= n+1)


#função para indentificar o ponto médio
pontom=0.5* (x0[-1] + x0[-n-1])
  
                    
#função construida para calcular a integral em suas raízes                    
fun_1=function(x){
  
  x^(3/2)*(1-x)^(7/2)
    
  
}                    

#tirando a média dos pontos
mean(fun_1(pontom))

#função beta
beta(3/2+1, 7/2+1)




#Exemplo 2



x1=seq(1.92,-(-1.92) ,length=n+1)



fun_2=function(x){
  
  3.92*(1/(sqrt(2*pi))*exp(-1/2*(-1.96+3.92*x)^2))
  
}      


#Observe os pontos das quadraturas são fixas.
pontom=0.5* (x0[-1] + x0[-n-1])


mean(fun_2(pontom))




#Exercício regra do trapézio


#Observe o tamanho do N
n=98


#Refazendo a sequencia 
x3=seq(0,1,length=n+1)


#construindo a nova função

fun_3=function(x){
  
  3.92*(1/(sqrt(2*pi))*exp(-1/2*(-1.96+3.92*x)^2))
  
}      

#Plotando
ponto_2=(1/(2*n))*(sum(fun_3(x0[-1])) + sum(fun_3(x0[-n-1])))






#Quadratura de Gaus Legendre

#Pesos para aplicar na função
t0=c(-1/3*sqrt(5+2*sqrt(10/7)),-1/3*sqrt(5-2*sqrt(10/7)),0,+1/3*sqrt(5-2*sqrt(10/7)),+1/3*sqrt(5+2*sqrt(10/7)))


#ponto para aplicar na função
w0=c(((322-13*sqrt(70))/900),((322+13*sqrt(70))/900),128/225,((322+13*sqrt(70))/900),((322-13*sqrt(70))/900))



#função de uma normal parametrizada 
fun_4=function(x){
  1.96*sqrt(1/(2*pi))*exp(-((1.96*x)^2/2))
  
}


sum(w0*fun_4(t0))





#criando função 1


fun_integral_1=function(x){
  x^2
}

#Usando a função integrate
integrate(fun_integral_1,0,2)

#criando função 2


fun_integral_2=function(x){
  sin(x)
}


#aplicando integrate

integrate(fun_integral_2,0,pi)



#Aplicando em um exercicio ..

a=list(1,3,4,5,6)

densidade_integral=function(x){
  fun_int_5=function(x)(2/(*sqrt(pi)))*(exp(-(x)^2))+((2/(15))*(x^3)*exp(-x))
  
  p=integrate(fun_int_5,0,x)
  
  return(p$value)
  
  
}




densidade_integral(Inf)




fun_hoot_1=function(x){
  densidade_integral(x)-0.25
  
}

uniroot(fun_hoot_1,c(1,4))
