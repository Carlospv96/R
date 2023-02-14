x<-c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5) 
b0<-c() 
b1<-c()
RM<-c()
y<-c()
coefi<-c()
aux1<-0
aux2<-0
e<-0
for (i in 1:500) {
  e=rnorm(20,0,16)
  y=50+10*x+e
  reg_lin<-lm(y~x)
  coefi<-reg_lin$coefficients
  #b0 son los interceptos
  b0[i]<-coefi[1]
  #b1 las pendientes
  b1[i]<-coefi[2]
  #Respuesta media
  RM[i]<-reg_lin$fitted.values[9]
  a<-confint(reg_lin,level = 0.95)[2,1]
  b<-confint(reg_lin,level = 0.95)[2,2]
  
  #Ver si el calor verdadero c=10 esta en el intervalo de confianza de las pendientes
  if(a<= 10 & 10<= b){
    aux1<-aux1+1
  }
  
  #predict.lm calcula los intervalos de confianza de la respuesta media
  a<-predict.lm(reg_lin,interval = "confidence", level=0.95)[9,2]
  b<-predict.lm(reg_lin,interval = "confidence", level=0.95)[9,3]
  
  if(a<=100 & 100<=b){
    aux2<-aux2+1
  }
  
}

aux1
aux2

hist(b0,main = "Histograma de los interceptos",xlab = "Interceptos")
hist(b1,main = "Histograma de las pendientes",xlab = "Pendientes")
hist(RM,main = "Histograma de la respuesta media estimada E(y|x=5)",xlab = "Respuesta media estimada E(y|x=5)")
