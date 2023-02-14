#cargamos los datos Datos_Tarea_4

datos<-read.csv(choose.files(),header =  TRUE)

datos

#a) Generamos la variables dicotomicas para AP, SX y EC

datos$DAP=ifelse(datos$ï..AP=="SI",1,0)

datos$DSX=ifelse(datos$SX=="H",1,0)

datos$DEC=ifelse(datos$EC=="C",1,0)

datos

#Generamos el modelo lineal de probabilidad

rl<-lm(DAP~DSX+DEC+ED+SM+DT,datos)

rl

yh=predict(rl)

FFV=yh*(1-yh)

W=1/FFV

#Modelo lineal de probabilidad

rl1<-lm(DAP~DSX+DEC+ED+SM+DT,weights = W,data = datos)

predict(rl1,newdata=data.frame(DSX=1,DEC=1,ED=36,SM=24.5,DT=8.5))

#b) modelo LOGIT

rlogit=glm(DAP~DSX+DEC+ED+SM+DT,data=datos,family = binomial("logit"))

#una mujer soltera de 30 años, con un salario mensual de 27.25k pesos 
#y que vive a 5.7 kilómetros de su lugar de trabajo tenga un auto propio

u=predict(rlogit,newdata = data.frame(DSX=0,DEC=0,ED=30,SM=27.25,DT=5.7))

#encontramos la probabilidad de esta persona con el siguiente comando

exp(u)/(1+exp(u))

#g) a) primero realizamos el modelo probit

rprobit=glm(DAP~DSX+DEC+ED+SM+DT,data=datos,family = binomial("probit"))

rprobit

#evaluamos para un hombre casado de 36 años con ingreso mensual de 24.5k y que
#vive a una distancia de 8.5km

r=predict(rprobit,newdata=data.frame(DSX=1,DEC=1,ED=36,SM=24.5,DT=8.5))

#encontramos la probabilidad de esta persona al aplicar la funcion normal

pnorm(r)

#b) para una mujer de estas caracterisiticas

v=predict(rprobit,newdata = data.frame(DSX=0,DEC=0,ED=30,SM=27.25,DT=5.7))

#encontramos la probabilidad de esta persona al aplicar la funcion normal

pnorm(v)

#d) obtenemos el valor minimo del salario con el siguiente comando

(qnorm(0.85)-rprobit$coefficients[1]-sum(rprobit$coefficients[2:4]*c(0,1,33))-rprobit$coefficients[6]*5)/rprobit$coefficients[5]

#e) veamos el resumen del modelo rprobit para ver el p valor de la variable del estado civil

summary(rprobit)
