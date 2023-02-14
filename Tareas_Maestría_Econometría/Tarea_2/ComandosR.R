#cargamos los datos Female GDPPPP

datos<-read.csv(choose.files(),header =  TRUE)

datos                    

mse<-function(rl){
  mean(rl$residuals^2)
}

#regresión lineal simple

#a) realizamos al regresion lineal simple

rl<-lm(LFPRFemale~GDPPPP,data = datos)

summary(rl)

#b) valor esperado con 122.5469

predict(rl, newdata=data.frame(GDPPPP=122.5469))

#c) grafica de los datos y linea ajustada

plot(datos$GDPPPP,datos$LFPRFemale,xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Regresión lineal simple"),pch=19,col="mediumpurple1")

abline(rl)

#d) calculamos el nivel de influencia y la distancia de cook

Hrl<-hatvalues(rl)

for (i in 1:175) {
  if(Hrl[i]>=2/175) {
    print(i)
  }
}

drls<-cooks.distance(rl)

for (i in 1:175) {
  if(drls[i]>1) {
    print(i)
  }
}

#MSE

mse(rl)

#Akaike

AIC(rl)

#Modelo sin Intercepto

#e) a) realizamos al regresion lineal sin intercepto

rlint<-lm(LFPRFemale~GDPPPP-1,data = datos)

summary(rlint)

#b) valor esperado con 122.5469

predict(rlint, newdata=data.frame(GDPPPP=122.5469))

#c) grafica de los datos y linea ajustada

plot(datos$GDPPPP,datos$LFPRFemale, xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Regresión lineal sin intercepto"),pch=19,col="burlywood1")

abline(rlint)

#d) calculamos el nivel de influencia y la distancia de cook

Hrlint<-hatvalues(rlint)

for (i in 1:175) {
  if(Hrlint[i]>=2/175) {
    print(i)
  }
}

drlsint<-cooks.distance(rlint)

for (i in 1:175) {
  if(drlsint[i]>1) {
    print(i)
  }
}

#MSE

mse(rlint)

#Akaike

AIC(rlint)

#Modelo LinLog

#ejecutamos el comando para el modelo linlog

rllinlog<-lm(LFPRFemale~log(GDPPPP),data = datos)

summary(rllinlog)

#b) valor esperado con 122.5469

predict(rllinlog, newdata=data.frame(GDPPPP=122.5469))

#c) grafica de los datos y linea ajustada
  
plot(sort(datos$GDPPPP),datos$LFPRFemale, xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Modelo Linlog"),pch=19,col="firebrick2")

lines(sort(predict(rllinlog, newdata=data.frame(GDPPPP=1:1500))))

lines(sort(rllinlog$coefficients[1]+log(datos$GDPPPP^rllinlog$coefficients[2])))

#d) calculamos el nivel de influencia y la distancia de cook

Hlinlog<-hatvalues(rllinlog)

Hlinlog<-sort(Hlinlog)

for (i in 1:175) {
  if(Hlinlog[i]>=2/175) {
    print(i)
  }
}

dlinlog<-cooks.distance(rllinlog)

for (i in 1:175) {
  if(dlinlog[i]>1) {
    print(i)
  }
}

#MSE

mse(rllinlog)

#Akaike

AIC(rllinlog)

#Modelo LogLin

#ejecutamos el comando para el modelo linlog

rlloglin<-lm(log(LFPRFemale)~GDPPPP,data = datos)

summary(rlloglin)

#b) valor esperado con 122.5469

exp(predict(rlloglin,newdata=data.frame(GDPPPP=122.5469)))

#c) grafica de los datos y linea ajustada

plot(sort(datos$GDPPPP),datos$LFPRFemale, xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Modelo Loglin"),pch=19,col="mediumspringgreen")

lines(exp(sort(predict(rlloglin, newdata=data.frame(GDPPPP=0:1300)))))

#d) calculamos el nivel de influencia y la distancia de cook

Hloglin<-hatvalues(rlloglin)

Hloglin<-sort(Hloglin)

for (i in 1:175) {
  if(Hloglin[i]>=2/175) {
    print(i)
  }
}

dloglin<-cooks.distance(rlloglin)

for (i in 1:175) {
  if(dloglin[i]>1) {
    print(i)
  }
}

#MSE

mean((exp(predict(rlloglin))-datos$LFPRFemale)^2)

#Akaike

AIC(rlloglin)

#Modelo LogLog

#ejecutamos el comando para el modelo linlog

rlloglog<-lm(log(LFPRFemale)~log(GDPPPP),data = datos)

summary(rlloglog)

#b) valor esperado con 122.5469

exp(predict(rlloglog,newdata=data.frame(GDPPPP=122.5469)))

#c) grafica de los datos y linea ajustada

plot(sort(datos$GDPPPP),datos$LFPRFemale, xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Modelo Loglog"),pch=19,col="lightsalmon1")

lines(exp(sort(predict(rlloglog, newdata=data.frame(GDPPPP=0:1300)))))

#d) calculamos el nivel de influencia y la distancia de cook

Hloglog<-hatvalues(rlloglog)

Hloglog<-sort(Hloglog)

for (i in 1:175) {
  if(Hloglog[i]>=2/175) {
    print(i)
  }
}

dloglog<-cooks.distance(rlloglog)

for (i in 1:175) {
  if(dloglog[i]>1) {
    print(i)
  }
}

#MSE

mean((exp(predict(rlloglog))-datos$LFPRFemale)^2)

#Akaike

AIC(rlloglog)

#Modelo Reciproco

#generamos los datos reciprocos

datos$rcpGDPPPP<-1/datos$GDPPPP

#ejecutamos el comando para el modelo reciproco

rlrcp<-lm(LFPRFemale ~ rcpGDPPPP,data = datos)

summary(rlrcp)

#b) valor esperado con 122.5469

predict(rlrcp,newdata=data.frame(rcpGDPPPP=1/122.5469))

#c) grafica de los datos y linea ajustada

plot(sort(datos$GDPPPP),datos$LFPRFemale, xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Modelo Reciproco"),pch=19,col="lightseagreen")

lines(sort(predict(rlrcp, newdata=data.frame(rcpGDPPPP=seq(0,1,by=0.00005)))))

#d) calculamos el nivel de influencia y la distancia de cook

Hrcp<-hatvalues(rlrcp)

Hrcp<-sort(Hrcp)

for (i in 1:175) {
  if(Hrcp[i]>=2/175) {
    print(i)
  }
}

drcp<-cooks.distance(rlrcp)

for (i in 1:175) {
  if(drcp[i]>1) {
    print(i)
  }
}

#MSE

mse(rlrcp)

#Akaike

AIC(rlrcp)

#Modelo Log Reciprocro

#ejecutamos el comando para el modelo LOG reciproco

rllogrcp<-lm(log(LFPRFemale) ~ rcpGDPPPP,data = datos)

summary(rllogrcp)

#b) valor esperado con 122.5469

exp(predict(rllogrcp,newdata = data.frame(rcpGDPPPP=1/122.5469)))

#c) grafica de los datos y linea ajustada

plot(sort(datos$GDPPPP),datos$LFPRFemale, xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Modelo log-Reciproco"),pch=19,col="deepskyblue2")

lines(exp(sort(predict(rllogrcp, newdata=data.frame(rcpGDPPPP=seq(0,1,by=0.00005))))))

#d) calculamos el nivel de influencia y la distancia de cook

Hlogrcp<-hatvalues(rllogrcp)

Hlogrcp<-sort(Hlogrcp)

for (i in 1:175) {
  if(Hlogrcp[i]>=2/175) {
    print(i)
  }
}

dlogrcp<-cooks.distance(rllogrcp)

for (i in 1:175) {
  if(dlogrcp[i]>1) {
    print(i)
  }
}

#MSE

mean((exp(predict(rllogrcp))-datos$LFPRFemale)^2)

#Akaike

AIC(rllogrcp)

#Modelo Polinomial grado 3

#Generamos los datos al cubo

datos$GDPPPP3<-(datos$GDPPPP)^3

#ejecutamos el comando para el modelo polinomial

rlp3<-lm(LFPRFemale ~ GDPPPP3,data = datos)

summary(rlp3)

#b) valor esperado con 122.5469

predict(rlp3, newdata=data.frame(GDPPPP3=122.5469))

#c) grafica de los datos y linea ajustada

plot(sort(datos$GDPPPP),datos$LFPRFemale, xlab = "GDPPPP", ylab = "LFPRFemale",
     title(main = "Modelo Polinomial de grado 3"),pch=19,col="springgreen4")

lines(sort(predict(rlp3, newdata=data.frame(GDPPPP3=1:1350))))

#d) calculamos el nivel de influencia y la distancia de cook

Hp3<-hatvalues(rlp3)

Hp3<-sort(Hp3)

for (i in 1:175) {
  if(Hp3[i]>=2/175) {
    print(i)
  }
}

dp3<-cooks.distance(rlp3)

for (i in 1:175) {
  if(dp3[i]>1) {
    print(i)
  }
}

#MSE

mse(rlp3)

#Akaike

AIC(rlp3)
