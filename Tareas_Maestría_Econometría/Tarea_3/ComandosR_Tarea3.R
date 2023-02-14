#cargamos los datos Female GDPPPP

datos<-read.csv(choose.files(),header =  TRUE)

datos                    

#regresión lineal simple

#realizamos al regresion lineal simple

rl<-lm(GHI~BPL+GDP,data = datos)

summary(rl)

#a) Verificamos si hay multicolinealidad

library(car)

vif(rl)

#b) grafica de los residuales vs los valores estimados

plot(predict(rl),rl$residuals,ylab = "Residuales", xlab = "Valores estimados",
     title(main = "Residuales vs. Valores estimados"),pch=19,col="darkorchid1")

#c) cargamos la libreria correspondiente

library(lmtest)

#ordenamos los datos BPL

S=datos[order(datos$BPL,datos$GHI,datos$GDP),]

MR<-lm(GHI~BPL+GDP,S)

gqtest(MR,fraction = 10)

#d) prueba de BPG

library(skedastic)

bptest(rl,studentize = FALSE)

#e) prueba de white

white_lm(rl,interactions=TRUE)

#f) con el FFV tenemos

FFV=3*datos$BPL+datos$GDP

W=1/FFV

#regresion con los pesos

rl1<-lm(GHI~BPL+GDP,weights = W,data = datos)

predict(rl1, newdata=data.frame(BPL=3.85,GDP=13.15))


#g) Clculamos S0 con la siguiente funcion

library(sandwich)

vcovHC(rl,type='HC')

#h) cargamos la libreria randtests

library(randtests)

runs.test(MR$residuals,threshold = 0)

#i) Prueba de d-w

dwtest(MR)

#j) Eliminando autocorrelación, asumiendo que existe

dwtest(rl)

R<-data.frame()

for (i in 1:93) {
  for (j in 1:3) {
    R[i,j]<-datos[i+1,j+1]-(1-2.2631/2)*datos[i,j+1]
  }
}

MRA<-lm(V1~V2+V3,R)

predict(MRA, newdata=data.frame(V2=4.45,V3=12.54))
