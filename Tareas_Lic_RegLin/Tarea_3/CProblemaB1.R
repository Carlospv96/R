nfl<-data.frame(
  JueGan = c(10,11,11,13,10,11,10,11,4,2,7,10,9,9,6,5,5,5,6,4,3,3,4,10,6,8,2,0),
  YardAir = c(1985,2855,1737,2905,1666,2927,2341,2737,1414,1838,1480,2191,2229,2204,2140,1730,2072,2929,2268,1982,1792,1606,1492,2835,2416,1638,2649,1503),
  JugTierr = c(59.7,55.0,65.6,61.4,66.1,61.0,66.1,58.0,57.0,58.9,67.5,57.2,58.8,58.6,59.2,54.4,49.6,54.3,58.7,51.7,61.9,52.7,57.8,59.7,54.9,65.3,43.8,53.5),
  JugTierrCon = c(2205,2096,1847,1903,1457,1848,1564,1821,2577,2476,1984,1917,1761,1709,1901,2288,2062,2861,2411,2289,2203,2592,2053,1979,2048,1786,2876,2560)
)

nfl

regres.multp<-lm(JueGan ~ YardAir+JugTierr+JugTierrCon,data = beisbol)

regres.multp$fitted.values

regres.multp$coefficients

regres.multp$residuals

p<-0

for (i in 1:27) {
  p[i]<-(i-1/2)/27
}

p

par(mfrow=c(2,2))

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

#Grafica normal
plot(sort(regres.multp$residuals),p,pch=19,col="red",xlab = "Residuo", ylab = "Probabilidad", main="Gráfica de Probabilidad Normal")
abline(lm(p[7:20]~sort(regres.multp$residuals)[7:20]),col="blue")

plot(sort(rstandard(regres.multp)),p,pch=19,col="green",xlab = "Residuo Estandarizado", ylab = "Probabilidad", main = "Gráfica de probabilidad Normal")
abline(lm(p[7:19]~sort(rstandard(regres.multp))[7:19]),col="blue")

#Grafica RESIDUALES estudentizados
plot(sort(studres(regres.multp)),p,pch=19,col="purple",xlab = "Residuo Estudentizado", ylab = "Probabilidad", main = "Gráfica de probabilidad Normal")
abline(lm(p[7:19]~sort(studres(regres.multp))[7:19]),col="blue")

#Grafica de los residuales contra los valores ajustados

#Normal
plot(regres.multp$fitted.values,regres.multp$residuals,ylim = c(-4,4),pch = 19, col="red",ylab = "Residuo", xlab = "Valores Ajustados", main="Residuos vs. Valores Ajustados")
abline(a=0,b=0,col="blue")

#Estandarizados
plot(regres.multp$fitted.values,rstandard(regres.multp),ylim = c(-3,3),pch=19,col="green",ylab = "Residuo Estandarizado", xlab = "Valores Ajustados", main ="Residuo Estandarizado vs. Valores Ajustados")
abline(a=0,b=0,col="blue")

#Estudentizados
plot(regres.multp$fitted.values,studres(regres.multp),ylim = c(-3,3),pch=19,col="purple",ylab = "Residuo Estudentizados", xlab = "Valores Ajustados", main ="Residuo Estudentizados vs. Valores Ajustados")
abline(a=0,b=0,col="blue")

#Contra los regresores

plot(nfl[,c("YardAir")],studres(regres.multp),ylim = c(-3,3), pch=19, col="purple", ylab = "Residuo Estudentizado" , xlab="Yardas por Aire (Temporada)", main = "Residuo Estudentizado vs. Yardas por Aire")
abline(a=0,b=0,col="blue")

plot(nfl[,c("JugTierr")],regres.multp$residuals,ylim = c(-3,3), pch=19, col="purple", ylab = "Residuo" , xlab="Jugadas por Tierra", main = "Residuo Estudentizado vs. Jugadas por Tierra")
abline(a=0,b=0,col="blue")

plot(nfl[,c("JugTierrCon")],studres(regres.multp),ylim = c(-3,3), pch=19, col="purple", ylab = "Residuo Estudentizado" , xlab="Yardas por Tierra del contrario", main = "Residuo Estudentizado vs. Yardas por Tierra del contrario")
abline(a=0,b=0,col="blue")

#Contra el tiempo

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

ts.plot(regres.multp$residuals,ylab="Residuales",ylim=c(-4,4),main="Residuales vs. Tiempo")
points(regres.multp$residuals,pch=19,col="red")

ts.plot(rstandard(regres.multp), ylab="Residuales Estandarizados",ylim=c(-3,3),main="Residuales estandarizados vs. Tiempo")
points(rstandard(regres.multp),pch=19, col="green")

ts.plot(studres(regres.multp), ylab="Residuales Estudentizados",ylim=c(-3,3),main="Residuales estudentizados vs. Tiempo")
points(studres(regres.multp),pch=19, col="purple")

#Press

X<-matrix(NA,nrow = 28,ncol = 4)

for (i in 1:28) {
  X[i,1]<-1
  X[i,2]<-nfl[,c("YardAir")][i]
  X[i,3]<-nfl[,c("JugTierr")][i]
  X[i,4]<-nfl[,c("JugTierrCon")][i]
}

H<-X%*%solve(t(X)%*%X)%*%t(X)
ncol(H)

PRES<-0

for (i in 1:28) {
  PRES<-PRES+(regres.multp$residuals[i]/(1-H[i,i]))^2
}

for (i in 1:27) {
  PRES<-PRES+(Regres$residuals[i]/(1-H[i,i]))^2
}

PRES

sum((NFLTabla[,1]-regres.multp$fitted.values)^2)
(NFLTabla[,1]-regres.multp$fitted.values)^2

R2<-1-PRESS/326.9643
R2

#Valores atipicos
plot(regres.multp$fitted.values,studres(regres.multp),pch=15,col="blue")

nfl2<-nfl[-c(1),]
nfl2<-nfl2[-c(20),]


regres.multp2<-lm(JueGan ~ YardAir+JugTierr+JugTierrCon,data = nfl2)

regres.multp2$coefficients
regres.multp$coefficients

summary(regres.multp2)$adj.r.squared
summary(regres.multp2)$coefficients
summary(regres.multp)$adj.r.squared
summary(regres.multp)$coefficients
anova(regres.multp)
par(mfrow=c(2,2))
par(mfrow=c(1,1))
p<-0

for (i in 1:27) {
  p[i]<-(i-1/2)/27
}

plot(sort(studres(regres.multp2)),p,pch=19,col="purple",xlab="Residuales Estudentizados", ylab="Probabilidad",main="Gráfica de probabilidad Normal")
abline(lm(p[6:18]~sort(studres(regres.multp2))[6:18]),col="blue")

plot(regres.multp2$fitted.values,studres(regres.multp2),pch=19,col="purple",xlab = "Valores ajustados", ylab="Residuales Estudentizados",ylim = c(-3,3),main = "Estudentizado vs. Ajustados")
abline(h=0,col="blue")

ts.plot(studres(regres.multp2), ylab="Residuales Estudentizados",ylim=c(-3,3))
points(studres(regres.multp2),pch=19, col="purple")

hist(studres(regres.multp2),col="purple",xlab = "Residuo Estudentizado", main = "Histograma")

anova(regres.multp2)

#Transofrmacion (de la ultima grafica todo se ve constante no se necesita una transformacion)

#Balanceo

H[1,1]
2*4/27

dffits<-0
h<-0

for (i in 1:27) {
  h[i]<-H[i,i]
}

h

h_ii<-data.frame(bal=h)
write.csv(h_ii,file = "h.csv")

nfl2<-nfl[-c(18),]
nfl2<-nfl2[-c(26),]


regres.multp2<-lm(JueGan ~ YardAir+JugTierr+JugTierrCon,data = nfl2)

regres.multp2$coefficients
regres.multp$coefficients

summary(regres.multp2)$adj.r.squared
summary(regres.multp2)$coefficients
summary(regres.multp)$adj.r.squared
summary(regres.multp)$coefficients
anova(regres.multp)
par(mfrow=c(2,2))
par(mfrow=c(1,1))
p<-0

for (i in 1:26) {
  p[i]<-(i-1/2)/26
}

plot(sort(studres(regres.multp2)),p,pch=19,col="purple",xlab="Residuales Estudentizados", ylab="Probabilidad",main="Gráfica de probabilidad Normal")
abline(lm(p[6:18]~sort(studres(regres.multp2))[6:18]),col="blue")

plot(regres.multp2$fitted.values,studres(regres.multp2),pch=19,col="purple",xlab = "Valores ajustados", ylab="Residuales Estudentizados",ylim = c(-3,3),main = "Estudentizado vs. Ajustados")
abline(h=0,col="blue")

ts.plot(studres(regres.multp2), ylab="Residuales Estudentizados",ylim=c(-3,3))
points(studres(regres.multp2),pch=19, col="purple")

hist(studres(regres.multp2),col="purple",xlab = "Residuo Estudentizado", main = "Histograma")

anova(regres.multp2)

for (i in 1:28) {
  dffits[i]<-sqrt(h[i]/(1-h[i]))*studres(regres.multp)[i]
}

Pd<-

#DFFITS calcular
dffits[18]
dffits[27]
2*sqrt(4/28)

#Distancia de Cook
cook<-data.frame("cook"=cooks.distance(Regres))
cook

dff<-data.frame(dffi=dffits(Regres))

write.csv(dff,file = "dffits.csv")
write.csv(cook,file="cook.csv")

#Variables indicadoras pendiente

shapiro.test(studres(regres.multp))
durbinWatsonTest(regres.multp)

anova(regres.multp)

write.csv(nfl,file="nfl.csv")

nfl0<-data.frame(0)
x0<-0

for (j in 1:4) {
  for (i in 2:28) {
    x0[i-1]<-nfl[i,j]-0.194457*nfl[i-1,j]
  }
  nfl0<-cbind(nfl0,x0)
}

nfl0<-nfl0[-c(1,8),-c(1)]

Regres<-lm(x0~x0.1+x0.2+x0.3,data = nfl0)
durbinWatsonTest(Regres)
Regres$coefficients
anova(Regres)
summary(Regres)

plot(sort(Regres$residuals),p,pch=19,col="red",xlab = "Residuo", ylab = "Probabilidad", main="Gráfica de Probabilidad Normal")
abline(lm(p~sort(Regres$residuals)),col="blue")
plot(Regres$fitted.values,Regres$residuals,ylim = c(-4,4),pch = 19, col="red",ylab = "Residuales", xlab = "Valores Ajustados", main="Residuos vs. Valores Ajustados")
abline(a=0,b=0,col="blue")
ts.plot(Regres$residuals, ylab="Residuales",ylim=c(-3,3))
points(Regres$residuals,pch=19, col="red")
hist(Regres$residuals,col="red",main = "Histograma de los Residuales",xlab = "Residuales")
plot(nfl0[,3],Regres$residuals,ylim = c(-3,3),pch=19,col="red",ylab = "Residuales",xlab="Jugadas por Tierra", main="Residuales vs. Jugadas por tierra")

s<-0
