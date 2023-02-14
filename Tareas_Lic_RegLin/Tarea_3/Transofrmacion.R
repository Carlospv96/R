B2<-data.frame(Temperatura=c(273,283,293,303,313,323,333,343,353,363,373),
               PresDeVap=c(4.6,9.2,17.5,31.8,55.3,92.5,149.4,233.7,355.1,525.8,760)
)

plot(B2)

par(mfrow=c(1,1))

regres.multp3<-lm(PresDeVap ~ Temperatura,data = B2)

summary(regres.multp3)

regres.multp3$coefficients

plot(B2,pch=19,xlab="K", ylab="mm Hg", main="Diagrama de Dispersión",col="red")

plot(B2[,2],B2[,1]^(14))

p<-0

for (i in 1:11) {
  p[i]<-(i-1/2)/11
}

p

par(mfrow=c(2,2))

plot(sort(studres(regres.multp3)),p,pch=19,col="red",xlab = "Residuales Estudentizados", ylab="Probabilidad", main = "Gráfica de Probabilidad Normal")

plot(regres.multp3$fitted.values,studres(regres.multp3), pch=19, col="red", main="Residuales Estudentizados vs. Valores Ajustados", xlab="Valores ajustados", ylab = "Residuales Estudentizados")
abline(h=0,col="blue")

ts.plot(studres(regres.multp3),ylab="Residuales Estudentizados")
points(studres(regres.multp3), pch=19, col="red")

hist(studres(regres.multp3),xlab = "Residuales Estudentizados", col="red")

par(mfrow=c(1,1))

plot(B2[,1],log(B2[,2]),pch=19,col="red",xlab = "x", ylab = "y'=log(y)", main = "Diagrama de Dispersión")

plot(regres.multp3$fitted.values,studres(regres.multp3),col="red",pch=19)

B3<-data.frame(Temp=B2[,1]^(14),
               Pres=B2[,2]
)

B3<-data.frame(Temp=B2[,1],
               Pres=log(B2[,2])
)

regres.multp4<-lm(Temp~Pres,data = B3)

summary(regres.multp4)

plot(B3,pch=19,xlab="K", ylab="mm Hg", main="Diagrama de Dispersión",col="red")

plot(regres.multp4$fitted.values,regres.multp4$residuals,pch=19, col="red")

par(mfrow=c(2,2))

plot(sort(studres(regres.multp4)),p,pch=19,col="red",xlab = "Residuales Estudentizados", ylab="Probabilidad", main = "Gráfica de Probabilidad Normal")

plot(regres.multp4$fitted.values,studres(regres.multp4), pch=19, col="red", main="Residuales Estudentizados vs. Valores Ajustados", xlab="Valores ajustados", ylab = "Residuales Estudentizados")
abline(h=0,col="blue")

ts.plot(studres(regres.multp4),ylab="Residuales Estudentizados")
points(studres(regres.multp4), pch=19, col="red")

hist(studres(regres.multp4),xlab = "Residuales Estudentizados", col="red", main = "Histograma de los Residuales Estudentizados")

B4<-data.frame(
  Tempe=-1/B2[,1],
  Presi=ln(B2[,2])
)

plot(B4,ylab="y'=ln(y)",xlab="x'=-1/x",main="Diagrama de Dispersíon", pch=19, col="red")

regres.multp5<-lm(Presi~Tempe,data = B4)

summary(regres.multp5)

par(mfrow=c(1,1))

par(mfrow=c(2,2))

plot(sort(studres(regres.multp5)),p,pch=19,col="red",xlab = "Residuales Estudentizados", ylab="Probabilidad", main = "Gráfica de Probabilidad Normal")

plot(regres.multp5$fitted.values,studres(regres.multp5), pch=19, col="red", main="Residuales Estudentizados vs. Valores Ajustados", xlab="Valores ajustados", ylab = "Residuales Estudentizados")
abline(h=0,col="blue")

ts.plot(studres(regres.multp5),ylab="Residuales Estudentizados")
points(studres(regres.multp5), pch=19, col="red")

hist(studres(regres.multp5),xlab = "Residuales Estudentizados", col="grey")

plot(B4[,1],studres(regres.multp5))

shapiro.test(regres.multp5$residuals)

durbinWatsonTest(regres.multp3)

cochrane.orcutt(regres.multp5)
