#GUARDAMOS LOS DATOS

salarios<-read.csv("Salarios.csv",header =  TRUE)

salarios                     

#hacemos la regresion con los datros guardados

rl<-lm(SAL~EDU+EXP+HAB+EDM+EDP+HER,data = salarios)

#Resumen de nuestro modelo de regresion

summary(rl)

#Intervalos de confianza de los coeficientes de la regresion

confint(rl,level = 0.94)

#Intervalos de confianza para la varianza

sum(rl$residuals^2)/qchisq(0.99,df=2171)

sum(rl$residuals^2)/qchisq(0.01,df=2171)

#Valor esperado con los datos 

sum(rl$coefficients*c(1,21,10,1.95,15,6,2))