#Todos los Modelos Posibles

regres.multp<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data = NFLTabla)

regres.multp<-lm(y~x2+x7+x8+x9,data = NFLTabla)

MODELOS<-ols_step_all_possible(regres.multp)

plot(c(MODELOS[17,2],MODELOS[46,2]),c(MODELOS[17,4],MODELOS[46,4]),pch=19,col="red",ylim = c(0,1.2),xlim = c(0,9))

text(c(MODELOS[17,2],MODELOS[46,2]),c(MODELOS[17,4],MODELOS[46,4]),labels=c(MODELOS[17,3],MODELOS[46,3]))

R0<-1-(1-0.8156)*(1+1.055)

plot(c(MODELOS[17,2],MODELOS[46,2]),c(MODELOS[17,7],MODELOS[46,7]),pch=19,col="red",ylim = c(0,25),xlim = c(0,9),xlab = "p",ylab = "Cp")

text(c(MODELOS[17,2],MODELOS[46,2]),c(MODELOS[17,7],MODELOS[46,7]),labels=c(MODELOS[17,3],MODELOS[46,3]))

abline(a=0,b=1)

write.csv(MODELOS,file="modelos.csv")

#Pasos

stepAIC(regres.multp)
summary(stepAIC(regres.multp))

147.898/3.350 - 28+2*3
