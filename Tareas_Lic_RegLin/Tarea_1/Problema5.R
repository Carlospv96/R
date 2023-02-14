vol<-c(2084,2084,2273,2273,2273,2463,2463,2651,2652,2652,2842,2842,3030,3031,3031,3221,3221,3409,3410,3600,3600,3788,3789,3789,3979,3979,4167,4168,4168,4358,4358,4546,4547)
pres<-c(4599,4600,5044,5043,5044,5488,5487,5931,5932,5932,6380,6380,6818,6817,6818,7266,7268,7709,7710,8156,8156,8597,8599,8600,9048,9048,9484,9487,9487,9938,9938,10377,10379)

reg_lin<-lm(pres~vol)
reg_lin$coefficients

Sxx<-0
for (i in 1:length(vol)) {
  Sxx<-Sxx+(vol[i]-mean(vol))^2
}

Sxy<-0
for (i in 1:length(vol)) {
  Sxy<-Sxy+pres[i]*(vol[i]-mean(vol))
}

b1<-Sxy/Sxx
b1
b0<-mean(pres)-b1*mean(vol)
b0
reg_lin<-lm(pres~vol)
reg_lin

plot(vol,b0+b1*vol,type = "l",col="red",xlab = "Volumen", ylab = "Presión")
points(vol,pres)
