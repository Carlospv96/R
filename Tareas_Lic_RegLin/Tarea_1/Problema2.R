x<-c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
y<-c(1,5,4,7,10,8,9,13,14,13,18)
reg_lin<-lm(y~x)
reg_lin
cor(x,y)
cor.test(x,y,method = "pearson")
plot(x,y)

confint(reg_lin,level = 0.95)

Sxx<-0
for (i in 1:length(x)) {
  Sxx<-Sxx+(x[i]-mean(x))^2
}

Sxy<-0
for (i in 1:length(x)) {
  Sxy<-Sxy+y[i]*(x[i]-mean(x))
}

b1<-Sxy/Sxx
b0<-mean(y)-b1*mean(x)

b1
b0

plot(x,y)
plot(x,b0+b1*x,type = "l")
points(x,y,col="red")
