peso<-c(165,167,180,155,212,175,190,210,200,149,158,169,170,172,159,168,174,183,215,195,180,143,240,235,192,187)
bpsis<-c(130,133,150,128,151,146,150,140,148,125,133,135,150,153,128,132,149,158,150,163,156,124,170,165,160,159)

reg_lin<-lm(bpsis~peso)
reg_lin

Sxx<-0
for (i in 1:length(peso)) {
  Sxx<-Sxx+(peso[i]-mean(peso))^2
}

Sxy<-0
for (i in 1:length(peso)) {
  Sxy<-Sxy+bpsis[i]*(peso[i]-mean(peso))
}

Syy<-0
for (i in 1:length(peso)) {
  Syy<-Syy+(bpsis[i]-mean(bpsis))^2
}

SSt<-0
for (variable in 1:length(peso)) {
  SSt<-SSt+(bpsis[i])^2
}
SSt<-SSt-length(peso)*(mean(bpsis)^2)


b1<-Sxy/Sxx
b0<-mean(bpsis)-b1*mean(peso)

cor(peso,bpsis)
cor.test(peso,bpsis,method = "pearson")
r<-Sxy/sqrt(Sxx*SSt)

(r*sqrt(length(peso)-2))/sqrt(1-r^2)

0.551b1
b0