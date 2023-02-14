x<-c(20:50)
curve(108+0.2*x,from = 20, to = 50, xlab="Temperatura de reacción (x1)", ylab = "Cantidad de conversión (Y)", type = "l",n=20,col="red",bg="red",pch=16, ylim=c(110,210))
curve(101+2.15*x,from = 20, to = 50, n=20, type = "l", pch=22, col="blue", add = TRUE)
legend(c(20,33), c(180,200), legend = c("1. Y=108+0.2x2","2. Y=101+2.15x1"), col=c("red","blue"), lty=1, cex=0.6)

curve(132+0.2*x,from = 20, to = 50, xlab="Temperatura de reacción (x1)", ylab = "Cantidad de conversión (Y)", type = "l",n=20,col="red",bg="red",pch=16, ylim=c(130,530))
curve(119+8.15*x,from = 20, to = 50, n=20, type = "l", pch=22, col="blue", add = TRUE)
legend(c(20,33), c(420,500), legend = c("1. Y=132+0.2x2","2. Y=119+8.15x1"), col=c("red","blue"), lty=1, cex=0.6)