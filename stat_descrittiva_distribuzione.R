#distribuzione normale standard
normale_stadard = rnorm(1000000,0,1)
hist(normale_stadard, freq=FALSE, breaks=100)
lines(density(normale_stadard),col="red",lwd=3)

plot(density(normale_stadard),xlim=c(-4,12))

#altri esempi distribuzione normale
normale_5_2 <- rnorm(1000000,5,2)
lines(density(normale_5_2),col="green",lwd=3)

normale_5_3 <- rnorm(1000000,5,3)
lines(density(normale_5_3),col="blue",lwd=3)

#standardizzazione
# (x-mu)/sigma
Z <- (normale_5_3-mean(normale_5_3)/sd(normale_5_3))
lines(density(Z), col="orange")

#esempio popolazione con altezza media 170 e deviazioe standrd 10
#probabilita <= 170 è 50%
mu=170
sigma=10
n=100000

altezza <- rnorm(n,mu,sigma)
plot(density(altezza))
abline(v=mu,col="red")
pnorm(155,mu,sigma) #area sottesa curva da -infinito a 155cm

Z_altezza <- (altezza-mu)/sigma
plot(density(Z_altezza))
Z_altezza_155 <- (155-mu)/sigma
abline(v=Z_altezza_155, col="blue")
pnorm(Z_altezza_155,0,1) #area calcolata con normalizzazione standard, stesso valore