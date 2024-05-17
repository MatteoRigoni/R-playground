set.seed(1) # per avere sempre stessi numeri casuali in base al seed
reddito <- rchisq(100000,30) # simile a rnorm, valori random su distribuzione asimmetrica
plot(density(reddito))

# indice fisher, maggiore di zero, quindi con la "pancia" a sinistra, valori più frequenti in basso
moments::skewness(reddito) 

mu_vero <- mean(reddito)
sigma_vero <- sd(reddito)
mediana_vero <- median(reddito)
abline(v=mu_vero,col=2)

# stima della media su un campione della popolazione
campione <- sample(reddito,30)
mean(campione)
abline(v=mean(campione)) # risulterà sottostima/sovrastimata rispetto al vero valore

#verifica stimatori effettuando calcolo su più campioni
#aumento numero campioni/elementi campione: 
#  - la forma va verso la "normale", per teorema limite centrale:
#    le medie di distribuzioni non normali, converge verso una normale
#  - le medie di media/deviazione si avvicinano a quella vera e standard error cala  
# PREMESSA: media e deviazione sono stimatori non distorti, la mediana è distorta
medie <- c(NA)
dev.std <- c(NA)
mediane <- c(NA)
N.campioni <- 150 #numero di campioni da estratte dalla popolazione
n <- 15 # numerosità campionaria, quindi elementi per campione

set.seed(2)
for (i in 1:N.campioni) {
  campione <- sample(reddito, n)
  medie[i] <- mean(campione)
  mediane[i] <- median(campione)
  dev.std[i] <- sd(campione)
}
plot(density(medie))

# verifica correttezza e non distorsione degli stimatori: 
#   molto vicini, tranne mediana che è stimatore distorto
mean(medie); mu_vero
mean(dev.std); sigma_vero
mean(mediane); mediana_vero 

# standard error medio, più è basso, più lo stimatore è preciso. Cala all'aumentare di n
std.err <- sd(medie)/n