QI = rnorm(100000, mean=100, sd=15) # quoziente intellettivo ha media 100 e variazione 15, normale standard
plot(density(QI))

dati <- read.csv("dati QI.csv") # quoziente intellettivo di studenti in tre classi
View(dati)

#mu0 = parametro da saggiare sotto ipotesi nulla
#alfa = livello significatività a priori
#bilaterale
z_test <- function(x, mu0, stdev, alfa) {
  x= na.omit(x) # toglie dati mancanti per classe
  mu_cap = mean(x) # media stimata
  n = length(x)
  
  Z = (mu_cap-mu0) / (stdev / sqrt(n))
  #quantili della distribuzione soglia, che indicano zona di rifiuto:
  valori.soglia = qnorm(c(alfa/2, 1-alfa/2))
  
  CI = c(mu_cap-qnorm(alfa/2)*(stdev/sqrt(n)),
         mu_cap+qnorm(alfa/2)*(stdev/sqrt(n)))
  
  return (
    list(
      media.campionaria = mu_cap,
      stat.test = Z,
      valori.soglia = valori.soglia,
      pvalue = 2*pnorm(-abs(Z)),
      Int.Conf = CI,
      
      #rappresentazione grafica delle soglie su normale standard
      grafico = plot(density(rnorm(100000, 0, 1))),
      abline(v=valori.soglia, col=2)
    )
  )
}

#sistema ipotesi bilaterale
#H0: mu = 100
#H0: mu != 100

# 1)stat test ricade nella zona di accettazione
# 2)pvalue 0.12 > livello significatività 5% 
#   (alto indica che le osservazioni non sono statisticamente significative e possono essere spigate da ipotesi nulla)
# 3)intervallo confidenza include valore dell'ipotesi nulla
#   quindi se si raccolgono nuovi dati tante volte, il 95% degli IC calcolati include il vero valore del parametro di interesse
# quindi NON rifiutiamo l'ipotesi nulla, la classe non ha nulla di particolare rispetto alla media
z_test(dati$c1, 100, 15, 0.05)
points(x=1.55,y=0,pch=20,col=4,cex=3) 

# 1)stat test ricade nella zona di accettazione
# 2)pvalue 0.77 > livello significatività (siamo molto sicuri che ipotesi H0 è vera, visto che max è 1)
# 3)intervallo confidenza include valore dell'ipotesi nulla H0
# quindi NON rifiutiamo l'ipotesi nulla, la classe non ha nulla di particolare rispetto alla media
z_test(dati$c2, 100, 15, 0.05)
points(x=0.28,y=0,pch=20,col=4,cex=3) 

# 1)stat test non ricade nella zona di accettazione
# 2)pvalue 0.02 > livello significatività (siamo molto sicuri visto che 0 il max di certezza di differenza osservata)
# 3)intervallo confidenza non contiene ipotesi H0
# quindi rifiutiamo l'ipotesi nulla, in quanto la classe ha una media che si discosta molto
z_test(dati$c3, 100, 15, 0.05)
points(x=-2.26,y=0,pch=20,col=4,cex=3) 


library(TeachingDemos) 
z.test(na.omit(dati$c3),
       100,
       stdev=15,
       alternative="two.sided", # per gestire test unilaterale o non
       conf.level = 0.95) # non livello significatività ma confidenza, quindi 1 - alfa