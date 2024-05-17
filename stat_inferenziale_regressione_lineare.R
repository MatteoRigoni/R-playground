dati <- read.csv("energy output.csv") # produzione energia giornaliera di una centrale elettrica
summary(dati)
attach(dati)
plot(Temperature, Energy.output,pch=20) #atteso coefficiente relazione vicino a 1
cor(Temperature, Energy.output) #all'aumentare di una cala l'altra

#variabile risposta = energy output
# Y = b0 + b1*x + e
# energy.output = b0 + b1 * Temperature + e (dove b0 è l'intercetta sulle y)
#si usano i minimi quadrati per il calcolo
b1 = cov(Energy.output, Temperature) / var(Temperature)
b0 = mean(Energy.output) - b1*mean(Temperature)
b0;b1

#...facendolo con R:
#decremento stasticamente rilevante, con p-value = 0, quindi contro ipotesi nulla di indipendeza
mod_lin <- lm(Energy.output~Temperature)
mod_lin$coefficients
abline(mod_lin, col=2) # retta d regressione
summary(mod_lin) 

#valutazine bontà modello, è buono se la retta passa in mezzo
#R-quadro è nel summary sopra, 0.887, oppure:
cor(Energy.output, Temperature)^2
#previsione
predict(mod_lin, newdata = data.frame(Temperature=40))

#determinazione della parte erratica sui residui, cioè le distanze dalla retta
#residui sparsi attorno alla media di zero.
#la curva assomiglia alla normale con curca però a sinistra, dovuti a valore "diversi" dalla media
par(mfrow=c(1,2))
plot(residuals(mod_lin))
abline(h=mean(residuals(mod_lin)),col=2)
plot(density(residuals(mod_lin)))

#saggiamo ipotesi che la distribuzione è uguale alla normale
shapiro.test(residuals(mod_lin)) #p-value molto piccolo, quindi si rifiuta l'ipotesi di normalità

library(lmtest) #soglia significatività del 5%
bptest(mod_lin) #ipotesi di varianza costante
dwtest(mod_lin) #ipotesi di non correlazione dei residui