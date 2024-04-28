#allontanamento da una distribuzione normale

x <- rnorm(100000,10,2)
plot(density(x))

#indice asimmetrica Fisher, detto momento terzo distribuzione
#prossimo allo zero per distribuzione normali
mu <- mean(x)
sigma <- sd(x)
n <- length(x)

m3 <- sum((x-mu)^3)/n
asim_index = m3 / sigma^3
abline(v=mu,col="red")

#curtosi, detto momento quarto distribuzione
#definisce allungamento o schiacciamento distribuzione rispetto campana normale
#prossimo allo zero per distribuzione normali, "mesocurtico"
m4 <- sum((x-mu)^4)/n
kurtosi_index = m4 / sigma^4 - 3 #3 è il valore sulla normale e va sottratto

#indici forma nativi R
library(moments)
skewness(x)
kurtosis(x)-3

#esempio pratico (non normale)
library(ggplot2)
data("diamonds")
attach(diamonds)

summary(price)
ggplot()+
  geom_density(aes(x=price), col="black",fill="lightblue")

View(table(price)) # per vedere moda su valori continui

skewness(price) #asimmetrica positiva, picco a sinistra, quindi media>mediana>moda
kurtosis(price)-3 #leptocurtica, cioè appuntita

#pacchetto gghalves
#boxplot+densita probabilita, quindi indici posizione, indici variabilita e forma
library(gghalves)

ggplot(data=diamonds)+
  geom_half_boxplot(aes(x=cut,y=price),
                     side="l", fill="pink") +
  geom_half_violin(aes(x=cut,y=price),
                   side="r",fill="lightblue")