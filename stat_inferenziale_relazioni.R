dati <- read.csv("energy output.csv")
attach(dati)

#scatterplot, punta un pò verso il basso, quindi discordanti (quanto uno aumenta, l'altro diminuisce)
plot(Temperature,Pressure, pch=20)
summary(Temperature)
summary(Pressure)

# valorenegativo, quindi discordanza, ma non sappiamo ordine di grandezza
n <- length((Temperature))
covarianza <- sum((Temperature-mean(Temperature)) *
                  (Pressure-mean(Pressure))) / (n-1)

#...covarianza in R
#leggermente diverso, perchè R di default effettua calcolo usando stimatore non distorto della varianza, usando n-1
cov(Temperature, Pressure)

#sempre negativa, da 0 a 1
pearson.rho <- cov(Temperature,Pressure) / (sd(Temperature) * sd
  (Pressure))

#...coefficiente pearson in R
cor(Temperature, Pressure)


#altro esempio con plausibile correlazione
plot(Temperature, Vacuum, pch=20)
cov(Temperature, Vacuum)
cor(Temperature, Vacuum)



# correlazioni NON PARAMETRICHE
dati <- read.csv("gare.csv",sep=";")
View(dati)

#verifica disaccordo 100m e maratona, siccome si parla di ordimaneto si usa indice Spearman (esempio voti scuola)
dati$pos.100m <- rank(dati$m100)
dati$pos.maratona <- rank(dati$Maratona)
dati$diff.rank <-dati$pos.100m - dati$pos.maratona

n <- nrow(dati)
#leggera cograduazione negativa, quindi chi va forte 100m non è detto che vadano forte anche maratona, anzi si stima cosa opposta
rho.spearman <- 1-6 *(sum(dati$diff.rank^2)) / (n*(n^2-1))
plot(dati$m100,dati$Maratona) # non si capisce molto da qui
plot(dati$pos.100m,dati$pos.maratona)  # anche qui la nuvola dei punti non è chiara perchè la cograduzione è debole
#altro modo per calcolare in R la funzione spearman
cor(dati$m100,dati$Maratona, method="spearman")
cor(dati$m100,dati$Maratona, method="kendall") 

#x^2 di pearson
tabella <- matrix(data = c(45,22,32,12,
                           53,24,21,30,
                           24,65,40,3,
                           12,43,2,2,
                           20,13,7,0),
                  nrow=5,
                  ncol=4,
                  byrow=T)

colnames(tabella) <- c("0","1","2","3+")
row.names(tabella) <- c("analfabeta","elementare","media","superiore","universitaria")
sum(tabella) #numero totale famiglie analizzate su due variabili
#visione delle variabili assolute, in caso di indipendenza totale nel grafico ci saranno pallini tutti uguali
ggpubr::ggballoonplot(data=as.data.frame(tabella),
                      fill="blue")
attese <- tabella
margin.table(tabella,1) #totale di riga
margin.table(tabella,2) #totale di colonna
n=margin.table(tabella) #470

#frequenza attesa del primo valore in altro a sinistra, data dal totale riga * totale colonna / totale elementi

for(i in 1:nrow(tabella)){
  for(j in 1:ncol(tabella)){
    attese [i,j] <- (margin.table(tabella, 1)[i] * margin.table(tabella, 2)[j] / n)
  }
}

osservate <- tabella
x_quadro <- sum((osservate-attese)^2/attese)

#x quadro si distribuisce come chi quadrato con libertà = col-1 * row-1 in questo caso 12
plot(density(rchisq(1000000, 12)), xlim=c(0,130))
#determinaiamo valore sogli aper un certo livello significativa es 1%
abline(v=qchisq(0.99, 12),col=2)
#valutiamo statistica test calcolata
points(x_quadro, 0,cex=3,col=4,pch=20) #oltre valore soglia, quindi zona rifiuto, si rifiuta ipotesi nulla di indipendenza, c'è associazione statistica

#...esempio usando funzione base di R
test.indipendenza <- chisq.test(tabella)
#p-value praticamente 0 quindi siamo sicuri dell'esito del test
#gradi di libertà = df = 12

test.indipendenza$expected #con $ si ottengono stesse variabili calcolate sopra a mano


#stima sesso sul colore degli occhi
dati <- HairEyeColor[1,,]
ggpubr::ggballoonplot(data=as.data.frame(dati),
                      fill="blue")
#si vede che alcuni colori sono meno osservati in base dimensione baloon
#invece non si nota differenza tra uomo e donna, quindi suggerisce indipendenza
#verifichiamo col chi test
chisq.test(dati) #2.16 fa rif. a distribuzione con 3 gradi di libertà, con p-value di 0.5, quindi non si rifiuta ipotesi indipendenza



