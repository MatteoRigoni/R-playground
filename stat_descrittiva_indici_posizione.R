# caricamento dataset predefinito
data("iris")
head(iris,3)
levels(iris$Species)

n <- length(Petal.Length)

# moda (solo per variabili qualitative): modalità con frequenza assoluta più alta
# in questo caso distribuzione trimodale, se era 50,50,30 ad esempio sarebbe stata bimodale
attach(iris) # attach/detach: per usare direttamente il dataframe
table(Species) 
detach(iris)

# minimo e massimo
sort(Petal.Length)[1]
sort(Petal.Length)[n]

min(Petal.Length)
max(Petal.Length)

# mediana (taglia in due la distribuzione ordinata dei dati)
# se il numero di elementi è pari si considerano i due centrali
n/2 
sort(Petal.Length)[c(75,76)]

median(Petal.Length)

# quartili: 1/4 (min), 2/4(mediana), 3/4, 4/4 (max)
# esprime la suddivisone della distribuzione in 4 blocchi o n
n/4
sort(Petal.Length)[38]
n*3/4
sort(Petal.Length)[113]

quantile(Petal.Length)
quantile(Petal.Length, seq(0,1,0.1)) # decili
quantile(Petal.Length, seq(0,1,00.000.1)) # percentili

#media
sum(Petal.Length)/n

mean(Petal.Length)

#indici posizione in classi
#classe modale = maggior numeo unità statistiche, quindi max(ni)
#quartili = il primo è la classe la cui frequenza relativa cumulata (Fi) > 25
#mediana = la prima classe con Fi > 50%
Petal.Length_CL <- cut(Petal.Length, seq(0,7,1))
table(Petal.Length_CL)

distr_freq <- as.data.frame(
  cbind(
    ni = table(Petal.Length_CL),
    fi = table(Petal.Length_CL)/n,
    Ni=cumsum(table(Petal.Length_CL)),
    Fi=cumsum(table(Petal.Length_CL)/n)
  )
)

quantile(Petal.Length) #con distribuzione in calssi c'è perdita di precisione rispetto al dato grezzo

#media ponderata in classi
#sommatoria(valori centrali classi * frequenza assoluta) / sommatoria(pesi)
distr_freq$cXi <- seq(0.5,6.5,1)
sum(distr_freq$cXi * distr_freq$ni)/sum(distr_freq$ni)

weighted.mean(distr_freq$cXi, distr_freq$ni)

#media geometrica (per incrementi in percentuale nel tempo)
#esempio di conteggio cellule nel corso dei giorni
cellule <- c(1000,1800,2100,3000,5000)
incrementi_perc <- quantmod::Delt(cellule)*100
incrementi_perc <- incrementi_perc[-1,1]

geometric_mean <- function(x) {
  return (prod(x)^(1/length(x)))
}

geometric_mean(incrementi_perc)
mean(incrementi_perc)

#media armonica (per rapporti e velocità)
#esempio calcolo media su più tratti di strada a velocità diverse
#reciproco media aritmetica dei reciproci di tutti i dati 
speed <- c(100,80,40,90)

armonic_mean <- function(x) {
  return (1 / (sum(1/x)/length(x)))
}

armonic_mean(speed)
mean(speed)




