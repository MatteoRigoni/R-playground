#simile a normale standard con code più grandi e regolata da gradi libertà
#converge a normale per gradi di libertà molto alti
plot(density(rt(100000,5)), xlim=c(-4,4))
lines(density(rt(100000,10)), col=2)
lines(density(rt(100000,30)), col=3)
lines(density(rt(100000,100)), col=4)

#funzione che identifica quantile della distribuzione fissata una certa probabilità
#livello significatività del 5% bilaterale, con 5 gradi libertà
#dato che la probabilità è l'area sotto la curva, all'aumentare di n, la cosa si assottiglia e il valore cala
qt(0.025,5) #valore soglia sinistro
qt(0.025,20)
qt(0.025,30)
qt(0.025,100)
qt(0.025,1000)
qnorm(0.025) #infatti questo "normale" è molto vicino a quello sopra con n eleevato



#verifica su 10 lanci di monete, ipoetsi sulle croci
#H0: P = 0.5
#H0: P != 0.5
campione_TC <- c("testa","testa","testa","croce","testa",
                 "croce","croce","testa","croce","testa")
campione_tc_d <- ifelse(campione_TC=="croce",1,0) #normalizzazione di vettore in numeri

P0 = 0.5
P_cap = mean(campione_tc_d)
T.stat <- (P_cap - P0) / sqrt( (P0*(1-P0)) / 10 )
qt(0.025,9) #valore soglia con n-1 gradi di libertà, bilaterale, ricade nella zona accettazione, non si può dire moneta truccata
#intervalli confidenza, il valore ipotesi nulla ricade ampiamente
0.4-(2.26 * sqrt( (0.5*(1-0.5))/10 )) ; 0.4+(2.26 * sqrt( (0.5*(1-0.5))/10 ))


#funzione nativa di R, per confronto tra media stimata nel campione e media campione riferimento
#un pò diverso da quello sopra, perchè funzione standard approssima a P stimato e non P0, cioè 0.4
t.test(campione_tc_d,
       mu = 0.5, # parametro ipotesi nulla
       conf.level = 0.95, # non alfa, ma 1-alfa
       alternative = "two.sided") 

moneta <- c(1,0)
lanci <- sample(moneta, 100, prob=c(0.2,0.8), replace=T) #"trucchiamo" la moneta
t.test(lanci, mu = 0.5) #conferma della moneta truccata



#1) confronto media campione di test (esperimenti controllati)
data("sleep") #extra=ore sonno, group=farmaco1/farmaco2, soggetti gli stessi sottoposti entrambi farmaci
colnames(sleep)[2] <- "drug" # per chiarezza
sleep

#boxplot condizionato per vedere distribuzione variabile extra rispetto farmaco 1 e 2
#visualizzazione efficace per vedere differenze variabile quantitva rispetto alle modalità di altre variabili
boxplot(extra~drug, data=sleep)
#statistiche di sintesi condizionate:
summary(sleep$extra[sleep$drug==1])
summary(sleep$extra[sleep$drug==2])

#differenza media != 0; inter. di conf. non contine 0, va da -2 a -0.70; t= -4 con p-value a 0.002 ovvero livello conf. > 99%
#rifiuteremo l'ipotesi 0 di uguaglianza tra i test, il farmaco 2 è più efficace dell'1

#codice originale non va!!!
#t.test(data=sleep,
#       extra~drug, 
#       paired = TRUE) 
t.test(sleep$extra[sleep$drug==1],
       sleep$extra[sleep$drug==2], 
       paired = TRUE) #due farmaci stessi pazienti, quindi campioni appaiati, non indipendenti


#2) confronto larghezza media tra gruppi indipendenti 
data("iris")
head(iris,5)
attach(iris)

#boxplot condizionati larghezza sepalo rispetto specie
boxplot(Sepal.Width~Species)

#in un colpo solo test multipli su gruppi diversi
#Il risultato di pairwise.t.test include una matrice dei p-value per ogni coppia di gruppi confrontati. 
#Il p-value misura la probabilità di ottenere un risultato almeno estremo come quello osservato, sotto l'ipotesi nulla che non ci siano differenze tra i gruppi (le medie sono uguali). 
#  Ecco cosa significano i p-value:
#  P-value basso (< 0.05 di solito): C'è una forte evidenza contro l'ipotesi nulla, suggerendo che esiste una differenza significativa tra i gruppi confrontati. Un p-value basso ti porta a rifiutare l'ipotesi nulla.
#  P-value alto (≥ 0.05): Non c'è sufficiente evidenza contro l'ipotesi nulla, quindi non si rifiuta. Questo non significa necessariamente che i gruppi siano uguali, ma che non ci sono prove sufficienti di una loro differenza statisticamente significativa.
#si ottengono p-value molto piccoli, minor 1%, pertanto le differenze in media tra i vari gruppi sono considerate significativamente != 0
pairwise.t.test(Sepal.Width, Species, 
                paired = FALSE, #dati indipendenti e non appaiati come sopra
                pool.sd = TRUE, #varianza pooled, media ponderata tra varienze gruppi
                p.adjust.method = "bonferroni") # funzione aggiustamento p-value


#test non parametrici, usato quando le assuznioni del T test non sono rispettate, es. la variazbile quantitativa non si distribuisce normalmente
data("InsectSprays") #numero "count" insetti uccisi durante espirimento, non si distribuisce in modo normale
boxplot(data=InsectSprays, count~spray)

#prima colonna di A rispetto agli altri, p-value vicino a zero, quindi efficacia diversa da C,D,E (in questo caso superiore come si vede da boxplot)
#si ha invece valore 1 con pari efficacia rispetto a B,F
pairwise.wilcox.test(InsectSprays$count,
                     InsectSprays$spray,
                     paired = FALSE, 
                     pool.sd = TRUE, 
                     p.adjust.method = "bonferroni") 
       

