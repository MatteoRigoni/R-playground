  dati <- read.csv("energy output.csv")
  attach(dati)
  n <- nrow(dati)
  
  #relazione tra generazione di energi e varie variabili esplicative
  
  #per iniziare vediamo che la variabile risposta sia circa normale, vedendo indici forma e shapiro
  #se si discosta dalla normalità, cade anche sui residui
  moments::skewness(Energy.output) #leggermente possitiva, ma simmetrica
  moments::kurtossi(Energy.output)-3
  shapiro.test(Energy.output) #vediamo se non rifiuta ipotesi normalità, 0.08, non si rifiuta ipotesi con alfa fissato al 5%
  
  #matrice correlazione due a due (per natura simmetrica)
  round(cor(dati),2)
  #variabili con alta correlazione sulla risposta saranno più utili, invece i regressori molto legati posso dare problemi multicorrelatività
  ?pairs
  
  #matrice correlazione in maniera grafica
  #ultima riga correzione risposta con le altre, quindi possiamo vedere quelle più correlate 
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
  {
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
  }
  pairs(dati, upper.panel = panel.smooth, lower.panel = panel.cor)
  
  #stima primo modello (tilde indica che dipende da)
  mod1 <- lm(Energy.output ~ Temperature + Vacuum + Pressure + Humidity,
             data = dati)
  summary(mod1)
  #si vede tabella coefficienti...
  #temperatura effetto negativo su produzione energia, per ogni grado temp, -2.1Mw in meno
  #pressure, coefficiente basso e p-value alto, alto indica che non abbiamo prove per dire che c'è legame significativo
  #R quadro aggiustato molto buono
  
  #stepwise, per testare modelli diversi......
  mod2 <- update(mod1, ~.-Pressure)
  summary(mod2)
  #quasi inalterato perchè Pressure era inutile. Abbiamo ancora significatività (p-value) mantenuta, R quadro fisso
  #ora si valuta effetto quadratico che si era visto dal grafico, in quanto sembrava non lineare
  plot(Vacuum, Energy.output,pch=20)
  mod3 <- update(mod2, ~.+I(Vacuum^2))
  summary(mod3)
  #curva rilevata dal modello, infatti la stima di vacuum è ora -1.19, con p valore sotto al 5%, quindi significativa
  #effetto quadratico I() indica stima posistiva, in quanto effetto negativo di 1° grado si riduce, infatti curva si va appiattendo
  #  p valore al limite, in quanto la curvatura è appena accennata
  #R quadro aggiuntato è un simile al precedente quindi no c'è contributo rilevante con Vacuum!
  #modelli più semplici, a parità di performance sono da preferire, quindi togliamo Vacuum
  mod4 <- lm(Energy.output~Temperature+Humidity)
  summary(mod4)
  #quindi solo due variabili esplicative, entrambe molto significative, con R quadro rimasto quasi uguale
  
  #ora potremmo pensare che le variaibli hanno anche effetto congiunto, verifichiamo effetto interazione..
  mod5 <- lm(Energy.output~Temperature*Humidity)
  summary(mod5)
  #ha rotto umidità, che non è più significativa e R quadrato non è migliorato , quindi escludiamo questo modello
  
  #analisi varianza ANOVA, altro test che permetet di capire se l'aggiunta di una variabile è bene o no..
  anova(mod5, mod4)
  
  #criterio AIC e BIC per fare altre valutazioni..
  #vanno confrontati con altri modelli, quelli più bassi, di solito sono i migliori
  AIC(mod1,mod2,mod3,mod4,mod5) #tende a preferire modelli con più parametri
  BIC(mod1,mod2,mod3,mod4,mod5)
  
  #verifichiamo anche che non ci sia multi-collinearità
  library(car)
  vif(mod4) #sotto al 5, quindi non indicano problemi
  
  
  
  #...in R si può automatizzare stepwise, partendo dal modello pieno
  stepwise.mod <-MASS::stepAIC(mod1,
                  direction="both",
                  k=2) # criterio AIC, oppure log(n) per BIC
  summary(stepwise.mod) #corrisponde al modello 4 calcolato sopra
  
  
  #analisi degli errori del modello, calcolo dei residui e verifica valori anomali
  # 1°: punto ABBASTANZA vicini a zero, ma un pò ricurso, come se l'informazione non è sata filtrrata dai regressori e è finita nei residui
  # 2°: punto vicini alla bisettrice, quindi distribuzione normale dei residui
  # 3°: non si devono vedere pattern, ma varianza costanza, anche cui curva piccola
  # 4°: potenziali valori influenti outliers(risposta) o leverages(regressori). 0.5 warning, 1 di allarme
  par(mfrow=c(2,2))
  plot(mod4)
  
  #calcolo esatto leverage (6)
  lev <- hatvalues(mod4)
  plot(lev)
  p=sum(lev)
  soglia = 2*p/n
  
  lev[lev>soglia]
  
  #calcolo esatto outliers (1)
  plot(rstudent(mod4))
  abline(h=c(-2,2),col=2)
  outlierTest(mod4) #applica approssimazione Bonferroni
  abline(h=soglia,col=2)
  
  #distanza cook tra leverages e outliers
  cook <- cooks.distance(mod4)
  plot(cook)
  
  
  #test sui residui
  library(lmtest)
  bptest(mod4) # omoschedasticità, non è rifiutata ipotesi nulla, quindi varianza costante
  dwtest(mod4) # anche qui non si rifiuta ipotesi nulla, quindi non sono auto-correlati
  shapiro.test(residuals(mod4)) #qui si rifiuta ipotesi nulla normalità, per via di alcune osservazioni lontane
  plot(density(residuals(mod4)))
  
  scatter3d(Energy.output~Temperature+Humidity)
  
  
  
  #------------------------------------------
  dati <- read.csv("stopsmoking150.csv",sep=";",stringsAsFactors=T)
  attach(dati)
  summary(dati)
  n<- nrow(dati)
  
  #p-value 0.39 non si rifiuta ipotesi di normalità, quindi il modello regressione lineare è congruo
  shapiro.test(peso)
  
  #matrice correlazione 2 a 2 con funzione pairs (peso è variabile risposta)
  #correlazione peso-altezza pari a 0.78, vicino a 1, molto correlate (anche lo scatterplot)
  #correlazione peso-età vicino a zero con nuvola punti orizzontale
  #mesistop mostra relatizione quasi nulla di 0.22, col passare dei mesi cala un pò il peso
  #sulle variabili qualitative perde significato scatterplot e correlazione...
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
  {
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
  }
  pairs(dati, upper.panel = panel.smooth, lower.panel = panel.cor)
  
  #per le qualitative meglio usare i bloxplot
  par(mfrow=c(1,2))
  boxplot(peso)
  boxplot(peso~sesso) #evidente differenza tra maschio e femmina
  mean(peso[sesso=="M"])
  mean(peso[sesso=="F"])
  t.test(peso~sesso) #si saggia ipotesi di uguaglianza tra media in gruppi indipendenti, con p-value piccolissimo si rifiuta ipotesi nulla, quindi sono diverse
  
  #si fa lo stesso su sport
  par(mfrow=c(1,2))
  boxplot(sport)
  boxplot(peso~sport)
  t.test(peso~sport)
  
  #primo modello regressione multipla
  mod1 <- lm(peso ~ ., data=dati) #partiamo da tutte
  summary(mod1)
  #vediamo tabella coefficienti...
  #altezza beta positivo e significativo (pvalue bassissimo)
  #età e reddito non significative non le consideriamo
  #mesistop 1.4, quidi ogni mese perde 1.4
  #le varibili qualitative sono codificate come dummy:
  # confronto rispetto baseline, quindi tenendo fisse le altre variabili, nei maschi si calcolano 8kg in piu alle femmine
  
  #togliamo reddito, lasciamo età che è variabile di "controllo" come il sesso
  mod2 <- update(mod1, ~.-reddito)
  summary(mod2)
  #rispetto al precedente le stime significative sono rimaste tali e l'R quadro aggiustato è rimasto praticamente uguale 
  anova(mod2,mod1) # ulteriore validazione..aggiungendo reddito non si aggiunge abbastanza varianza rilevante, non significativo
  BIC(mod2, mod1) #anche da qui meglio il 2 più basso
  car::vif(mod2) #tutto sotto 5, non c'è rischio di multicollinearità
  
  mod3 <- update(mod2, ~.-eta)
  summary(mod3)
  BIC(mod1, mod2, mod3) #il 3 sembra migliore, ma meglio tenere variabile controllo età in quanto è importante nel contesto
  
  #esempio errato, se togliessimo il sesso:
  mod4 <- update(mod3, ~.-sesso)
  summary(mod4) #Rquadro perde parecchio
  BIC(mod1, mod2, mod3, mod4) #mod4 valore più alto, peggiore!
  
  #esaminiamo i residui
  par(mfrow=c(2,2))
  plot(mod2)
  #1)i residui sono attorno media zero senza pattern, casuali, ok
  #2)tutti su bisettrice grafico, quindi distribuzione normale
  #3)varianza sembra costante
  #4)nessuno supera variabile coook
  #tramite test statistici:
  shapiro.test(residuals(mod2)) # 0.70, non si rifiuta ipotesi di normalità
  lmtest::bptest(mod2) #non si rifiuta ipotesi omoschedasticità (ovvero variabilità rimane la stessa, indipendentemente del valore predetto)
  lmtest:dwtest(mod2) #anche qui pvalue alto, quindi non si rifiuta ipotesi di incorrelazione
  
  #calcolo esatto leverage, ne vengono fuori 10
  lev <- hatvalues(mod2)
  plot(lev)
  p=sum(lev)
  soglia = 2*p/n
  abline(h=soglia,col=2)
  lev[lev>soglia]
  
  #calcolo esatto outliers , solo valore 84 dà dei problemi, ma non evidenzito da cook quindi ok
  plot(rstudent(mod2))
  abline(h=c(-2,2),col=2)
  outlierTest(mod2) #applica approssimazione Bonferroni
  abline(h=soglia,col=2)
  
  #distanza cook tra leverages e outliers
  cook <- cooks.distance(mod2)
  plot(cook)
  
  
  #grafico
  library(ggplot2)
  ggplot(data=dati)+
    geom_point(aes(x=mesistop,
                   y=peso,
                   col=sesso), position="jitter")+ #rende più leggibile valore x simulandolo più continuo temporale
    geom_smooth(aes(x=mesistop,
                    y=peso,
                    col=sesso), se=F, method="lm")