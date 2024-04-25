# lettura CSV
dati <- read.csv("tonni.csv",sep=";")
N <- dim(dati)[1]

# frequenze assolute e relative su variabile qualitativa, scala nominale
freq_ass <- table(dati["SESSO"])
freq_rel <- table(dati["SESSO"])/N
distr_freq_SESSO <- cbind(freq_ass, freq_rel)

freq_ass <- table(dati["LOCALITA"])
freq_rel <- table(dati["LOCALITA"])/N
distr_freq_LOCALITA <- cbind(freq_ass, freq_rel)

# suddivisione in classi
min(dati$LUNGHEZZA)
max(dati$LUNGHEZZA)
dati$LUNGHEZZA_CL <- cut(dati$LUNGHEZZA, breaks = c(10,15,20,25,30))

# distribuzione completa su classi di lunghezza
ni <- table(dati$LUNGHEZZA_CL)
fi <- table(dati$LUNGHEZZA_CL)/N
Ni <- cumsum(ni)
Fi <- Ni/N

distr_freq_lungh_cl <- as.data.frame(cbind(ni, fi, Ni, Fi))
write.csv(distr_freq_lungh_cl, "distribuzione frequenza tonni.csv")

# distribuzione di frequenze doppie
table(dati$SESSO, dati$LUNGHEZZA_CL)
table(dati$LOCALITA, dati$LUNGHEZZA_CL)/N

# grafico a torta (R)
etichette <- paste(rownames(distr_freq_lungh_cl),
                   distr_freq_lungh_cl$fi*100,
                   "%")

colori <- c("pink", "pink2","pink3", "pink4")

pie(distr_freq_lungh_cl$fi,
    main="Distribuzione delle classi di lunghezza",
    labels = etichette,
    col = colori)

# grafico a barre (R)
barplot(distr_freq_lungh_cl$ni,
        main="Distribuzione delle classi di lunghezza",
        xlab = "Classi di lunghezza, cm",
        ylab = "Frequenze assolute",
        ylim = c(0,10),
        col = "blue",
        names.arg = rownames(distr_freq_lungh_cl))


# grafico a barre con ggplot
library(ggplot2)
ggplot(data = dati) +
  geom_bar(aes(x=LUNGHEZZA_CL,
               fill=SESSO),
           position = "dodge", # dodge/stack/fill
           stat = "count",
           col = "black") +
  labs(title="Distribuzione delle classi di lunghezza",
       main="Distribuzione delle classi di lunghezza",
       x = "Classi di lunghezza, cm",
       y = "Frequenze assolute") +
  scale_y_continuous(breaks = seq(0,8,1)) +
  theme_classic() +
  theme(legend.position = "bottom")