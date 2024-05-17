#1) Importa il dataset “neonati.csv” e controlla che sia stato letto correttamente dal software
dati <- read.csv("neonati.csv", sep=",", stringsAsFactors = T)
head(dati, 10)
n <- nrow(dati)
attach(dati)

dati$Fumatrice <- dati$Fumatrici == 1

summary(dati)
str(dati)

library(psych)
library(kableExtra)

all_indici <- describe(dati, IQR=TRUE,quant=c(.25,.75),omit=TRUE,type=1)

indici <- all_indici[c("min", "Q0.25", "mean", "median", "Q0.75", "max", "sd", "IQR", "skew", "kurtosis")]
indici_2_decimali <- round(indici, 2)

kable(indici_2_decimali, "markdown") %>%
  kable_styling(full_width = FALSE)%>%
  column_spec(1, border_right = TRUE)

# Calcola le frequenze di ogni valore



frequenze.N.gravidanze <- table(dati$N.gravidanze)
par(mfrow=c(2,2))
plot(frequenze.N.gravidanze, col = 2, main = "Numero di occorrenze gravidanze", xlab = "N. gravidanze")
boxplot(dati$N.gravidanze, main="Boxplot gravidanze pregresse", ylab="N. gravidanze")

table(dati$N.gravidanze)
barplot(table(dati$N.gravidanze), main="N. gravidanze per madre", xlab="Numero di gravidanze", ylab="Numero occorrenze")

table(dati$Fumatrici)
pie(table(dati$Fumatrici), main="Fumatrici / Non Fumatrici", xlab="Fumatrice (0=No, 1=Sì)", ylab="Frequenza")


summary(neonati$Gestazione)
hist(neonati$Gestazione, main="Distribuzione delle settimane di gestazione", xlab="Settimane")
boxplot(neonati$Gestazione, main="Boxplot delle settimane di gestazione", ylab="Settimane")

# Calcola il numero di occorrenze per ogni settimana di gestazione
gestazione_freq <- table(dati$Gestazione)

par(mfrow=c(2,2))
plot(table(dati$Gestazione))
boxplot(dati$Gestazione, main="Boxplot gestazione", ylab="N. sett. gestazione")

# Crea il line chart
ggplot(as.data.frame(gestazione_freq), aes(x = Var1, y = Freq)) +
  geom_line() +
  labs(title = "Frequenza delle Settimane di Gestazione",
       x = "Settimane di Gestazione",
       y = "Numero di Occorrenze") +
  theme_minimal()



summary(neonati$Peso)
hist(dati$Peso, main="Distribuzione del peso dei neonati", xlab="Peso (grammi)")
boxplot(dati$Peso, main="Boxplot del peso dei neonati", ylab="Peso (grammi)")



ggplot(dati, aes(x = Tipo.parto, y = Peso)) +
  geom_boxplot() +
  labs(title = "Distribuzione del Peso dei Neonati per Tipo di Parto",
       x = "Tipo di Parto",
       y = "Peso (grammi)") +
  theme_minimal()



ggplot(dati, aes(x = Gestazione, y = Peso)) +
  geom_point() +  
  geom_smooth(method = "lm", se = TRUE, color = 2) +  # si renderizza anche la linea di regressioene
  labs(title = "Relazione tra Peso del Neonato e il numero  di Settimane di Gestazione",
       x = "Settimane di Gestazione",
       y = "Peso (grammi)") +
  theme_minimal()





ggplot(dati, aes(x = as.factor(Fumatrici), y = Peso)) +
  geom_boxplot() +
  labs(title = "Rilevanza del Fumo sul Peso dei Neonati",
       x = "Madre Fumatrice (0=No, 1=Sì)",
       y = "Peso (grammi)") +
  theme_minimal()





ggplot(dati, aes(x = Ospedale, y = Peso, fill = Ospedale)) +
  geom_boxplot() +
  labs(title = "Distribuzione del Peso dei Neonati per Ospedale",
       x = "Ospedale",
       y = "Peso (grammi)") +
  theme_minimal()
