# lettura CSV
dati <- read.csv("realestate_texas.csv",sep=",")
head(dati, 10)

attach(dati)

View(sales)
table(volume)

summary(median_price)
summary(sales)

var(median_price)
sd(median_price)

CV<-function(x) {
  return (sd(x)/mean(x)*100)
}

CV(median_price)
CV(z)

N <- dim(dati)[1]

sales_CL <- cut(sales, seq(79,423+100,100))
table(sales_CL)

distr_freq_sales <- as.data.frame(
  cbind(
    ni = table(sales_CL),
    fi = table(sales_CL)/N,
    Ni=cumsum(table(sales_CL)),
    Fi=cumsum(table(sales_CL)/N)
  )
)

kable(distr_freq_sales, "markdown") %>%
  kable_styling(full_width = FALSE)%>%
  column_spec(1, border_right = TRUE)

distr_freq_median_price <- as.data.frame(
  cbind(
    ni = table(media_price_CL),
    fi = table(media_price_CL)/N,
    Ni=cumsum(table(media_price_CL)),
    Fi=cumsum(table(media_price_CL)/N)
  )
)

barplot(distr_freq_sales$ni,
        main="Distribuzione degli intervalli di vendite mensili",
        xlab = "Intervalli di vendite",
        ylab = "Frequenze assolute",
        ylim = c(0,200),
        col = "blue",
        names.arg = rownames(distr_freq_sales))

gini_index<-function(x) {
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J=length(table(x))
  
  gini=1-sum(fi2)
  gini_normalizzato = gini/((J-1)/J)
  
  return (gini_normalizzato)
}

table(dati$sales)
gini_index(dati$city) #0.97, quindi molto eterogeneo, range tra 0 e 1


ggplot(data=distr_freq_sales)+
  geom_histogram(aes(x=rownames(distr_freq_sales),
                     y=distr_freq_sales$ni),
                 stat="count",
                 col="black",
                 fill="lightblue")+
  labs(x="Intervalli di vendite",y="Totale per mese")

distr_freq_sale$cXi <- seq(83,419,8)
weighted.mean(distr_freq_sale$cXi, distr_freq_sale$ni)

incrementi_perc_listing <- quantmod::Delt(listings)*100
incrementi_perc_listing <- incrementi_perc_listing[-1]

geometric_mean <- function(x) {
  return (prod(x)^(1/length(x)))
}

geometric_mean(incrementi_perc_listing)
mean(incrementi_perc_listing)

distr_freq_city <- as.data.frame(
  cbind(
    ni = table(city*sales),
    fi = table(city*sales)/N,
    Ni=cumsum(table(city*sales)),
    Fi=cumsum(table(city*sales)/N)
  )
)

library(dplyr)
percent_sales_by_city <- dati %>%
  group_by(city) %>%
  summarise(percent_sales = sum(sales) / sum(dati$sales) * 100)

library(moments)
skewness(median_price)
kurtosis(median_price)



sale_CL <- cut(volume, seq(79,423,8)) 

View(table(sale_CL) )



distr_freq_sale <- as.data.frame( 
  
  cbind( 
    
    ni = table(sale_CL), 
    
    fi = table(sale_CL)/N, 
    
    Ni=cumsum(table(sale_CL)), 
    
    Fi=cumsum(table(sale_CL)/N) 
    
  ) 
  
) 



distr_freq_sale$cXi <- seq(83,419,8) 

weighted.mean(distr_freq_volume$cXi, distr_freq_volume$ni) 




library(psych)
library(kableExtra)

all_indici <- describe(dati, IQR=TRUE,quant=c(.25,.75),omit=TRUE,type=1)

indici <- descrizione[c("min", "Q0.25", "mean", "median", "Q0.75", "max", "sd", "IQR", "skew", "kurtosis")]
indici_2_decimali <- round(indici, 2)

kable(indici_2_decimali, "markdown") %>%
  kable_styling(full_width = FALSE)%>%
  column_spec(1, border_right = TRUE)

dati$mean_price = round(volume*1000000/sales, 0)
print(dati)

kable(dati, "markdown") %>%
  kable_styling(full_width = FALSE)%>%
  column_spec(1, border_right = TRUE)

dati$ratio_volume <- dati$listings / dati$volume 
dati$ratio_sales <- dati$listings / dati$sales

dati$media_ponderata <- with(dati, (dati$volume * dati$mean_price + dati$listings) / 2) 

correlazione <- cor(dati$listings, dati$volume)
print(correlazione)

plot(dati$volume, dati$ratio_volume, xlab = "Annunci per mese", ylab = "Incasso vendite per mese", main = "Relazione incassi mensili e numero annunci")
plot(dati$volume, dati$listings, xlab = "Numero vendite per mese", ylab = "Rapporto annunci/vendite", main = "Relazione vendite mensili e numero annunci")


dati$volume <- as.numeric(dati$volume)

sum(dati$volume)



dati_summary_city = dati %>%
  group_by(city) %>%
  summarise(
    tot_sales = sum(sales),
    tot_volume = sum(volume),
    max_volume = max(volume)
  ) %>%
  arrange(city) %>%
  mutate(
    diff_perc_sales = ((tot_sales / lag(tot_sales)) - 1) * 100,
    diff_perc_volume = ((tot_volume / lag(tot_volume)) - 1) * 100
  )
kable(dati_summary_city, "markdown") %>%
  kable_styling(full_width = FALSE)%>%
  column_spec(c(1,3), border_right = TRUE)



dati_summary_perc = dati %>%
  group_by(year) %>%
  summarise(
    tot_sales = sum(sales),
    tot_volume = sum(volume)
  ) %>%
  arrange(year) %>%
  mutate(
    diff_perc_sales = ((tot_sales / lag(tot_sales)) - 1) * 100,
    diff_perc_volume = ((tot_volume / lag(tot_volume)) - 1) * 100
  )
kable(dati_summary_perc, "markdown") %>%
  kable_styling(full_width = FALSE)%>%
  column_spec(c(1,3), border_right = TRUE)

summary(dati_summary_perc$diff_perc_volume)

kable(dati_summary_perc, "markdown") %>%
  kable_styling(full_width = FALSE)%>%
  column_spec(c(1,3), border_right = TRUE)

library(ggplot2)

dati$year_factor <- as.factor(dati$year)

ggplot(data = dati, aes(x = year_factor, y = volume, fill = year_factor)) +
  geom_boxplot() +
  labs(x = "Città", y = "Prezzo vendite totali", title = "Distribuzione del prezzo di vendite totale per città") +
  theme_light() +
  theme(legend.position = "right")

ggplot(data = dati, aes(x = city, y = median_price, fill = city)) +
  geom_boxplot(coef = 1.5, outlier.shape = NA, width = 0.6) +  # Boxen plot
  labs(x = "City", y = "Median Price", title = "Boxen Plot of Median Price by City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = dati, aes(x = month, y = volume, fill = city)) +
  geom_bar(stat = "identity", position = "stack") +  # Barre sovrapposte
  labs(x = "Mese", y = "Totale vendite", title = "Totale vendite per mese e città") +
  theme_light() +
  theme(legend.position = "right")


dati$periodo <- as.Date(paste(dati$year, dati$month, "01", sep = "-"))

trimestri <- unique(dati$periodo)[seq(1, length(unique(dati$periodo)), by = 3)]
trimestri_labels <- format(trimestri, "%m/%Y")

ggplot(data = dati, aes(x = factor(periodo, ordered = TRUE), y = volume, fill = city)) +
  geom_col(position = "fill") +
  labs(x = "Mese di riferimento", y = "Totale vendite", title = "Totale vendite per periodo e città") +
  scale_x_discrete(breaks = as.character(trimestri), labels = trimestri_labels) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom",axis.title.x = element_text(margin = margin(t = 10)))

ggplot(data = dati, aes(x = factor(periodo, ordered = TRUE), fill = city)) +
  geom_bar(position = "fill") +  
  labs(x = "Mese/Anno", y = "Proporzione di vendite", title = "Proporzione delle vendite per mese/anno e città") +
  scale_y_continuous(limits=c(0,1)) +  # Formattazione percentuale sull'asse y
  scale_x_discrete(breaks = as.character(trimestri), labels = trimestri_labels) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")



dati_normalized <- dati %>%
  group_by(periodo) %>%
  mutate(prop_vendite = volume / sum(volume))

# Grafico a barre normalizzato con geom_col()
ggplot(data = dati_normalized, aes(x = factor(periodo, ordered = TRUE), y = prop_vendite, fill = city)) +
  geom_col() +  # Barre normalizzate
  labs(x = "Mese/Anno", y = "Proporzione di vendite", title = "Proporzione delle vendite per mese/anno e città") +
  scale_x_discrete(breaks = as.character(trimestri), labels = trimestri_labels) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")




# Filtra il dataset per un anno specifico
dati_2014 <- filter(dati, year == 2014)

# Crea il line chart per confrontare una variabile (ad esempio, volume di vendite) tra città e periodi storici




ggplot(data = dati, aes(x = periodo, y = months_inventory, color = city)) +
  geom_line() +  # Linee per ogni città
  geom_point() +
  labs(x = "Periodo", y = "Numero di mesi", title = "Andamento del numero di mesi residui per chiudere le vendite") +
  theme_light() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  






dati <- dati %>%
  arrange(city, periodo)

dati <- dati %>%
  group_by(city) %>%
  mutate(incremento_volume = volume - lag(volume),
         incremento_listings = listings - lag(listings))

tot_aumento_volume <- sum(!is.na(dati$incremento_volume) & dati$incremento_volume >0)
tot_aumento_entrambi <- sum(!is.na(dati$incremento_volume) & dati$incremento_volume >0 & !is.na(dati$incremento_listings) & dati$incremento_listings >0)

perc_correlazione = tot_aumento_entrambi/tot_aumento_volume * 100
