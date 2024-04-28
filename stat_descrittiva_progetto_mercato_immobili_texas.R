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