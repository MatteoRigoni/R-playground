library(ggplot2)

#lancio di due dadi (frequentista)
dado_1 <- c(1,2,3,4,5,6)
dado_2 <- c(1:12) # dodici facce

expand.grid(dado_1,dado_2) #possibili combinazioni

lanci <- function(x,y,n) {
  var_cas <- sample(x,n,replace=TRUE) + sample(y,n,replace=TRUE)
}
  
esperimento <- lanci(dado_1, dado_2, 20000)

ggplot()+
  geom_histogram(aes(x=esperimento,
                     y=stat(count/sum(count))),
                 stat="count",
                 col="black",
                 fill="lightblue")+
  scale_x_continuous(breaks=seq(2,50,1))+
  labs(x="Somma di due dadi",y="Probabilita")