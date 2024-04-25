# lettura CSV
dati <- read.csv("nascite e morti.csv",sep=",")
N <- dim(dati)[1]

library(ggplot2)
library(mapdata)

# serie temporale
ggplot(data = dati) +
  geom_line(aes(x=tempo,y=nascite, col="nascite"),lwd=1) + 
  geom_line(aes(x=tempo,y=morti ,col="morti"),lwd=1) +
  geom_point(aes(x=tempo,y=nascite), col="green3",lwd=3) + 
  geom_point(aes(x=tempo,y=morti),col="red",lwd=3) +
  labs(x="Anni",
       y="Numero di Nascite/Morti",
       title="Andamento demografico") +
  scale_color_manual(
    name = "",
    breaks = c("nascite","morti"),
    values = c("green3","red"),
    labels = c("Nascite","Morti")
  ) +
  geom_text(aes(x=tempo,y=nascite-10,label=nascite)) + 
  geom_text(aes(x=tempo,y=morti+10,label=morti)) +
  scale_x_continuous(breaks = seq(2000,2020,1))

# cartogramma sud/nord
italia <- map_data("italy")
ggplot(data=italia) +
  geom_map(map = italia,
           mapping = aes(map_id=region,fill=group),
           col="black") +
  scale_x_continuous(limits = c(7,18)) +
  scale_y_continuous(limits = c(37,47)) +
  theme_bw()