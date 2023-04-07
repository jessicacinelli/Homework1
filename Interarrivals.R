# Lettura del file interarrivals.txt
interarrivals <- read.table("C:/Users/jessi/Desktop/Università/Magistrale/II ANNO/Data Science/Homeworks/Homework1/ffdatools/tuples-bglsep_1-120/interarrivals.txt")
plot(interarrivals)

#scatterplot timestamp-nodo 

plot(interarrivals$V1, pch=3)

x<- interarrivals$V1

mean_val <- mean(interarrivals$V1)

#boxplot interarrivi con media di colore rosso
color<-c("Mean"= "red")
p<-ggplot(interarrivals, aes(x=V1, y="")) + 
  geom_boxplot(width=0.5) +
  geom_point(aes(x=mean_val, y="", color="Mean"), size=4 ) +
  ggtitle("Boxplot with Mean") +
  labs(x = "",
       y = "Values",
       color = "")+
  scale_color_manual(values = color)+
  theme(
    panel.border = element_blank(), axis.ticks = element_blank(),
    legend.position = "top", legend.text = element_text(size = 11),
    legend.title = element_text(size = 11.5),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    plot.margin = unit(c(0.5, 0.2, 0, 0.2), "cm")
  ) 

p

hist(x,20, xlab = "Interarrivals" )



#hist(x, breaks = "Sturges")
#hist(x, breaks = "Sturges", freq = FALSE)
#hist(x, breaks = "Sturges")
#hist(x, breaks = "Sturges", freq = FALSE)
r <- hist(x, breaks = "Sturges", freq = FALSE)

r$density
r$density0.5
sum (r$density0.5)

lines(density(x), col = "blue", lwd=3)
plot(ecdf(x))
x1<-seq(1,150000,0.1)
x1
pnorm(x1, mean(x), sd(x))

lines(x1,  pnorm(x1, mean(x), sd(x)), col = "red", lwd=4)

qqnorm(x)
qqline(x)
plot(ecdf(x))
x1<-seq(1,150,0.1)
lines(x1,  pnorm(x1, mean(x), sd(x)), col = "red", lwd=4)
qqnorm(x)
qqline(x)
