library(ggplot2)
# Lettura del file interarrivals.txt
interarrivals <- read.table("C:/Users/jessi/Desktop/Università/Magistrale/II ANNO/Data Science/Homeworks/Homework1/ffdatools/tuples-bglsep_1-120/interarrivals.txt")


#scatterplot timestamp-nodo 



x<- interarrivals$V1

mean_val <- mean(interarrivals$V1)

hist(x, xlab = "Interarrivals" )



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
