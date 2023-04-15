library(ggplot2)
# Lettura del file interarrivals.txt
interarrivals <- read.table("ffdatools/tuples-R63-M1-NC-1200/interarrivals.txt")

# Plot della ecdf 
plot(ecdf(interarrivals$V1), col="blue", main=NULL, xlab="time (s)", ylab="p")

# Calcolo della reliability come 1 - ttf
ttf<-ecdf(interarrivals$V1) 

# knots restituisce i punti su cui la ecdf è stata invocata
t<-knots(ttf)
t

r <- 1-ttf(t)

#pch=16 per plottare i punti con i cerchi pieni
lines(t, r, , type="o", pch=16)
legend( x="right", 
        legend=c("CDF empirica", "Reliability"),
        col=c("blue","black"), lwd=1)
plot(t, r,type="o", pch=16 , xlab = "t_NC", ylab="r_NC")
1/mean(interarrivals$V1)
# Stima delle regressione: modello esponenziale
exp_mod <- nls (r ~ exp(-(l*t)), start=list(l=(1/mean(interarrivals$V1))))
lines(t, predict(exp_mod), col="blue", lwd=2)
ks.test(r, predict(exp_mod))

# Stima delle regressione: modello weibull
wei_mod<-nls (r ~ exp(-(l*t)^a), start=list(l=(1/mean(interarrivals$V1)), a=0.95))
wei_mod
lines(t, predict(wei_mod), col="red", lwd=2)
ks.test(r, predict(wei_mod))

1/mean(interarrivals$V1)

# Stima delle regressione: modello iperesponenziale (1)
hex_mod<-nls (r ~ 0.5*exp(-(l1*t))+0.5*exp(-(l2*t)), start=list(l1=(1/mean(interarrivals$V1)),l2=1.369802e-06 ))

#lines(t, predict(hex_mod), col="magenta", lwd=4)
ks.test(r, predict(hex_mod))

# Stima delle regressione: modello iperesponenzionale (2)
hex2_mod<-nls (r ~ 0.2*exp(-(l1*t))+0.8*exp(-(l2*t)), start=list(l1=(1/mean(interarrivals$V1)),l2=1.369802e-06 ))
ks.test(r, predict(hex2_mod)) #restituisce il p-value più alto. 
lines(t, predict(hex2_mod), col="green", lwd=2)

legend( x="right", 
        legend=c("Modello esponenziale", "Modello Weibull", "Modello iper-esponenziale"),
        col=c("blue","red", "green"), lwd=1)

