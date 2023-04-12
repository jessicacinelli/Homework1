####### CWIN selection ####### 

# Lettura del dataset tcount-bgloct_1.txt
tcount.bgloct <- read.delim("C:/Users/jessi/Desktop/Università/Magistrale/II ANNO/Data Science/Homeworks/Homework1/ffdatools/counts/tcount-bgloct_1.txt")
CWIN<-tcount.bgloct$CWIN
COUNT<-tcount.bgloct$COUNT

p<-plot(CWIN, COUNT, type='o')

# Analisi ravvicinata per scegliere i parametri per trovare la retta tangente
p<-plot(CWIN, COUNT, type='o', xlim =c(0,400), ylim=c(0,400))

# Plot della retta tangente che passa per il punto di ginocchio 
x1=c(0,295)
y1=c(195,0)
line<-lines(x1,y1,type='l', col='red', lwd=3)

# lm per ottenere i coefficiente angolare e intercetta della retta tangente
lm(x1 ~ y1)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
#abline(v=100, lwd=2, col="blue") 
point <- points(CWIN[which.max(CWIN > 100)], COUNT[which.max(CWIN>100)], pch=20, col="green", cex=2)

CWIN[which.max(CWIN > 100)] #restituisce il valore di CWIN selezionato


############################## 
######reliability modeling#### 

# Lettura del dataset interarrivals di tuples-bgloct_1-120
interarrivals <- read.table("C:/Users/jessi/Desktop/Università/Magistrale/II ANNO/Data Science/Homeworks/Homework1/ffdatools/tuples-bgloct_1-120/interarrivals.txt")

# Plot della ecdf 
plot (ecdf(interarrivals$V1), col="blue", xlim=c(0,130000), main=NULL, xlab="time (s)", ylab="p")
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
plot(t, r,type="o", pch=16 )

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
hex_mod<-nls (r ~ 0.5*exp(-(l1*t))+0.5*exp(-(l2*t)), start=list(l1=(1/mean(interarrivals$V1)),l2=4.964024e-06 ))

#lines(t, predict(hex_mod), col="magenta", lwd=2)
ks.test(r, predict(hex_mod))

# Stima delle regressione: modello iperesponenzionale (2)
hex2_mod<-nls (r ~ 0.4*exp(-(l1*t))+0.6*exp(-(l2*t)), start=list(l1=(1/mean(interarrivals$V1)),l2=4.964024e-06 ))

lines(t, predict(hex2_mod), col="green", lwd=2)
ks.test(r, predict(hex2_mod)) #restituisce il p-value più alto. 

legend( x="right", 
        legend=c("Modello esponenziale", "Modello Weibull", "Modello iper-esponenziale"),
        col=c("blue","red", "green"), lwd=1)
