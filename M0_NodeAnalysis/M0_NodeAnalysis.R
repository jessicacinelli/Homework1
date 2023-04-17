####### CWIN selection ####### 

# Lettura del dataset tcount-NODE.txt
tcount.N0 <- read.delim("ffdatools/counts/tcount-R62-M0-N0.txt")
CWIN_N0<-tcount.N0$CWIN
COUNT_N0<-tcount.N0$COUNT

tcount.N4 <- read.delim("ffdatools/counts/tcount-R62-M0-N4.txt")
CWIN_N4<-tcount.N4$CWIN
COUNT_N4<-tcount.N4$COUNT

tcount.NC <- read.delim("ffdatools/counts/tcount-R62-M0-NC.txt")
CWIN_NC<-tcount.NC$CWIN
COUNT_NC<-tcount.NC$COUNT



p0<-plot(CWIN_N0, COUNT_N0, type='o')
p4<-plot(CWIN_N4, COUNT_N4, type='o')
pC<-plot(CWIN_NC, COUNT_NC, type='o')

# Analisi ravvicinata per scegliere i parametri per trovare la retta tangente
p0<-plot(CWIN_N0, COUNT_N0, type='o', xlim =c(0,1000), ylim=c(50,170))

# Plot della retta tangente che passa per il punto di ginocchio 
x0=c(0,165)
y0=c(165,0)
line<-lines(x0,y0,type='l', col='red', lwd=3)

# lm per ottenere i coefficiente angolare e intercetta della retta tangente
lm(x0 ~ y0)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
point0 <- points(CWIN_N0[which.max(CWIN_N0 > 110)], COUNT_N0[which.max(CWIN_N0>110)], pch=20, col="cyan", cex=2)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
point0 <- points(CWIN_N0[which.max(CWIN_N0 > 200)], COUNT_N0[which.max(CWIN_N0>200)], pch=20, col="green", cex=2)

CWIN_N0[which.max(CWIN_N0 > 200)] #restituisce il valore di CWIN selezionato

#####################################################################################################################

p4<-plot(CWIN_N4, COUNT_N4, type='o', xlim =c(0,1000), ylim=c(50,160))
# Plot della retta tangente che passa per il punto di ginocchio 
x4=c(0,167)
y4=c(167,0)
line<-lines(x4,y4,type='l', col='red', lwd=3)

# lm per ottenere i coefficiente angolare e intercetta della retta tangente
lm(x4 ~ y4)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
point4<- points(CWIN_N4[which.max(CWIN_N4 > 100)], COUNT_N4[which.max(CWIN_N4>100)], pch=20, col="cyan", cex=2)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
point4<- points(CWIN_N4[which.max(CWIN_N4 > 200)], COUNT_N4[which.max(CWIN_N4>200)], pch=20, col="green", cex=2)

CWIN_N4[which.max(CWIN_N4 > 200)] #restituisce il valore di CWIN selezionato

#####################################################################################################################

pc<-plot(CWIN_NC, COUNT_NC, type='o', xlim =c(0,1000), ylim=c(50,100))

# Plot della retta tangente che passa per il punto di ginocchio 
xc=c(0,165)
yc=c(165,0)
line<-lines(xc,yc,type='l', col='red', lwd=3)

# lm per ottenere i coefficiente angolare e intercetta della retta tangente
lm(xc ~ yc)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
pointc <- points(CWIN_NC[which.max(CWIN_NC > 90)], COUNT_NC[which.max(CWIN_NC>90)], pch=20, col="cyan", cex=2)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
pointc <- points(CWIN_NC[which.max(CWIN_NC > 200)], COUNT_NC[which.max(CWIN_NC>200)], pch=20, col="green", cex=2)

CWIN_NC[which.max(CWIN_NC > 200)] #restituisce il valore di CWIN selezionato

#####################################################################################################################


############################## 
######reliability modeling#### 

# Lettura del dataset interarrivals di tuples-M0-2700
interarrivals_N0 <- read.table("ffdatools/tuples-R62-M0-N0-240/interarrivals.txt")
interarrivals_N4 <- read.table("ffdatools/tuples-R62-M0-N4-240/interarrivals.txt")
interarrivals_NC <- read.table("ffdatools/tuples-R62-M0-NC-240/interarrivals.txt")

#-------------------------------------------------------N0-----------------------------------------------------------

# Plot della ecdf 
plot (ecdf(interarrivals_N0$V1), col="blue", main=NULL, xlab="time (s)", ylab="p")

# Calcolo della reliability come 1 - ttf
ttf_N0<-ecdf(interarrivals_N0$V1) 

# knots restituisce i punti su cui la ecdf è stata invocata
t_N0<-knots(ttf_N0)
t_N0

r_N0 <- 1-ttf_N0(t_N0)

#pch=16 per plottare i punti con i cerchi pieni
lines(t_N0, r_N0, , type="o", pch=16)
legend( x="right", 
        legend=c("CDF empirica", "Reliability"),
        col=c("blue","black"), lwd=1)
plot(t_N0, r_N0,type="o", pch=16 )

# Stima delle regressione: modello esponenziale
exp_mod <- nls (r_N0 ~ exp(-(l*t_N0)), start=list(l=(1/mean(interarrivals_N0$V1))))
lines(t_N0, predict(exp_mod), col="blue", lwd=2)
ks.test(r_N0, predict(exp_mod))

# Stima delle regressione: modello weibull
wei_mod<-nls (r_N0 ~ exp(-(l*t_N0)^a), start=list(l=(1/mean(interarrivals_N0$V1)), a=0.95))
wei_mod
lines(t_N0, predict(wei_mod), col="red", lwd=2)
ks.test(r_N0, predict(wei_mod))

1/mean(interarrivals_N0$V1)

# Stima delle regressione: modello iperesponenziale
hex2_mod<-nls (r_N0 ~ 0.2*exp(-(l1*t_N0))+0.8*exp(-(l2*t_N0)), start=list(l1=(1/mean(interarrivals_N0$V1)),l2=1.020735e-06 ))
lines(t_N0, predict(hex2_mod), col="green", lwd=2)
ks.test(r_N0, predict(hex2_mod)) #restituisce il p-value più alto. 
legend( x="right", 
        legend=c("Modello esponenziale", "Modello Weibull", "Modello iper-esponenziale"),
        col=c("blue","red", "green"), lwd=1)

#-------------------------------------------------------N4-----------------------------------------------------------

# Plot della ecdf 
plot (ecdf(interarrivals_N4$V1), col="blue", main=NULL, xlab="time (s)", ylab="p")

# Calcolo della reliability come 1 - ttf
ttf_N4<-ecdf(interarrivals_N4$V1) 

# knots restituisce i punti su cui la ecdf è stata invocata
t_N4<-knots(ttf_N4)
t_N4

r_N4 <- 1-ttf_N4(t_N4)

#pch=16 per plottare i punti con i cerchi pieni
lines(t_N4, r_N4, , type="o", pch=16)
legend( x="right", 
        legend=c("CDF empirica", "Reliability"),
        col=c("blue","black"), lwd=1)
plot(t_N4, r_N4,type="o", pch=16 )

# Stima delle regressione: modello esponenziale
exp_mod <- nls (r_N4 ~ exp(-(l*t_N4)), start=list(l=(1/mean(interarrivals_N4$V1))))
lines(t_N4, predict(exp_mod), col="blue", lwd=2)
ks.test(r_N4, predict(exp_mod))

# Stima delle regressione: modello weibull
wei_mod<-nls (r_N4 ~ exp(-(l*t_N4)^a), start=list(l=(1/mean(interarrivals_N4$V1)), a=0.95))
wei_mod
lines(t_N4, predict(wei_mod), col="red", lwd=2)
ks.test(r_N4, predict(wei_mod))

1/mean(interarrivals_N4$V1)

# Stima delle regressione: modello iperesponenziale
hex2_mod<-nls (r_N4 ~ 0.2*exp(-(l1*t_N4))+0.8*exp(-(l2*t_N4)), start=list(l1=(1/mean(interarrivals_N4$V1)),l2=1.040371e-06 ))
lines(t_N4, predict(hex2_mod), col="green", lwd=2)
ks.test(r_N4, predict(hex2_mod)) #restituisce il p-value più alto. 
legend( x="right", 
        legend=c("Modello esponenziale", "Modello Weibull", "Modello iper-esponenziale"),
        col=c("blue","red", "green"), lwd=1)

#-------------------------------------------------------NC-----------------------------------------------------------

# Plot della ecdf 
plot (ecdf(interarrivals_NC$V1), col="blue", main=NULL, xlab="time (s)", ylab="p")

# Calcolo della reliability come 1 - ttf
ttf_NC<-ecdf(interarrivals_NC$V1) 

# knots restituisce i punti su cui la ecdf è stata invocata
t_NC<-knots(ttf_NC)
t_NC

r_NC <- 1-ttf_NC(t_NC)

#pch=16 per plottare i punti con i cerchi pieni
lines(t_NC, r_NC, , type="o", pch=16)
legend( x="right", 
        legend=c("CDF empirica", "Reliability"),
        col=c("blue","black"), lwd=1)
plot(t_NC, r_NC,type="o", pch=16 )

# Stima delle regressione: modello esponenziale
exp_mod <- nls (r_NC ~ exp(-(l*t_NC)), start=list(l=(1/mean(interarrivals_NC$V1))))
lines(t_NC, predict(exp_mod), col="blue", lwd=2)
ks.test(r_NC, predict(exp_mod))

# Stima delle regressione: modello weibull
wei_mod<-nls (r_NC ~ exp(-(l*t_NC)^a), start=list(l=(1/mean(interarrivals_NC$V1)), a=0.95))
wei_mod
lines(t_NC, predict(wei_mod), col="red", lwd=2)
ks.test(r_NC, predict(wei_mod))

1/mean(interarrivals_NC$V1)

# Stima delle regressione: modello iperesponenziale
hex2_mod<-nls (r_NC ~ 0.2*exp(-(l1*t_NC))+0.8*exp(-(l2*t_NC)), start=list(l1=(1/mean(interarrivals_NC$V1)),l2=1.040371e-06 ))
lines(t_NC, predict(hex2_mod), col="green", lwd=2)
ks.test(r_NC, predict(hex2_mod)) #restituisce il p-value più alto. 
legend( x="right", 
        legend=c("Modello esponenziale", "Modello Weibull", "Modello iper-esponenziale"),
        col=c("blue","red", "green"), lwd=1)

