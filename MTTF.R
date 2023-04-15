
# Lettura del dataset interarrivals di tuples-M0-2700
interarrivals_M0N0 <- read.table("ffdatools/tuples-R62-M0-N0-2700/interarrivals.txt")
interarrivals_M0N4 <- read.table("ffdatools/tuples-R62-M0-N4-2700/interarrivals.txt")
interarrivals_M0NC <- read.table("ffdatools/tuples-R62-M0-NC-2700/interarrivals.txt")
interarrivals_M1N0 <- read.table("ffdatools/tuples-R63-M1-N0-1200/interarrivals.txt")
interarrivals_M1N8 <- read.table("ffdatools/tuples-R63-M1-N8-1200/interarrivals.txt")
interarrivals_M1NC <- read.table("ffdatools/tuples-R63-M1-NC-1200/interarrivals.txt")


ttf_M0N0<-ecdf(interarrivals_M0N0$V1) 
ttf_M0N4<-ecdf(interarrivals_M0N4$V1) 
ttf_M0NC<-ecdf(interarrivals_M0NC$V1) 
ttf_M1N0<-ecdf(interarrivals_M1N0$V1) 
ttf_M1N8<-ecdf(interarrivals_M1N8$V1) 
ttf_M1NC<-ecdf(interarrivals_M1NC$V1) 


# knots restituisce i punti su cui la ecdf è stata invocata
t_M0N0<-knots(ttf_M0N0)
t_M0N4<-knots(ttf_M0N4)
t_M0NC<-knots(ttf_M0NC)
t_M1N0<-knots(ttf_M1N0)
t_M1N8<-knots(ttf_M1N8)
t_M1NC<-knots(ttf_M1NC)

r_M0N0 <- 1-ttf_M0N0(t_M0N0)
r_M0N4 <- 1-ttf_M0N4(t_M0N4)
r_M0NC <- 1-ttf_M0NC(t_M0NC)
r_M1N0 <- 1-ttf_M1N0(t_M1N0)
r_M1N8 <- 1-ttf_M1N8(t_M1N8)
r_M1NC <- 1-ttf_M1NC(t_M1NC)


wei_mod_M0N0<-nls (r_M0N0 ~ exp(-(l*t_M0N0)^a), start=list(l=(1/mean(interarrivals_M0N0$V1)), a=0.95))
plot(t_M0N0, predict(wei_mod_M0N0), col="red", lwd=2, type="l", xlim= c(0,100000), xlab="timestamp", ylab="probability")

wei_mod_M0N4<-nls (r_M0N4 ~ exp(-(l*t_M0N4)^a), start=list(l=(1/mean(interarrivals_M0N4$V1)), a=0.95))
lines(t_M0N4, predict(wei_mod_M0N4), col="blue", lwd=2)

wei_mod_M0NC<-nls (r_M0NC ~ exp(-(l*t_M0NC)^a), start=list(l=(1/mean(interarrivals_M0NC$V1)), a=0.95))
lines(t_M0NC, predict(wei_mod_M0NC), col="gold3", lwd=2)

hex2_mod_M1N0<-nls (r_M1N0 ~ 0.2*exp(-(l1*t_M1N0))+0.8*exp(-(l2*t_M1N0)), start=list(l1=(1/mean(interarrivals_M1N0$V1)),l2=1.350401e-06 ))
lines(t_M1N0, predict(hex2_mod_M1N0), col="magenta", lwd=2)

hex2_mod_M1N8<-nls (r_M1N8 ~ 0.2*exp(-(l1*t_M1N8))+0.8*exp(-(l2*t_M1N8)), start=list(l1=(1/mean(interarrivals_M1N8$V1)),l2=1.31128e-06 ))
lines(t_M1N8, predict(hex2_mod_M1N8), col="grey48", lwd=2)


# Stima delle regressione: modello iperesponenzionale (2)
hex2_mod_M1NC<-nls (r_M1NC ~ 0.2*exp(-(l1*t_M1NC))+0.8*exp(-(l2*t_M1NC)), start=list(l1=(1/mean(interarrivals_M1NC$V1)),l2=1.369802e-06 ))
lines(t_M1NC, predict(hex2_mod_M1NC), col="cyan", lwd=2)


legend( x="topright", 
        legend=c("R62-M0-N0", "R62-M0-N4","R62-M0-NC","R63-M1-N0","R63-M1-N8","R63-M1-NC"),
        col=c("red","blue", "gold3", "magenta", "grey48", "cyan"), lwd=1)

