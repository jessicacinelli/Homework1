# Calcolo della media
media <- mean(interarrivals$V1)

# Calcolo numero degli interarrivi
n <- length(interarrivals$V1)

# Calcolo deviazione standard
deviazione_standard <- sd(interarrivals$V1)

# Calcolo mediana
median(interarrivals$V1)

# Calcolo range semi-inter-quartile
quartiles<-quantile(interarrivals$V1, probs=c(0,0.25,0.5,0.75,1))
SIQR<-(quartiles[4] - quartiles[2])/2

# Calcolo errore standard
errore_standard <- deviazione_standard/ sqrt(n)

# Calcolo intervallo di confidenza
alpha = 0.10 # 90%
alpha = 0.05 # 95%
gradi_di_liberta = n - 1
t_score = qt(p=alpha/2, df=gradi_di_liberta,lower.tail=F)
errore_margine <- t_score * errore_standard

limite_inferiore <- media - errore_margine
limite_superiore <- media + errore_margine

