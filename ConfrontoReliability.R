# Lettura del dataset interarrivals di tuples-bglsep_1-120
sep <- read.table("ffdatools/tuples-bglsep_1-120/interarrivals.txt")

oct <- read.table("ffdatools/tuples-bgloct_1-120/interarrivals.txt")

# Calcolo della media
media_sep <- mean(sep$V1)
media_oct <- mean(oct$V1)
# Calcolo numero degli interarrivi
n_sep <- length(sep$V1)
n_oct <- length(oct$V1)
# Calcolo deviazione standard

deviazione_standard_sep <- sd(sep$V1)
deviazione_standard_oct <- sd(oct$V1)
# Calcolo mediana
m_sep<-median(sep$V1)
m_oct<-median(oct$V1)

# Calcolo range semi-inter-quartile
quartiles_sep<-quantile(sep$V1, probs=c(0,0.25,0.5,0.75,1))
SIQR_sep<-(quartiles_sep[4] - quartiles_sep[2])/2

quartiles_oct<-quantile(oct$V1, probs=c(0,0.25,0.5,0.75,1))
SIQR_sep<-(quartiles_oct[4] - quartiles_oct[2])/2

# Calcolo errore standard
errore_standard_sep <- deviazione_standard_sep/ sqrt(n_sep)
errore_standard_oct <- deviazione_standard_oct/ sqrt(n_oct)
# Calcolo intervallo di confidenza
alpha = 0.10 # 90%
#alpha = 0.05 # 95%

gradi_di_liberta_sep = n_sep - 1
gradi_di_liberta_oct = n_oct - 1
t_score_sep = qt(p=alpha/2, df=gradi_di_liberta_sep,lower.tail=F)
t_score_oct = qt(p=alpha/2, df=gradi_di_liberta_oct,lower.tail=F)
errore_margine_sep <- t_score_sep * errore_standard_sep
errore_margine_oct <- t_score_oct * errore_standard_oct

limite_inferiore_sep <- media_sep - errore_margine_sep
limite_superiore_sep <- media_sep + errore_margine_sep



limite_inferiore_oct <- media_oct - errore_margine_oct
limite_superiore_oct <- media_oct + errore_margine_oct

# Import ggplot2 library
library("ggplot2")


# Creating Data
CI_90<-round(data.frame(x = c(1,2),
                      y = c(1,30),
                      low = c(limite_inferiore_sep,limite_inferiore_oct),
                      up = c(limite_superiore_sep, limite_superiore_oct)),4)

# Creating scatter plot with its
# confindence intervals
plot90<-ggplot(CI_90, aes(x, y), width=0.9) + geom_point(aes( x=c(1,2), y=c(media_sep, media_oct)), col="red", size=2 ) + 
  geom_errorbar(aes(ymin = low, ymax = up), width=0.1, colour="blue", alpha=0.9, position=position_dodge(5))+
  scale_x_continuous(breaks=1:2, labels=c("September", "October")) +
  ylab("Mean") +
  xlab("") +
  coord_cartesian(xlim = c(0.92, 2.1), ylim = c(9000 , 25000)) +
  theme(
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1.5/1)+
  ggtitle("90% Confidence")


############################################### 95 % ######################################
#alpha = 0.10 # 90%
alpha = 0.05 # 95%

gradi_di_liberta_sep = n_sep - 1
gradi_di_liberta_oct = n_oct - 1
t_score_sep = qt(p=alpha/2, df=gradi_di_liberta_sep,lower.tail=F)
t_score_oct = qt(p=alpha/2, df=gradi_di_liberta_oct,lower.tail=F)
errore_margine_sep <- t_score_sep * errore_standard_sep
errore_margine_oct <- t_score_oct * errore_standard_oct

limite_inferiore_sep <- media_sep - errore_margine_sep
limite_superiore_sep <- media_sep + errore_margine_sep



limite_inferiore_oct <- media_oct - errore_margine_oct
limite_superiore_oct <- media_oct + errore_margine_oct

# Creating Data
CI_95<-round(data.frame(x = c(1,2),
                        y = c(1,30),
                        low = c(limite_inferiore_sep,limite_inferiore_oct),
                        up = c(limite_superiore_sep, limite_superiore_oct)),4)

# Creating scatter plot with its
# confindence intervals
plot95<-ggplot(CI_95, aes(x, y), width=0.9) + geom_point(aes( x=c(1,2), y=c(media_sep, media_oct)), col="red", size=2 ) + 
  geom_errorbar(aes(ymin = low, ymax = up), width=0.1, colour="blue", alpha=0.9, position=position_dodge(5))+
  scale_x_continuous(breaks=1:2, labels=c("September", "October")) +
  ylab("Mean") +
  xlab("") +
  coord_cartesian(xlim = c(0.92, 2.1), ylim = c(9000 , 25000)) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    aspect.ratio = 1.5/1)+
  ggtitle("95% Confidence")

library(cowplot)

plot_grid(plot90, plot95, labels = "")

