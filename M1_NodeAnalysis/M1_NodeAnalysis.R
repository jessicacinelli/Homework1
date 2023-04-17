####### CWIN selection ####### 

# Lettura del dataset tcount-bglsep_1.txt
tcount.R63M1N0 <- read.delim("ffdatools/counts/tcount-R63-M1-N0.txt")
CWIN_0<-tcount.R63M1N0$CWIN
COUNT_0<-tcount.R63M1N0$COUNT
p0<-plot(CWIN_0, COUNT_0, type='o',xlab = "CWIN_N0", ylab="COUNT_N0", xlim=c(0,1000), ylim=c(0,150) )

# Plot della retta tangente che passa per il punto di ginocchio 
x0=c(0,180)
y0=c(180,0)
line<-lines(x0,y0,type='l', col='red', lwd=3)

# lm per ottenere i coefficiente angolare e intercetta della retta tangente
lm(x0 ~ y0)


# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
point <- points(CWIN_0[which.max(CWIN_0 > 100)], COUNT_0[which.max(CWIN_0>100)], pch=20, col="cyan", cex=2)

CWIN_0[which.max(CWIN_0 > 100)] #restituisce il valore di CWIN selezionato


# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
point <- points(CWIN_0[which.max(CWIN_0 > 200)], COUNT_0[which.max(CWIN_0>200)], pch=20, col="green", cex=2)

CWIN_0[which.max(CWIN_0 > 200)] #restituisce il valore di CWIN selezionato

######################################################################## 
tcount.R63M1N8 <- read.delim("ffdatools/counts/tcount-R63-M1-N8.txt")
CWIN_8<-tcount.R63M1N8$CWIN
COUNT_8<-tcount.R63M1N8$COUNT
p0<-plot(CWIN_8, COUNT_8, type='o' ,xlab = "CWIN_N8", ylab="COUNT_N8", xlim=c(0,1000), ylim=c(0,150) )

# Plot della retta tangente che passa per il punto di ginocchio 
x8=c(0,180)
y8=c(180,0)
line<-lines(x8,y8,type='l', col='red', lwd=3)

# lm per ottenere i coefficiente angolare e intercetta della retta tangente
lm(x8 ~ y8)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
point <- points(CWIN_8[which.max(CWIN_8 > 100)], COUNT_8[which.max(CWIN_8>100)], pch=20, col="cyan", cex=2)

CWIN_8[which.max(CWIN_8 > 100)] #restituisce il valore di CWIN selezionato

point <- points(CWIN_8[which.max(CWIN_8 > 200)], COUNT_8[which.max(CWIN_8>200)], pch=20, col="green", cex=2)

CWIN_8[which.max(CWIN_8 > 200)] #restituisce il valore di CWIN selezionato



##############
tcount.R63M1NC <- read.delim("ffdatools/counts/tcount-R63-M1-NC.txt")
CWIN_C<-tcount.R63M1NC$CWIN
COUNT_C<-tcount.R63M1NC$COUNT
pC<-plot(CWIN_C, COUNT_C, type='o' ,xlab = "CWIN_NC", ylab="COUNT_NC", xlim=c(0,1000), ylim=c(0,150) )

# Analisi ravvicinata per scegliere i parametri per trovare la retta tangente
#p<-plot(CWIN, COUNT, type='o', xlim =c(0,3000), ylim=c(50,100))

# Plot della retta tangente che passa per il punto di ginocchio 
xC=c(0,180)
yC=c(180,0)
line<-lines(xC,yC,type='l', col='red', lwd=3)

# lm per ottenere i coefficiente angolare e intercetta della retta tangente
lm(xC ~ yC)

# Selezione del valore di CWIN nell’intorno destro del punto di tangenza
#abline(v=100, lwd=2, col="blue") 
point <- points(CWIN_C[which.max(CWIN_C > 100)], COUNT_C[which.max(CWIN_C>100)], pch=20, col="cyan", cex=2)

CWIN_C[which.max(CWIN_C > 1000)] #restituisce il valore di CWIN selezionato


point <- points(CWIN_C[which.max(CWIN_C > 200)], COUNT_C[which.max(CWIN_C>200)], pch=20, col="green", cex=2)

CWIN_C[which.max(CWIN_C > 200)] #restituisce il valore di CWIN selezionato

