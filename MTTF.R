R63M1N0<- read.table("ffdatools/tuples-R63-M1-N0-1200/interarrivals.txt")
R63M1N8<- read.table("ffdatools/tuples-R63-M1-N8-1200/interarrivals.txt")
R63M1NC<- read.table("ffdatools/tuples-R63-M1-NC-1200/interarrivals.txt")
R63M1N0_ttf<-ecdf(R63M1N0$V1) 
R63M1N8_ttf<-ecdf(R63M1N8$V1)
R63M1NC_ttf<-ecdf(R63M1NC$V1)

R63M1N0_ttf
# Load ggplot2
library(ggplot2)

# Create data
data <- data.frame(
  name=c("R63M1N0","R63M1N8","R63M1NC") ,  
  value=c(length(R63M1N0_ttf), length(R63M1N8_ttf),length(R63M1NC_ttf))
)

# Barplot
ggplot(data, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

# 3: Using RColorBrewer
ggplot(data, aes(x=name )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")
