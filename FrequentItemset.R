library('arules') 
library('arulesViz')

#lettura delle transazioni del mese di settembre
data_sep <- read.transactions("C:/Users/jessi/Desktop/Università/Magistrale/II ANNO/Data Science/Homeworks/Homework1/ffdatools/tuples-bglsep_1-120/output.csv", sep=",")

#calcolo della soglia: almeno due fallimenti in un giorno per il mese di settembre
s_sep<-60/length(data_sep)
s_sep

#calcolo dei frequent itemset
freq_sep<-apriori(data_sep, parameter =list(support=s_sep, confidence=0.75, minlen=2, maxlen=20, target="frequent itemsets")) 

#calcolo delle regole associative
rules_sep<-apriori(data_sep, parameter =list(support=s_sep, confidence=0.75, minlen=2, maxlen=20, target="rules")) 
summary(rules_sep)
inspect(freq_sep)
inspect(sort(rules_sep, by="confidence"))

#opzione per non far sovrapporre le misure di lift 
options(digits=2)
plot(rules_sep, method = "grouped", control = list(k = 10), col=rainbow(3))

#rimozione regole ridondanti
nonr_rules <- rules_sep[!is.redundant(rules_sep)]   

## Check
summary(nonr_rules)
inspect(nonr_rules)

plot(nonr_rules, method = "grouped", control = list(k = 4), col=rainbow(3))

##lettura delle transazioni del mese di ottobre
data_oct <- read.transactions("C:/Users/jessi/Desktop/Università/Magistrale/II ANNO/Data Science/Homeworks/Homework1/ffdatools/tuples-bgloct_1-120/output.csv", sep=",")

#calcolo della soglia: almeno un fallimenti in un giorno per il mese di ottobre
s_oct<-30/length(data_oct)

#calcolo dei frequent itemset
freq_oct<-apriori(data_oct, parameter =list(support=0.257, confidence=0.75, minlen=2, maxlen=20, target="frequent itemsets")) 

#calcolo delle regole associative
rules_oct<-apriori(data_oct, parameter =list(support=0.257, confidence=0.75,  minlen=2, maxlen=20, target="rules")) 

inspect(freq_oct)
inspect(rules_oct)
summary(freq_oct)
summary(rules_oct)

plot(rules_oct, method = "grouped", control = list(k = 70), col=rainbow(3))


#rimozione regole ridondanti
nonr_rules <- rules_oct[!is.redundant(rules_oct)]   

## Check
summary(nonr_rules)
inspect(nonr_rules)

#due cifre dopo la virgola 
options(digits=3)
plot(nonr_rules, method = "grouped", control = list(k = 50), col=rainbow(3))

