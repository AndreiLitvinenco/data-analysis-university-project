data = date_prelucrare_excel1_echipa_racheta

# Testarea ipotezelor
# sectiunea 1-3 chi-patrat - nu are o relatie semnificativa intre x si y

colnames(data)[21] = "d1"
colnames(data)[23] = "d2"
chisq.test(data$d1, data$d2)


#sectiunea 2-5 chi-patrat - nu are o relatie semnificativa intre x si y
colnames(data)[16] = "d3"
colnames(data)[31] = "d4"
chisq.test(data$d3, data$d4)

#sectiunea 8-9 - nu are o relatie semnificativa
ks.test(data$Numeracy, "pnorm")
cor.test(data$Numeracy, data$SATS2, method="spearman")

#sectiunea 12-13 
ks.test(data$SATS6, "pnorm")
cor.test(data$SATS6, data$SATS7, method="spearman")

#sectiunea 14-15
ks.test(data$SATS9, "pnorm")
cor.test(data$SATS9, data$SATS10, method="spearman")

# ks pentru supersition thinking
ks.test(data$SATS7, "pnorm")

# ks pentru conspiracy
ks.test(data$SATS9, "pnorm")

# ks pentru life orientation
ks.test(data$SATS10, "pnorm")



#Statistici descriptive si reprezentari grafice

#Sectiunea 1-3
table(data$d1)
frecvente_d1 <- table(data$d1)
barplot(frecvente_d1)

summary(as.factor(data$d1))
describe(as.factor(data$d1))

table(data$d2)
frecvente_d2 <- table(data$d2)
barplot(frecvente_d2)
describe(as.factor(data$d2))
#Sectiunea 2-5
table(data$d3)
frecvente_d3 <- table(data$d3)
barplot(frecvente_d3)
describe(as.factor(data$d3))

table(data$d4)
frecvente_d4 <- table(data$d4)
barplot(frecvente_d4)
describe(as.factor(data$d4))


#Biblioteca "psych"
# install.packages("psych")
library(Hmisc)
library(psych)

#Sectiunea 8-9 

mean(data$Numeracy) # Media
median(data$Numeracy) # Mediana
sd(data$Numeracy)  # Abaterea standard
summary(data$Numeracy) # Rezumat statistic complet
describe(data$Numeracy) # Rezumat statistic complet
summary(data$Numeracy)

hist(data$Numeracy) # Reprezentare grafică – histograma

mean(data$SATS2) 
median(data$SATS2) 
sd(data$SATS2)  
summary(data$SATS2) 
describe(data$SATS2) 

hist(data$SATS2) 

#sectiunea 12-13

mean(data$SATS6) 
median(data$SATS6) 
sd(data$SATS6)  
summary(data$SATS6) 
describe(data$SATS6) 

hist(data$SATS6) 

mean(data$SATS7) 
median(data$SATS7) 
sd(data$SATS7)  
summary(data$SATS7) 
describe(data$SATS7) 

hist(data$SATS7) 

#sectiunea 14-15

mean(data$SATS9) 
median(data$SATS9) 
sd(data$SATS9)  
summary(data$SATS9) 
describe(data$SATS9) 

hist(data$SATS9)

mean(data$SATS10) 
median(data$SATS10) 
sd(data$SATS10)  
summary(data$SATS10) 
describe(data$SATS10) 

hist(data$SATS10)


# regresie liniara multipla

model_1 = lm(data$SATS9 ~ data$SATS6 + data$SATS7, data = data)
summary(model_1)

par(mfrow=c(2,2))
plot(model_1)
par(mfrow=c(1,1))


#install.packages("car")
library(car)


vif(model_1)
vif_values = vif(model_1)
barplot(vif_values, main = "VIF Values",col = 'green',ylim = c(0.0,8.0))
bad_vif <- 5.0
abline(h = bad_vif, col = 'red')

# regresie logistica


data$CRT_copy = ifelse(data$CRT == 3, 1, 0)

data$sylogistic_copy = ifelse(data$sylogistic == 2, 1, 0)

model_2 = glm(data$sylogistic_copy ~ data$SATS6 + data$SATS7,
              family = binomial, data = data )
summary(model_2)
par(mfrow=c(2,2))
plot(model_2)
par(mfrow=c(1,1))

vif(model_2)
vif_values = vif(model_2)
barplot(vif_values, main = "VIF Values",col = 'green',ylim = c(0.0,8.0))
bad_vif <- 5.0
abline(h = bad_vif, col = 'red')


# Curba ROC + AUC 

install.packages("pROC")  
library(pROC)

roc_obj <- roc(data$sylogistic_copy, predict(model_2, type = "response"))
plot(roc_obj, main = "Curba ROC - model_2", col = "orange", lwd = 2)
text(0.3, 0.3, paste("AUC:", round(auc(roc_obj), 3)), cex = 1.2, font = 2)
