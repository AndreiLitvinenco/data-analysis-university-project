#Redenumire coloane
#Merge între cele 3 seturi de date, după codul de înregistrare
#Intrebarile de control - ifelse
#Creat noi coloane
#Scorare scale - Itemi inversati (reversed)
#Scorare scale - ifelse
#Vizualizare
#Stergerea unor coloane
#Scrierea unui nou set de date (salvat din R)


#Redenumire coloane

colnames(Chestionar1)
colnames(Chestionar2)
colnames(Chestionar3)

colnames(Chestionar1)[3] = "cod"
colnames(Chestionar2)[3] = "cod"
colnames(Chestionar3)[3] = "cod"

#Merge între cele 3 seturi de date

data = merge(Chestionar1, Chestionar2, by="cod")
data = merge(data, Chestionar3, by="cod")

#Intrebarile de control - ifelse

colnames(data)[49] = "control1"
summary(data$control1)
data = subset(data, data$control1 == 2)


colnames(data)[99] = "control2"
summary(data$control2)
data = subset(data, data$control2 == 2)

colnames(data)[150] = "control3"
summary(data$control3)
data = subset(data, data$control3 == 2)

colnames(data)[180] = "control4"
summary(data$control4)
data = subset(data, data$control4 == 4)

#Prepocesare - Scala SATS

colnames(data) #coloanele 37 - 65 corespund scalei SATS (vezi documentul word cu scalele)
colnames(data)[37] = "st1"
colnames(data)[38] = "st2"
colnames(data)[39] = "st3"
colnames(data)[40] = "st4"
colnames(data)[41:48] = c("st5", "st6", "st7","st8","st9","st10","st11","st12")
colnames(data)[50:65] = c("st13","st14","st15","st16","st17","st18","st19","st20","st21","st22","st23","st24","st25","st26","st27","st28")
colnames(data)

data$SATS = data$st1 +
  (8-data$st2)+
  (8-data$st3)+
  data$st4+
  (8-data$st5)+
  (8-data$st6)+
  data$st7+
  data$st8+
  (8-data$st9)+
  (8-data$st10)+
  (8-data$st11)+
  (8-data$st12)+
  data$st13+
  (8-data$st14)+
  data$st15+
  (8-data$st16)+
  data$st17+
  (8-data$st18)+
  (8-data$st19)+
  (8-data$st20)+
  (8-data$st21)+
  (8-data$st22)+
  data$st23+
  data$st24+
  (8-data$st25)+
  (8-data$st26)+
  (8-data$st27)+
  (8-data$st28)

data$SATS = data$SATS/28

summary(data$SATS)
hist(data$SATS)

#CRT

colnames(data)[66] = "crt1"
colnames(data)[67] = "crt2"
colnames(data)[68] = "crt3"

summary(data$crt1)
summary(as.factor(data$crt1))
summary(as.factor(data$crt2))
summary(as.factor(data$crt3))

data$crt1 = ifelse(data$crt1 == 2.25, 1, 0)
data$crt2 = ifelse(data$crt2 == 5, 1, 0)
data$crt3 = ifelse(data$crt3 == 47, 1, 0)

summary(as.factor(data$crt1))
summary(as.factor(data$crt2))
summary(as.factor(data$crt3))

data$CRT = data$crt1 + data$crt2 + data$crt3

summary(data$CRT)
hist(data$CRT)
barplot(table(data$CRT))


#CRT extins

colnames(data)[69] = "crt4"
colnames(data)[70] = "crt5"
colnames(data)[71] = "crt6"
colnames(data)[72] = "crt7"

summary(as.factor(data$crt4))
summary(as.factor(data$crt5))
summary(as.factor(data$crt6))
summary(as.factor(data$crt7))

data$crt4 = ifelse(data$crt4 == 4, 1, 0)
data$crt5 = ifelse(data$crt5 == 29, 1, 0)
data$crt6 = ifelse(data$crt6 == 200, 1, 0)
data$crt7 = ifelse(data$crt7 == "mai puțin decât avea la început", 1, 0)

summary(as.factor(data$crt4))
summary(as.factor(data$crt5))
summary(as.factor(data$crt6))
summary(as.factor(data$crt7))

data$CRT_extins = data$crt4 + data$crt5 + data$crt6 + data$crt7

#Gambler's Fallacy

colnames(data)[73] = "crt8"
summary(as.factor(data$crt8))
data$crt8 = ifelse(data$crt8 == "Capul și pajura au șanse egale să apară la a șasea aruncare.", 1, 0)
summary(as.factor(data$crt8))
data$Gambler = data$crt8

#Conjunction Problem

colnames(data)[74] = "crt9"
summary(as.factor(data$crt9))
data$crt9 = ifelse(data$crt9 == "Linda este casieră la bancă și este activă în mișcarea feministă.", 1, 0)
summary(as.factor(data$crt9))
data$Conjunction = data$crt9

#Covariation

colnames(data)[75] = "crt10"
summary(as.factor(data$crt10))
data$crt10 = ifelse(data$crt10 < 5, 1, 0)
summary(as.factor(data$crt10))
data$Covariation = data$crt10

#Sylogistic

colnames(data)[76] = "crt11"
summary(as.factor(data$crt11))
data$crt11 = ifelse(data$crt11 == "Nu urmează", 1, 0)
summary(as.factor(data$crt11))

colnames(data)[77] = "crt12"
summary(as.factor(data$crt12))
data$crt12 = ifelse(data$crt12 == "Urmează", 1, 0)
summary(as.factor(data$crt12))

data$sylogistic = data$crt11 + data$crt12

#Numeracy - de terminat, doar primul crt

colnames(data)[78] = "crt13"
summary(as.factor(data$crt13))
data$crt13 = ifelse(data$crt13 == 500, 1, 0)
summary(as.factor(data$crt13))

colnames(data)[79] = "crt14"
summary(as.factor(data$crt14))
data$crt14 = ifelse(data$crt14 == 10, 1, 0)
summary(as.factor(data$crt14))

colnames(data)[80] = "crt15"
summary(as.factor(data$crt15))
data$crt15 = ifelse(data$crt15 == 0.1, 1, 0)
summary(as.factor(data$crt15))


colnames(data)[81] = "crt16"
summary(as.factor(data$crt16))
data$crt16 = ifelse(data$crt16 == "1 din 10", 1, 0)
summary(as.factor(data$crt16))

colnames(data)[82] = "crt17"
summary(as.factor(data$crt17))
data$crt17 = ifelse(data$crt17 == 0.1, 1, 0)
summary(as.factor(data$crt17))

colnames(data)[83] = "crt18"
summary(as.factor(data$crt18))
data$crt18 = ifelse(data$crt18 == 2, 1, 0)
summary(as.factor(data$crt18))

colnames(data)[84] = "crt19"
summary(as.factor(data$crt19))
data$crt19 = ifelse(data$crt19 == 100, 1, 0)
summary(as.factor(data$crt19))

colnames(data)[85] = "crt20"
summary(as.factor(data$crt20))
data$crt20 = ifelse(data$crt20 == 20, 1, 0)
summary(as.factor(data$crt20))

colnames(data)[86] = "crt21"
summary(as.factor(data$crt21))
data$crt21 = ifelse(data$crt21 == 5, 1, 0)
summary(as.factor(data$crt21))

data$Numeracy = data$crt13 + data$crt14 + data$crt15 + data$crt16 + data$crt17 + data$crt18 + data$crt19 + data$crt20 + data$crt21 

# Math self efficacy scale

colnames(data)[87:94] = c("crt22", "crt23", "crt24", "crt25", "crt26", "crt27", "crt28", "crt29")

data$SATS2 = data$crt22 + data$crt23 + data$crt24 + data$crt25 + data$crt26 + data$crt27 + data$crt28 + data$crt29
  
data$SATS2 = data$SATS2/8

summary(data$SATS2)
hist(data$SATS2)

# Statistics self efficacy scale

colnames(data)[95:98] = c("crt30", "crt31", "crt32", "crt33")

colnames(data)[100:103] = c("crt34", "crt35", "crt36", "crt37")

data$SATS3 = data$crt30 + data$crt31 + data$crt32 + data$crt33 + data$crt34 + data$crt35 + data$crt36 + data$crt37

data$SATS3 = data$SATS3/8

summary(data$SATS3)
hist(data$SATS3)

#Chestionar 3 - Pseudo-profound bullshit

colnames(data)[107:116] = c("crt38", "crt39", "crt40", "crt41", "crt42", "crt43", "crt44", "crt45", "crt46", "crt47")
data$SATS4 = data$crt38 + data$crt39 + data$crt40 + data$crt41 + data$crt42 + data$crt43 + data$crt44 + data$crt45 + data$crt46 + data$crt47

data$SATS4 = data$SATS4/10

summary(data$SATS4)
hist(data$SATS4)

#School engagement

colnames(data)[117:135] = c("crt48", "crt49", "crt50", "crt51", "crt52", "crt53", "crt54", "crt55", "crt56", "crt57", "crt58", "crt59", "crt60", "crt61", "crt62", "crt63", "crt64", "crt65", "crt66")
data$SATS5 = data$crt48 + (7-data$crt49) + (7-data$crt50) + data$crt51 
+ data$crt52 + data$crt53 + data$crt54 + data$crt55 + data$crt56 
+ data$crt57 + (7-data$crt58) + data$crt59 + data$crt60 + data$crt61 
+ data$crt62 + data$crt63 + data$crt64 + data$crt65 + data$crt66
data$SATS5 = data$SATS5/19

summary(data$SATS5)
hist(data$SATS5)

#Thinking dispositions

colnames(data)[136:138] = c("crt67", "crt68", "crt69")
data$SATS6 = data$crt67 + (6-data$crt68) + (6-data$crt69)
data$SATS6 = data$SATS6/3

summary(data$SATS6)
hist(data$SATS6)

#Superstitious thinking

colnames(data)[139:141] = c("crt70", "crt71", "crt72")
data$SATS7 = data$crt70 + data$crt71 + (6-data$crt72)
data$SATS7 = data$SATS7/3

summary(data$SATS7)
hist(data$SATS7)

data$SATS8 = data$SATS6 + data$SATS7

#Conspiracy ideation

colnames(data)[142:149] = c("crt73", "crt74", "crt75", "crt76", "crt77", "crt78", "crt79", "crt80")
colnames(data)[151:157] = c("crt81", "crt82" , "crt83","crt84","crt85","crt86","crt87")

data$SATS9 = data$crt73 + data$crt74 + data$crt75 + data$crt76 + data$crt77 + data$crt78 + data$crt79 + data$crt80 + data$crt81 + data$crt82 + data$crt83 + data$crt84 + data$crt85 + data$crt86 + data$crt87
data$SATS9 = data$SATS9/15

summary(data$SATS9)
hist(data$SATS9)


#Life orientation test
colnames(data)[158:169] = c("crt88", "crt89" , "crt90", "crt91", "crt92", "crt93", "crt94" , "crt95", "crt96", "crt97", "crt98", "crt99") 

data$SATS10 = data$crt88 + (6-data$crt90) + data$crt91 + data$crt92 + (6-data$crt95) + (6-data$crt96) + data$crt98 + (6-data$crt99)
data$SATS10 = data$SATS10/12

summary(data$SATS10)
hist(data$SATS10)

#Resilience scale

colnames(data)[170:179] = c("crt100", "crt101" , "crt102", "crt103", "crt104", "crt105", "crt106" , "crt107", "crt109", "crt110") 
colnames(data)[181:193] = c("crt111", "crt112" , "crt113", "crt114", "crt115", "crt116", "crt117", "crt118" , "crt119", "crt120", "crt121", "crt122", "crt123")

data$SATS11 = data$crt100 + data$crt101 + data$crt102 + data$crt103 + data$crt104 + data$crt105
+ data$crt106 + data$crt107 + data$crt108 + data$crt109 + data$crt110 + data$crt111 + data$crt112
+ data$crt113 + data$crt114 + data$crt115 + data$crt116 + data$crt117 + data$crt118 + data$crt119 + data$crt120 + data$crt121 + data$crt122 + data$crt123
data$SATS11 = data$SATS11/23

summary(data$SATS11)
hist(data$SATS11)



#Sters coloane
data = subset(data, select = -c(1:6))
data = subset(data, select = -c(43))
data = subset(data, select = -c(93))
data = subset(data, select = -c(98:100))
data = subset(data, select = -c(37))
data = subset(data, select = -c(85))
data = subset(data, select = -c(89:90))
data = subset(data, select = -c(129))
data = subset(data, select = -c(158))
data = subset(data, select = -c(22:24))


#Scriere set date
write.csv(data, file ="date_prelucrate1")
library(openxlsx)
write.xlsx(data,"date_prelucrare_excel1_echipa_racheta.xlsx")
