#---------------------------------------------------------#
# OlS logaritmi
#---------------------------------------------------------#
rm(list=ls())
library(tree)

require(leaps)
library(stargazer)
#setwd("C:/Users/letym/Desktop/15.Analysis_new")
df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')


df$age2 = (df$Age)^2

lm.fit1 = lm(log(price_tm) ~  role + age2 + openB + noMatch1Y + xg1Y +dB+dC , data = df)

lm.fit2 = lm(log(price_tm) ~  role + age2 + openB + noMatch1Y 
             + min1Y + actionS1Y + goal1Y + assist1Y
             +shotS1Y # + xg1Y + passA1Y 
             + lpassA1Y
             #+crossA1Y#+dribbS1Y
             #+duelS1Y#+aerDuelS1Y
             #+interc1Y
             +recOpp1Y+recTot1Y
             +losOwn1Y+yell1Y+losTot1Y+red1Y
             +dB+dC, data=df )


lm.fit3 = lm(log(price_tm) ~  role + age2 + openB + noMatch1Y 
             + min1Y + actionS1Y #+ goal1Y 
             + assist1Y
             +shotS1Y + xg1Y #+passA1Y
             +lpassA1Y
             #+crossA1Y#+dribbS1Y
             #+duelS1Y#+aerDuelS1Y
             #+interc1Y
             +recOpp1Y+recTot1Y
             +losOwn1Y+yell1Y+losTot1Y+red1Y
             +dB+dC, data=df)


lm.fit4 = lm(log(price_tm) ~  role + age2 + openB + noMatch1Y 
             + min1Y + actionS1Y #+ goal1Y 
             + assist1Y
             +shotS1Y + xg1Y #+passA1Y
             +lpassA1Y
             #+crossA1Y#+dribbS1Y
             #+duelS1Y#+aerDuelS1Y
             +interc1Y
             +recOpp1Y#+recTot1Y
             +losOwn1Y+yell1Y+losTot1Y+red1Y
             +dB+dC, data=df)

lm.fit5 = lm(log(price_tm) ~  role + age2 + openB + noMatch1Y 
             + min1Y #+ actionS1Y #+ goal1Y 
             + assist1Y
             + shotS1Y + xg1Y +passA1Y
             + lpassA1Y
             #+crossA1Y#+dribbS1Y
             #+duelS1Y#+aerDuelS1Y
             +interc1Y 
             +recOpp1Y #+recTot1Y
             +losOwn1Y+yell1Y+losTot1Y+red1Y
             +dB+dC, data=df)

lm.fit6 = lm(log(price_tm) ~  role + age2 + openB + noMatch1Y 
             + min1Y + actionS1Y #+ goal1Y 
             + assist1Y
             +shotS1Y + xg1Y #+passA1Y
             +lpassA1Y
             +crossA1Y+dribbS1Y
             #+duelS1Y
             +aerDuelS1Y
             +interc1Y
             +recOpp1Y#+recTot1Y
             +losOwn1Y+yell1Y+losTot1Y+red1Y
             +dB+dC, data=df)


# metto tutte le variabili anche quelle collinari e gli faccio selezionare
fit.bestsall <- regsubsets(log(price_tm) ~  role + age2 + openB + noMatch1Y 
                         + min1Y + actionS1Y + goal1Y 
                         + assist1Y
                         +shotS1Y + xg1Y +passA1Y
                         +lpassA1Y
                         +crossA1Y+dribbS1Y
                         +duelS1Y+aerDuelS1Y
                         +interc1Y+recOpp1Y+recTot1Y
                         +losOwn1Y+yell1Y+losTot1Y+red1Y
                         +dB+dC, data=df)

plot(fit.bestsall)

lm.fit.best <- lm(log(price_tm) ~ age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC, data = df)

library(MuMIn)
round(AIC(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5, lm.fit6,lm.fit.best)$AIC,0)
round(AICc(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5, lm.fit6,lm.fit.best)$AICc,0)
round(BIC(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5, lm.fit6,lm.fit.best)$BIC,0)

stargazer(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5, lm.fit6,lm.fit.best,title="Multiple regression results",
          type="latex",single.row=F,
          ci=TRUE, ci.level=0.95, omit.stat=c("f", "ser"), no.space = T)

#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#

# lm.fit4 = lm(log(price) ~  role+age2+openB
#              +noMatch1Y+min1Y+actionS1Y
#              +goal1Y+assist1Y+shotS1Y+xg1Y
#              +duelS1Y+xgT1Y+dB+dC, data = df)

# y=log(df$price)
# fit_tree4 <- tree(y ~ role+age2+openB
#                   +noMatch1Y+min1Y+actionS1Y
#                   +goal1Y+assist1Y+shotS1Y+xg1Y
#                   +duelS1Y+xgT1Y+dB+dC, data=df, split="deviance")
# plot(fit_tree4)
# text(fit_tree4)

# # tende a schiacciare tutto e non cattura le code, per cui per gli outlier non va bene

# parto dal best e gli aggiungo le performance della team
lm.fit7 = lm(log(price_tm) ~  #age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
             #+shotT1Y # + goalT1Y + xgT1Y
             #+passT1Y # 
             + passAT1Y #+ possT1Y
             +goalAbsT1Y
             #+losTot1Y
             +lowLosT1Y
             #+recTot1Y
             #+shotST1Y
             +highRecT1Y
             +challT1Y
             #+challST1Y
             +goalOppT1Y, data = df)

lm.fit8 = lm(log(price_tm) ~  #age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
             #+shotT1Y  + 
               goalT1Y #+ xgT1Y
             #+passT1Y # 
             + passAT1Y #+ possT1Y
             #+goalAbsT1Y
             #+losTot1Y
             +lowLosT1Y
             #+recTot1Y
             #+shotST1Y
             +highRecT1Y
             +challT1Y
             #+challST1Y
             +goalOppT1Y, data = df)

lm.fit9 = lm(log(price_tm) ~  #age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
             #+shotT1Y #+ goalT1Y 
             + xgT1Y
             #+passT1Y # 
             + passAT1Y #+ possT1Y
             #+goalAbsT1Y
             #+losTot1Y
             +lowLosT1Y
             #+recTot1Y
             #+shotST1Y
             +highRecT1Y
             +challT1Y
             #+challST1Y
             +goalOppT1Y, data = df)

lm.fit10 = lm(log(price_tm) ~  #age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
               #+shotT1Y #+ goalT1Y 
               #+ xgT1Y
             #+passT1Y # 
             passAT1Y #+ possT1Y
             +goalAbsT1Y
             #+losTot1Y
             +lowLosT1Y
             +recTot1Y
             #+shotST1Y
             #+highRecT1Y
             +challT1Y
             #+challST1Y
             +goalOppT1Y, data = df)

lm.fit11 = lm(log(price_tm) ~  #age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
             passAT1Y + goalAbsT1Y
             +losTot1Y+recTot1Y
             +challT1Y+goalOppT1Y, data = df)

# # meglio 12!!
# lm.fit12 = lm(log(price) ~  
#              passAT1Y +goalAbsT1Y
#              +losTot1Y
#              +recTot1Y+challT1Y
#              +goalOppT1Y, data = df)

lm.fit13 = lm(log(price_tm) ~  #age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
                passAT1Y + goalAbsT1Y
              +losTot1Y +goalOppT1Y, data = df)

lm.fit14 = lm(log(price_tm) ~  passAT1Y +goalAbsT1Y
              +losTot1Y + challT1Y + goalOppT1Y, data = df)


lm.fit15 = lm(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
              + passAT1Y +goalAbsT1Y
              + losTot1Y + challT1Y + goalOppT1Y, data = df)


lm.fit16 = lm(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
              + goalAbsT1Y, data = df)

lm.fit17 = lm(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
              + goalAbsT1Y + passAT1Y, data = df)


fit.bestteam <- regsubsets(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC
                                +losTot1Y
                                +goalT1Y
                                +passT1Y+passAT1Y+possT1Y+goalAbsT1Y
                                +xgT1Y
                                +losTot1Y+lowLosT1Y
                                +recTot1Y+shotST1Y
                                +highRecT1Y
                                +challT1Y
                                +challST1Y
                                +goalOppT1Y
                                +dB+dC, data = df)

plot(fit.bestteam)
lm.fit.bestteam = lm(log(price_tm) ~  age2+openB+noMatch1Y+ xg1Y+dB+dC
              +goalAbsT1Y+ shotST1Y, data = df)

lm.fit.bestteam2 = lm(log(price_tm) ~  age2+openB+noMatch1Y+ xg1Y+dB+dC
                     +passAT1Y+ highRecT1Y, data = df)

#lm.fit18 = lm(log(price_tm) ~  age2+openB+noMatch1Y+xg1Y+dB+dC+actionS1Y 
#              + goalAbsT1Y, data = df)

lm.fit19 = lm(log(price_tm) ~  age2+openB+noMatch1Y+xg1Y+dB+dC+ passAT1Y, data = df)


a <- AICc(lm.fit7, lm.fit8, lm.fit9, lm.fit10, lm.fit11,lm.fit13,lm.fit14,lm.fit15, lm.fit16, lm.fit17,lm.fit.bestteam,lm.fit.bestteam2, #lm.fit18, 
          lm.fit19)
b <- BIC(lm.fit7, lm.fit8, lm.fit9, lm.fit10, lm.fit11,lm.fit13,lm.fit14,lm.fit15, lm.fit16, lm.fit17,lm.fit.bestteam,lm.fit.bestteam2, #lm.fit18, 
         lm.fit19)
round(AIC(lm.fit7, lm.fit8, lm.fit9, lm.fit10, lm.fit11,lm.fit13,lm.fit14,lm.fit15, lm.fit16, lm.fit17,lm.fit.bestteam,lm.fit.bestteam2, #lm.fit18, 
          lm.fit19)$AIC,0)
round(a$AICc,0)
round(b$BIC,0)



stargazer(lm.fit7, lm.fit8, lm.fit9, lm.fit10, lm.fit11,lm.fit13,lm.fit14,lm.fit15, lm.fit16, lm.fit17,lm.fit.bestteam, lm.fit18,lm.fit19,title="Multiple regression results",
          type="latex",single.row=F,
          ci=F, ci.level=0.95, omit.stat=c("f", "ser"), no.space = T)

#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#

df$countryB <- as.factor(df$countryB) 
df$countryS <- as.factor(df$countryS)


df$countryBGB <- ifelse(df$countryB == "GB", 1, 0)
df$countryBFR <- ifelse(df$countryB == "FR", 1, 0)
df$countryBRU <- ifelse(df$countryB == "RU", 1, 0)
df$countryBIT <- ifelse(df$countryB == "IT", 1, 0)
df$countryBES <- ifelse(df$countryB == "ES", 1, 0)
df$countryBPT <- ifelse(df$countryB == "PT", 1, 0)
df$countryBTR <- ifelse(df$countryB == "TR", 1, 0)
df$countryBGR <- ifelse(df$countryB == "GR", 1, 0)
df$countryBNL <- ifelse(df$countryB == "NL", 1, 0)

df$countrySGB <- ifelse(df$countryS == "GB", 1, 0)
df$countrySNL <- ifelse(df$countryS == "NL", 1, 0)
df$countrySIT <- ifelse(df$countryS == "IT", 1, 0)
df$countrySES <- ifelse(df$countryS == "ES", 1, 0)
df$countrySFR <- ifelse(df$countryS == "FR", 1, 0)
df$countrySBE <- ifelse(df$countryS == "BE", 1, 0)
df$countrySPT <- ifelse(df$countryS == "PT", 1, 0)

lm.fit20 = lm(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
              +goalAbsT1Y+more1t+liq+d20+euroCS+euroCB+countrySGB + countrySNL + countrySIT + countrySES + countrySFR+ countrySBE+ countrySPT+ countryBGB + countryBFR + countryBRU + countryBIT + countryBES + countryBPT + countryBTR + countryBGR+ countryBNL, data = df)


lm.fit21 = lm(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
              +goalAbsT1Y+more1t+liq+d10+euroCS+euroCB+countrySGB + countrySNL + countrySIT + countrySES + countrySFR+ countrySBE+ countrySPT+ countryBGB + countryBFR + countryBRU + countryBIT + countryBES + countryBPT + countryBTR + countryBGR+ countryBNL, data = df)

# non provo neanche d5 e dummy sheikh, vedo se metto euroTB al posto di dummy 20 se è meglio o peggio
lm.fit22 = lm(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
              +goalAbsT1Y+more1t+liq+euroTB+euroCS+euroCB+countrySGB + countrySNL + countrySIT + countrySES + countrySFR+ countrySBE+ countrySPT+ countryBGB + countryBFR + countryBRU + countryBIT + countryBES + countryBPT + countryBTR + countryBGR+ countryBNL, data = df)

# euro Ts non la aggiungo perchè ha corr molto alta con goalAbsT1Y
# creo delle dummy così da semplificare il modello perchè anche dai boxplot vedo che probabilmente molto country non hanno influenza, lo vedo anche dal modello prima

stargazer(lm.fit25, ci.level = 0.99, ci = T)


lm.fit23 = lm(log(price_tm) ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
                +goalAbsT1Y+more1t+liq#+euroTB
                +d20
                +euroCS+euroCB+#+euroTS
                +countryBGB+countrySGB+countryBFR#+countryBRU
              +countrySNL
                , data = df)

lm.fit24 = lm(log(price_tm) ~  age2+openB#+min1Y
              +noMatch1Y+actionS1Y+xg1Y+dB+dC 
              +goalAbsT1Y+d20
              #+euroCS
              +euroCB+#+euroTS
              +countryBGB+countrySGB+countryBFR#+countryBRU
              +countrySNL
              , data = df)


lm.fit25 = lm(log(price_tm) ~  age2+noMatch1Y+actionS1Y+xg1Y+dB+dC
              +goalAbsT1Y+d20
              +countryBGB+countrySGB+countryBFR #+countryBRU
              +countrySNL
              , data = df)

lm.fit26 = lm(log(price_tm) ~  age2+noMatch1Y+actionS1Y+xg1Y+dB+dC
              +d20
              +countryBGB+countrySGB+countryBFR+countrySNL
              , data = df)

lm.fit27 = lm(log(price_tm) ~  age2+openB+noMatch1Y+actionS1Y+xg1Y+dB+dC
              +goalAbsT1Y+d20
              +lowLosT1Y+countryBGB+countryBFR, data = df)

lm.fit28 = lm(log(price_tm) ~  age2+openB+noMatch1Y+actionS1Y+xg1Y+dB+dC
              +goalAbsT1Y+d20
              +lowLosT1Y, data = df)



a <- AIC(lm.fit20, lm.fit21, lm.fit22, lm.fit23,lm.fit24, lm.fit25, lm.fit26, lm.fit27, lm.fit28)
b <- AICc(lm.fit20, lm.fit21, lm.fit22, lm.fit23,lm.fit24, lm.fit25, lm.fit26, lm.fit27, lm.fit28)
c <- BIC(lm.fit20, lm.fit21, lm.fit22, lm.fit23,lm.fit24, lm.fit25, lm.fit26, lm.fit27, lm.fit28)
round(a$AIC,0)
round(b$AICc,0)
round(c$BIC,0)


stargazer(lm.fit20, lm.fit21, lm.fit22, lm.fit23,lm.fit24, lm.fit25, lm.fit26, lm.fit27, lm.fit28, title="Multiple regression results",
          type="latex",single.row=F,
          ci=F, omit.stat=c("f", "ser"), no.space = T)

#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#




plot(lm.fit26, labels.id = df$name_final)
