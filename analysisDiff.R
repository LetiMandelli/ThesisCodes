# create column diff between MV and TF

rm(list=ls())
install.packages("aod")
library(texreg)
library(lme4)
library(lattice)
require(leaps)
library(xtable)
library(stargazer)
library(MASS)
library(MuMIn)
library(stargazer)
library(aod)
library(ggplot2)
install.packages("pscl")
library(pscl)

df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')
df5 <- df[which(df$noImp > 4),]


df$age2 <- (df$Age)^2

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


bestsubset <- regsubsets(diff ~  role + age2 + openB + noMatch1Y 
                           + min1Y + actionS1Y + goal1Y 
                           + assist1Y
                           +shotS1Y + xg1Y +passA1Y
                           +lpassA1Y
                           +crossA1Y+dribbS1Y
                           + duelS1Y+aerDuelS1Y 
                           + interc1Y + recOpp1Y + recT1Y
                           + losOwn1Y + yell1Y + losT1Y + red1Y
                           + dB + dC + goalT1Y
                           +passT1Y+passAT1Y+possT1Y+goalAbsT1Y
                           +xgT1Y
                           +losTot1Y+lowLosT1Y
                           +recTot1Y+shotST1Y
                           +highRecT1Y
                           +challT1Y
                           +challST1Y
                           +goalOppT1Y+countryB+countryS, data=df, really.big = T)

df$TT <-as.factor(df$TT)

plot(bestsubset)

fit1 <- lm(diff ~  duelS1Y + goal1Y + age2 + openB + yell1Y + dB + countryBGB, data=df)
# img 54




# diff = TF-MV
fit2 <- lm(diff ~  duelS1Y + assist1Y + goal1Y + age2 + openB + yell1Y + dB + countryBGB, data=df)
fit3 <- lm(diff ~ noMatch1Y + xg1Y + actionS1Y + passA1Y + age2 + dC+ dB + d20 + euroTB
            + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y, data=df)


summary(fit3)

stargazer(fit1,fit2, fit3)
AIC(fit1,fit2, fit3)$AIC
AICc(fit1,fit2, fit3)$AICc
BIC(fit1,fit2, fit3)$BIC

plot(fit1, labels.id = df$name_final)
plot(fit2, labels.id = df$name_final)
plot(fit3, labels.id = df$name_final)



#######################################
# logit

df$over <- ifelse(df$price_tm > df$MV, 1, 0)


mean(df$over)

# con variabili del mio modello
mylogit <- glm(over ~ noMatch1Y + xg1Y + actionS1Y + passA1Y + age2 + dC+ dB + d20 + euroTB
               + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y, data = df, family = "binomial")
summary(mylogit)
confint(mylogit)

#anova(mylogit, test="Chisq")

pR2(mylogit)

#pseudo R2 di 0.15


fullmod <- glm(over ~  role + age2 + openB + noMatch1Y 
                         + min1Y + actionS1Y + goal1Y 
                         + assist1Y
                         +shotS1Y + xg1Y +passA1Y
                         +lpassA1Y
                         +crossA1Y+dribbS1Y
                         + duelS1Y+aerDuelS1Y 
                         + interc1Y + recOpp1Y + recT1Y
                         + losOwn1Y + yell1Y + losT1Y + red1Y
                         + dB + dC + goalT1Y
                         +passT1Y+passAT1Y+possT1Y+goalAbsT1Y
                         +xgT1Y
                         +losTot1Y+lowLosT1Y
                         +recTot1Y+shotST1Y
                         +highRecT1Y
                         +challT1Y
                         +challST1Y
                         +goalOppT1Y+countryB+countryS, data = df, family = "binomial")

# model backwards 1
backwards = step(fullmod) # Backwards selection is the default
summary(selected)


back_CBmodified <-  glm(over ~ age2 + lpassA1Y + 
                          recOpp1Y + yell1Y + highRecT1Y + challT1Y + 
                          goalOppT1Y + countryBGB + countryBFR,  data = df, family = "binomial")
# model 2 selezionato a mano più semplice ma con AIC perggiore del precedente, BIC migliore
summary(back_CBmodified)


AIC(selected)
AIC(back_CBmodified)
BIC(selected)
BIC(back_CBmodified)

# sono comunque tutti pessimi modelli ma sicuramente si vede che quando acquista la gran bretagna c'è significativo overpaid

onlyGBage <-  glm(over ~ countryBGB + age2,  data = df, family = "binomial")

stargazer(mylogit, selected, back_CBmodified,onlyGBage)
AIC(mylogit, selected, back_CBmodified, onlyGBage)$AIC
AICc(mylogit, selected, back_CBmodified, onlyGBage)$AICc
BIC(mylogit, selected, back_CBmodified, onlyGBage)$BIC
pR2(mylogit)
pR2(selected)
pR2(back_CBmodified)
pR2(soloGB)

plot(mylogit)
