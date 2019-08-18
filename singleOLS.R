# First analysis

dtv <- read.csv('TVrights201617.csv', header = TRUE, sep = ',')
xtable(dtv)


# OLS on one variable

df <- read.csv('data_models.csv', header = TRUE, sep = ',')
library(xtable)
library(stargazer)

stargazer(df, style = 'aer')

df$dA <- as.factor(df$dA)
df$dB <- as.factor(df$dB)
df$dC <- as.factor(df$dC)
df$d5 <- as.factor(df$d5)
df$d10 <- as.factor(df$d10)
df$d20 <- as.factor(df$d20)
sum1 <-summary(df[c('TF','TT', 'countryS','countryB','role')])
sum2 <-summary(df[c('season','ws','foot','div','d5','d10','d20', 'dA','dB', 'dC')])

xtable(sum1)
xtable(sum2)
stargazer(df[,c('Age','height','weight','noMatch1Y','min1Y','actionS1Y','goal1Y', 'assist1Y','shotS1Y', 
            'xg1Y','passA1Y','lpassA1Y','crossA1Y','dribbS1Y','duelS1Y','aerDuelS1Y','interc1Y',
            'recOpp1Y','recT1Y','losOwn1Y','losT1Y','yell1Y','red1Y', 'openB','liq')])
stargazer(df[c('shotsT1Y','shotST1Y','xgT1Y','goalT1Y','passT1Y','passAT1Y','possT1Y','losT1Y','lowLosT1Y','recT1Y','highRecT1Y',
            'challT1Y','challST1Y','goalOppT1Y','goalAbsT1Y','euroTS','euroTB','euroCS','euroCB','noExp','noImp','noCS','noCB')])

summary(df$euroCB)
sqrt(var(df$euroCB))

summary(df$price)

fit1 <- lm(price ~ d18, data = df)
summary(fit1)

fit1 <- lm(price ~ height, data = df)
summary(fit1)

fit1 <- lm(price ~ weight, data = df)
summary(fit1)


fit1 <- lm(price ~ ws, data = df)
summary(fit1)

fit1 <- lm(price ~ role, data = df)
summary(fit1)

fit1 <- lm(price ~ countryS, data = df)
summary(fit1)

fit1 <- lm(price ~ countryB, data = df)
summary(fit1)

fit1 <- lm(price ~ birth, data = df)
summary(fit1)

fit1 <- lm(price ~ foot, data = df)
summary(fit1)

fit1 <- lm(price ~ euroTS, data = df)
summary(fit1)

fit1 <- lm(price ~ euroTB, data = df)
summary(fit1)

fit1 <- lm(price ~ openB, data = df)
summary(fit1)

fit1 <- lm(price ~ euroCS, data = df)
summary(fit1)

fit1 <- lm(price ~ euroCB, data = df)
summary(fit1)

fit1 <- lm(price ~ noExp, data = df)
summary(fit1)

fit1 <- lm(price ~ noImp, data = df)
summary(fit1)

fit1 <- lm(price ~ noCS, data = df)
summary(fit1)

fit1 <- lm(price ~ noCB, data = df)
summary(fit1)

fit1 <- lm(price ~ noMatch1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ Age, data = df)
summary(fit1)

df$Age2 = (df$Age)^2

fit1 <- lm(price ~ Age2 , data = df)
summary(fit1)

fit1 <- lm(price ~ min1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ actionS1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ goal1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ assist1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ shotS1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ xg1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ passA1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ lpassA1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ crossA1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ dribbS1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ duelS1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ aerDuelS1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ interc1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ recOpp1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ recT1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ losOwn1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ losT1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ yell1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ red1Y, data = df)
summary(fit1)

fit1 <- lm(price ~ shotT, data = df)
summary(fit1)

fit1 <- lm(price ~ shotST, data = df)
summary(fit1)

fit1 <- lm(price ~ xgT, data = df)
summary(fit1)

fit1 <- lm(price ~ goalT, data = df)
summary(fit1)

fit1 <- lm(price ~ passT, data = df)
summary(fit1)

fit1 <- lm(price ~ passAT, data = df)
summary(fit1)

fit1 <- lm(price ~ possT, data = df)
summary(fit1)

fit1 <- lm(price ~ losT, data = df)
summary(fit1)
fit1 <- lm(price ~ lowLosT, data = df)
summary(fit1)
fit1 <- lm(price ~ recT, data = df)
summary(fit1)
fit1 <- lm(price ~ highRecT, data = df)
summary(fit1)
fit1 <- lm(price ~ challT, data = df)
summary(fit1)
fit1 <- lm(price ~ challST, data = df)
summary(fit1)
fit1 <- lm(price ~ goalOppT, data = df)
summary(fit1)
fit1 <- lm(price ~ goalAbsT, data = df)
summary(fit1)
fit1 <- lm(price ~ liq, data = df)
summary(fit1)
fit1 <- lm(price ~ more1t, data = df)
summary(fit1)
fit1 <- lm(price ~ dA, data = df)
summary(fit1)
fit1 <- lm(price ~ dB, data = df)
summary(fit1)
fit1 <- lm(price ~ dC, data = df)
summary(fit1)
fit1 <- lm(price ~ d5, data = df)
summary(fit1)
fit1 <- lm(price ~ d10, data = df)
summary(fit1)
fit1 <- lm(price ~ d20, data = df)
summary(fit1)
fit1 <- lm(price ~ dSheikh, data = df)
summary(fit1)



