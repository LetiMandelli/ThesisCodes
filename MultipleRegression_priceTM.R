#---------------------------------------------------------#
# OLS no log
#---------------------------------------------------------#

rm(list=ls())

require(leaps)
library(stargazer)
df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')

df$countryB <- as.factor(df$countryB) 
df$countryS <- as.factor(df$countryS)

df$age2 = (df$Age)^2

df$countryBGB <- ifelse(df$countryB == "GB", 1, 0)
df$countrySGB <- ifelse(df$countryS == "GB", 1, 0)
df$countryBFR <- ifelse(df$countryB == "FR", 1, 0)
df$countrySNL <- ifelse(df$countryS == "NL", 1, 0)


# 7
lm.fit7 <- lm(price_tm ~ age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC, data = df)

# 17
lm.fit17 = lm(price_tm ~  age2+openB+min1Y+noMatch1Y+actionS1Y+xg1Y+dB+dC 
              + goalAbsT1Y + passAT1Y, data = df)

lm.fit19 = lm(price_tm ~  age2+openB+noMatch1Y+xg1Y+dB+dC+actionS1Y 
              + goalAbsT1Y, data = df)
  
  
lm.fit27 = lm(price_tm ~  age2+openB+noMatch1Y+actionS1Y+xg1Y+dB+dC
                         +goalAbsT1Y+d20
                         +lowLosT1Y+countryBGB+countryBFR, data = df)

  
plot(lm.fit27, labels.id = df$name_final)

a <- AIC(lm.fit7, lm.fit17,lm.fit19, lm.fit27)
b <- BIC(lm.fit7, lm.fit17,lm.fit19, lm.fit27)
round(a$AIC,0)
round(AICc(lm.fit7, lm.fit17,lm.fit19, lm.fit27)$AICc,0)
round(b$BIC,0)


stargazer(lm.fit7, lm.fit17,lm.fit19, lm.fit27, title="Multiple regression results",
          type="latex",single.row=F,
          ci=F, omit.stat=c("f", "ser"), no.space = T)







