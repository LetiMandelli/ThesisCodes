# Multilevel

rm(list=ls())
#install.packages('texreg')
library(texreg)
library(lme4)
library(lattice)
require(leaps)
library(xtable)
library(stargazer)
library(MASS)
library(MuMIn)
library(EnvStats)
library(texreg)
#install.packages("optimx")
library(optimx)


setwd("C:/Users/zo139907/Desktop/15. Analysis")
df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')
df5 <- df[which(df$noExp > 4),]


nullmodel <- lmer(log(price_tm) ~ (1 | TF),  data = df5, REML = FALSE)
fit <- lm(log(price_tm)~ 1, data = df5)
-2*logLik(fit)+2*logLik(nullmodel) 

df5$age2 <- (df5$Age)^2

df5$countryBGB <- ifelse(df5$countryB == "GB", 1, 0)
df5$countryBFR <- ifelse(df5$countryB == "FR", 1, 0)
df5$countryBRU <- ifelse(df5$countryB == "RU", 1, 0)
df5$countryBIT <- ifelse(df5$countryB == "IT", 1, 0)
df5$countryBES <- ifelse(df5$countryB == "ES", 1, 0)
df5$countryBPT <- ifelse(df5$countryB == "PT", 1, 0)
df5$countryBTR <- ifelse(df5$countryB == "TR", 1, 0)
df5$countryBGR <- ifelse(df5$countryB == "GR", 1, 0)
df5$countryBNL <- ifelse(df5$countryB == "NL", 1, 0)

df5$countrySGB <- ifelse(df5$countryS == "GB", 1, 0)
df5$countrySNL <- ifelse(df5$countryS == "NL", 1, 0)
df5$countrySIT <- ifelse(df5$countryS == "IT", 1, 0)
df5$countrySES <- ifelse(df5$countryS == "ES", 1, 0)
df5$countrySFR <- ifelse(df5$countryS == "FR", 1, 0)
df5$countrySBE <- ifelse(df5$countryS == "BE", 1, 0)
df5$countrySPT <- ifelse(df5$countryS == "PT", 1, 0)

fit1 <- lmer(log(price_tm) ~ noMatch1Y  + (1 | TF), data = df5, REML = FALSE)
fit2 <- lmer(log(price_tm) ~ xg1Y + (1 | TF), data = df5, REML = FALSE)
fit3 <- lmer(log(price_tm) ~ actionS1Y + (1 | TF), data = df5, REML = FALSE)
fit4 <- lmer(log(price_tm) ~ I(age2/100) + (1 | TF), data = df5, REML = FALSE)
fit5 <- lmer(log(price_tm) ~ goalAbsT1Y + (1 | TF), data = df5, REML = FALSE)
fit6 <- lmer(log(price_tm) ~ euroTB + (1 | TF), data = df5, REML = FALSE)
fit7 <- lmer(log(price_tm) ~ d20 + (1 | TF), data = df5, REML = FALSE)
fit8 <- lmer(log(price_tm) ~  openB+ (1 | TF), data = df5, REML = FALSE)
fit9 <- lmer(log(price_tm) ~ countryBFR + (1 | TF), data = df5, REML = FALSE)
fit10 <- lmer(log(price_tm) ~ countryBGB + (1 | TF), data = df5, REML = FALSE)
fit11 <- lmer(log(price_tm) ~ countrySGB + (1 | TF), data = df5, REML = FALSE)
fit12 <- lmer(log(price_tm) ~ countrySNL + (1 | TF), data = df5, REML = FALSE)

texreg(list(nullmodel, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12))
texreg(nullmodel)
round(c(r.squaredGLMM(nullmodel)[1],r.squaredGLMM(fit1)[1],r.squaredGLMM(fit1)[1],
        r.squaredGLMM(fit3)[1],r.squaredGLMM(fit4)[1],r.squaredGLMM(fit5)[1],r.squaredGLMM(fit6)[1],
        r.squaredGLMM(fit7)[1],r.squaredGLMM(fit8)[1],r.squaredGLMM(fit9)[1],r.squaredGLMM(fit10)[1],r.squaredGLMM(fit11)[1],r.squaredGLMM(fit11)[1]),3)


round(c(r.squaredGLMM(nullmodel)[2],r.squaredGLMM(fit1)[2],r.squaredGLMM(fit2)[2],
        r.squaredGLMM(fit3)[2],r.squaredGLMM(fit4)[2],r.squaredGLMM(fit5)[2],r.squaredGLMM(fit6)[2],
        r.squaredGLMM(fit7)[2],r.squaredGLMM(fit8)[2],r.squaredGLMM(fit9)[2],r.squaredGLMM(fit10)[2],r.squaredGLMM(fit11)[2],r.squaredGLMM(fit12)[2]),3)

round(AICc(nullmodel, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12)$AICc,0)

#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
# confronto modelli con piu esplicative
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
# parto dal modello indicato come migliore che e` fit 13 e  aggiugno variabili

fit13  <- lmer(log(price_tm) ~ noMatch1Y + d20 + (1 | TF), data = df5, REML = FALSE)


fit14 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y 
             + openB + d20 + euroTB + (1 | TF), data = df5, REML = FALSE)

fit15 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y 
             + openB + d20 + countryBGB + euroTB + (1 | TF), data = df5, REML = FALSE)

fit16 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y 
             + openB + d20 + countryBGB + countryBFR + euroTB + (1 | TF), data = df5, REML = FALSE)

fit17 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y 
             + openB + d20 + countryBGB + countryBFR  + countrySNL + countrySGB + euroTB + (1 | TF), data = df5, REML = FALSE)

fit18 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y 
              + d20 + countryBGB + countryBFR  + countrySGB + euroTB + (1 | TF), data = df5, REML = FALSE)


texreg(list(fit13, fit14, fit15,fit16, fit17, fit18))

round(c(r.squaredGLMM(fit13)[1],r.squaredGLMM(fit14)[1],r.squaredGLMM(fit15)[1],
        r.squaredGLMM(fit16)[1],r.squaredGLMM(fit17)[1],r.squaredGLMM(fit18)[1]),3)
round(c(r.squaredGLMM(fit13)[2],r.squaredGLMM(fit14)[2],r.squaredGLMM(fit15)[2],
r.squaredGLMM(fit16)[2],r.squaredGLMM(fit17)[2],r.squaredGLMM(fit18)[2]),3)
round(AICc(fit13, fit14, fit15,fit16, fit17, fit18)$AICc,0)

# commento:  R2 that its a poor tool for model selection, since it almost always favors the most 
#complex models. If the goal is to select among the best models, an information criterion approach 
#(such as AIC or BIC) is preferred, since these indicators penalize for the number of predictors.
#
#"A general and simple method for obtaining R2 from generalized linear mixed-effects models" by Shinichi Nakagawa and Holger Schielzeth
# they have derived two easily interpretable values of R2 that address the above issues, specifically that of negative pseudo-R2s with more predictors, while still honoring the random structure of the data.

#The first is called the marginal R2 and describes the proportion of variance explained by the fixed factor(s) alone:
#The second is the conditional R2, which describes the proportion of variance explained by both the fixed and random factors:
#https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/


# metto anche le variabili del migliore modello lineare 


fit18 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y 
              + d20 + countryBGB + countryBFR  + countrySGB + euroTB + (1 | TF), data = df5, REML = FALSE)


fit19 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +dB + goalAbsT1Y
              + d20 + countryBGB + countryBFR  +countrySNL+ countrySGB + (1 | TF), data = df5, REML = FALSE)
# aggiunti dB, goalabsT1Y, countrySNL

#########################################################

fit21 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +dB + xgT1Y +euroTB
             + d20 + countryBGB + countryBFR  +countrySNL+ countrySGB + (1 | TF), data = df5, REML = FALSE)

################## provo a sostituire absGoalT1Y con xg e migliora
fit28 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +dB + xgT1Y+euroTB
              + d20 + countryBGB + countryBFR  +countrySNL+ countrySGB + (1 | TF), data = df5, REML = FALSE)
r.squaredGLMM(fit28)
r.squaredGLMM(fit21)
BIC(fit21, fit28)
########################################
# metto tutte le altre teams per vedere se qualcuna viene significativa
# fit22 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y
#               + d20 + countryBGB + countryBFR  + countrySGB  
#               + countryBRU + countryBIT + countryBES + countryBPT + countryBTR + countryBNL
#               + (1 | TF), data = df5, REML = FALSE)
# 
# fit23 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y  
#               + d20 + countryBGB + countryBFR  + countrySGB  
#               + countryBRU + countryBIT + countryBES + countryBPT + countryBTR + countryBNL
#               + countrySNL+countrySIT+countrySES+countrySFR+countrySBE+countrySPT
#               + (1 | TF), data = df5, REML = FALSE)
# 
# fit24 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y  
#               + d20 + countryBGB + countryBFR  + countrySGB  
#               + countryBRU + (1 | TF), data = df5, REML = FALSE)

fit19b <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +dB + xgT1Y
               + d20 + countryBGB + countryBFR  +countrySNL+ countrySGB + (1 | TF), data = df5, REML = FALSE)



#nonstante sia selezionata sempre la tolgo eprche' correlata con quelle che aggiungo ma vedo che in realta serve

fit22 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +xgT1Y+countryBRU
              + euroTB + d20 + countryBGB + countryBFR  + countrySGB  
              + countryBRU + countryBIT + countryBES + countryBPT + countryBTR + countryBNL
              + (1 | TF), data = df5, REML = FALSE)

fit23 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +xgT1Y+countryBRU
              + euroTB + d20 + countryBGB + countryBFR  + countrySGB  
              + countryBRU + countryBIT + countryBES + countryBPT + countryBTR + countryBNL
              + countrySNL+countrySIT+countrySES+countrySFR+countrySBE+countrySPT
              + (1 | TF), data = df5, REML = FALSE)

fit24 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +xgT1Y
              + euroTB + d20 + countryBGB + countryBFR  + countrySGB + (1 | TF), data = df5, REML = FALSE)

fit25<- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y + xgT1Y +euroTB
              + d20 + countryBGB + countryBFR + (1 | TF), data = df5, REML = FALSE)

fit20 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +xgT1Y
              + euroTB + d20+dB + countryBGB + countryBFR  + countrySGB + (1 | TF), data = df5, REML = FALSE)


texreg(list(fit19, fit19b, fit21,  fit24,fit20,fit25, fit22, fit23))
round(c(r.squaredGLMM(fit19)[1],r.squaredGLMM(fit19b)[1],r.squaredGLMM(fit21)[1],r.squaredGLMM(fit20)[1],
        r.squaredGLMM(fit24)[1],r.squaredGLMM(fit22)[1],r.squaredGLMM(fit23)[1],r.squaredGLMM(fit25)[1]),3)
round(c(r.squaredGLMM(fit19)[2],r.squaredGLMM(fit19b)[2],r.squaredGLMM(fit21)[1],r.squaredGLMM(fit20)[2],
        r.squaredGLMM(fit24)[2],r.squaredGLMM(fit22)[2],r.squaredGLMM(fit23)[2],r.squaredGLMM(fit25)[2]),3)

round(AICc(fit19, fit19b, fit21, fit24,fit25, fit22, fit23,  fit20)$AICc,0)




# random slope


fit26 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y +xgT1Y
              + euroTB + d20 + countryBGB + countryBFR   + (1 +xg1Y | TF), data = df5, REML = FALSE)



fit27 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y +xgT1Y + actionS1Y 
              + d20 + countryBGB + countryBFR  + countrySGB + euroTB + (1 + xg1Y| TF), data = df5, REML = FALSE)

fit28 <- lmer(log(price_tm) ~ noMatch1Y + I(age2/100) + xg1Y + actionS1Y 
              + d20 + countryBGB + countryBFR + countrySGB +  (1 + xg1Y| TF), data = df5, REML = FALSE)




texreg(list(fit19b,fit20, fit26, fit27, fit28))
round(c(r.squaredGLMM(fit18)[1],r.squaredGLMM(fit21)[1],r.squaredGLMM(fit25)[1],
        r.squaredGLMM(fit26)[1],r.squaredGLMM(fit27)[1]),3)

round(c(r.squaredGLMM(fit18)[2],r.squaredGLMM(fit21)[2],r.squaredGLMM(fit25)[2],
        r.squaredGLMM(fit26)[2],r.squaredGLMM(fit27)[2]),3)

round(AICc(fit18,fit21, fit25, fit26, fit27)$AICc,0)





#########################################################################


# best multilevel e best lm su d5

bestlm <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + dB + goalAbsT1Y +countrySGB + countrySNL + noMatch1Y + xg1Y + countryBGB + countryBFR, data = df5)
lmcontrollo <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB, data = df5)

bestmultil <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB + (1 | TF), data = df5, REML = FALSE)

texreg(list(bestlm,lmcontrollo, bestmultil))
round(AIC(bestlm, lmcontrollo,bestmultil)$AIC,2)
round(AICc(bestlm,lmcontrollo, bestmultil)$AICc,2)
round(BIC(bestlm,lmcontrollo, bestmultil)$BIC,2)

round(c(r.squaredGLMM(bestlm)[1],r.squaredGLMM(lmcontrollo)[1], r.squaredGLMM(bestmultil)[1]),2)
round(c(r.squaredGLMM(bestlm)[2],r.squaredGLMM(lmcontrollo)[2], r.squaredGLMM(bestmultil)[2]),2)



#---------------------------------------
# fixed effects TF

df5$TF <- as.factor(df5$TF)
df5$TT <- as.factor(df5$TT)
df5$countryB <- as.factor(df5$countryB)
df5$countryS <- as.factor(df5$countryS)


fixTT  <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB +TT, data = df5)
randTT <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB + (1 | TT), data = df5, REML = FALSE)
fixTTB <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y  + xgT1Y + countryB + euroTB +TT, data = df5)
randTTB <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + xgT1Y + countryB + euroTB + (1 | TT), data = df5, REML = FALSE)
fixTTS <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y  + xgT1Y + countryS + euroTB +TT, data = df5)
randTTS <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + xgT1Y + countryS + euroTB + (1 | TT), data = df5, REML = FALSE)
randomTFfixTT <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB + TT + (1 | TF), data = df5, REML = FALSE)



texreg(list(fixTT,randTT,fixTTB,randTTB, fixTTS, randTTS, randomTFfixTT))
round(AIC(fixTT,randTT,fixTTB,randTTB, fixTTS, randTTS, randomTFfixTT)$AIC,2)
round(AICc(fixTT,randTT,fixTTB,randTTB, fixTTS, randTTS, randomTFfixTT)$AICc,2)
round(BIC(fixTT,randTT,fixTTB,randTTB, fixTTS, randTTS, randomTFfixTT)$BIC,2)

# manca R2




fixTF <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB +TF, data = df5)
randTF <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB + (1 | TF), data = df5, REML = FALSE)
fixTFB <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y  + xgT1Y + countryB + euroTB +TF, data = df5)
randTFB <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + xgT1Y + countryB + euroTB + (1 | TF), data = df5, REML = FALSE)
fixTFS <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y  + xgT1Y + countryS + euroTB +TF, data = df5)
randTFS <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + xgT1Y + countryS + euroTB + (1 | TF), data = df5, REML = FALSE)
randomTTfixTF <- lmer(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB + TF + (1 | TT), data = df5, REML = FALSE)


texreg(list(fixTF,randTF,fixTFB,randTFB, fixTFS, randTFS, randomTTfixTF))
round(AIC(fixTF,randTF,fixTFB,randTFB, fixTFS, randTFS, randomTTfixTF)$AIC,2)
round(AICc(fixTF,randTF,fixTFB,randTFB, fixTFS, randTFS, randomTTfixTF)$AICc,2)
round(BIC(fixTF,randTF,fixTFB,randTFB, fixTFS, randTFS, randomTTfixTF)$BIC,2)

# manca R2







fixTTS <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y  + xgT1Y + countryS + euroTB +TT, data = df5)
fixTTS1 <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + assist1Y + noMatch1Y + xg1Y  + xgT1Y + countryS + euroTB +TT, data = df5)
fixTTS2 <- lm(log(price_tm) ~ actionS1Y + I(age2/100) + assist1Y + noMatch1Y + xg1Y  + xgT1Y + countryS + TT, data = df5)
texreg(list(fixTTS,fixTTS1,fixTTS2))
round(AIC(fixTTS,fixTTS1,fixTTS2)$AIC,2)
round(AICc(fixTTS,fixTTS1,fixTTS2)$AICc,2)
round(BIC(fixTTS,fixTTS1,fixTTS2)$BIC,2)






################################################################################
# check norm

# normality tests fit23
u0 <- ranef(bestmultil, postVar = TRUE)
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
v <- as.matrix(u0[[1]]) # vedo ogni squadra se ha influenza + o -

vett_u0 <- (attr(u0[[1]], "postVar"))[1,,]
TF <- (rownames(u0[[1]]))
u0tab <- cbind(TF, u0[[1]], u0se)
colnames(u0tab) <- c("TF", "u0", "u0se")
# riordino per valori crescenti di u0
u0tab <- u0tab[order(u0tab$u0), ]


# aggiungo una colonna che riempio coi rank: così ho la tabella completa con nazione e la stima degli u_j uno per uno
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
colnames(u0tab)[4] <- "u0rank" #rinomino
u0tab <- u0tab[order(u0tab$TF),  ] # importante ordinarli se no il grafico fa schifo

# vedo se u_j son normali
par <- fitdistr(u0tab$u0,'normal')
ks.test(u0tab$u0,'pnorm',  mean = par$estimate[1],  sd= par$estimate[2])

qqPlot(u0tab$u0, dist = "norm", 
       estimate.params = TRUE, digits = 2, points.col = "#208F8C", pch=19,
       add.line = TRUE, main='QQplot for u_0', xlab = 'Quantiles of normal', ylab = 'Quantiles of u_0')


# MODELLO NULLO
# eps <- residuals(nullmodel, level = 1, type = "response")
# 
# par2 <- fitdistr(eps,'normal')
# ks.test(eps,'pnorm',  mean = par2$estimate[1],  sd= par2$estimate[2])
# with(EPA.94b.tccb.df, 
#      qqPlot(eps, dist = "norm", 
#             estimate.params = TRUE, digits = 2, points.col = "#208F8C", pch=19,
#             add.line = TRUE, main='QQplot for u_0', xlab = 'Quantiles of normal', ylab = 'Quantiles of u_0'))



plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of RE for TF", ylim = c(-0.5,0.5))
# points(u0tab$u0rank,  u0tab$u0, col = "blue")
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se, col= '#64CB5D')
points(u0tab$u0rank,  u0tab$u0, col = "#208F8C", pch=19)
abline(h = 0, col = "black")

###################################################################


