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

df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')
dTT <- df[which(df$noImp > 4),]

nullmodel <- lmer(log(price_tm) ~ (1 | TT),  data = dTT, REML = FALSE)
fit <- lm(log(price_tm)~ 1, data = dTT)
-2*logLik(fit)+2*logLik(nullmodel) 

summary(nullmodel)
texreg(nullmodel)

##############################################################################

dTT$age2 <- (dTT$Age)^2

dTT$countryBGB <- ifelse(dTT$countryB == "GB", 1, 0)
dTT$countryBFR <- ifelse(dTT$countryB == "FR", 1, 0)
dTT$countryBRU <- ifelse(dTT$countryB == "RU", 1, 0)
dTT$countryBIT <- ifelse(dTT$countryB == "IT", 1, 0)
dTT$countryBES <- ifelse(dTT$countryB == "ES", 1, 0)
dTT$countryBPT <- ifelse(dTT$countryB == "PT", 1, 0)
dTT$countryBTR <- ifelse(dTT$countryB == "TR", 1, 0)
dTT$countryBGR <- ifelse(dTT$countryB == "GR", 1, 0)
dTT$countryBNL <- ifelse(dTT$countryB == "NL", 1, 0)

dTT$countrySGB <- ifelse(dTT$countryS == "GB", 1, 0)
dTT$countrySNL <- ifelse(dTT$countryS == "NL", 1, 0)
dTT$countrySIT <- ifelse(dTT$countryS == "IT", 1, 0)
dTT$countrySES <- ifelse(dTT$countryS == "ES", 1, 0)
dTT$countrySFR <- ifelse(dTT$countryS == "FR", 1, 0)
dTT$countrySBE <- ifelse(dTT$countryS == "BE", 1, 0)
dTT$countrySPT <- ifelse(dTT$countryS == "PT", 1, 0)


# selected (1)-(8) and best linear model obtained IN PARAGRAPH precedente tranne euro TB

#selected model (1)-(8)
fit1 <- lmer(log(price_tm) ~ noMatch1Y  + (1 | TT), data = dTT, REML = FALSE)
fit2 <- lmer(log(price_tm) ~ xg1Y + (1 | TT), data = dTT, REML = FALSE)
fit3 <- lmer(log(price_tm) ~ actionS1Y + (1 | TT), data = dTT, REML = FALSE)
fit4 <- lmer(log(price_tm) ~ I(age2/100) + (1 | TT), data = dTT, REML = FALSE)
fit5 <- lmer(log(price_tm) ~ I(goalAbsT1Y/100) + (1 | TT), data = dTT, REML = FALSE)
fit6 <- lmer(log(price_tm) ~ dB + (1 | TT), data = dTT, REML = FALSE) # euroTB -> dB
fit7 <- lmer(log(price_tm) ~ dC + (1 | TT), data = dTT, REML = FALSE) 
fit8 <- lmer(log(price_tm) ~ d20 + (1 | TT), data = dTT, REML = FALSE)
fit9 <- lmer(log(price_tm) ~  min1Y + (1 | TT), data = dTT, REML = FALSE) # openB -> min1Y

# aggiungo quelle dei due modelli lineari migliori

fit10 <- lmer(log(price_tm) ~ countryBFR + (1 | TT), data = dTT, REML = FALSE)
fit11 <- lmer(log(price_tm) ~ countryBGB + (1 | TT), data = dTT, REML = FALSE)
fit12 <- lmer(log(price_tm) ~ countrySGB + (1 | TT), data = dTT, REML = FALSE)
fit13 <- lmer(log(price_tm) ~ countrySNL + (1 | TT), data = dTT, REML = FALSE)

texreg(list(nullmodel, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12, fit13))
round(c(r.squaredGLMM(nullmodel)[1],r.squaredGLMM(fit1)[1],r.squaredGLMM(fit1)[1],
        r.squaredGLMM(fit3)[1],r.squaredGLMM(fit4)[1],r.squaredGLMM(fit5)[1],r.squaredGLMM(fit6)[1],
        r.squaredGLMM(fit7)[1],r.squaredGLMM(fit8)[1],r.squaredGLMM(fit9)[1],r.squaredGLMM(fit10)[1],
        r.squaredGLMM(fit11)[1],r.squaredGLMM(fit12)[1],r.squaredGLMM(fit13)[1]),3)


round(c(r.squaredGLMM(nullmodel)[2],r.squaredGLMM(fit1)[2],r.squaredGLMM(fit2)[2],
        r.squaredGLMM(fit3)[2],r.squaredGLMM(fit4)[2],r.squaredGLMM(fit5)[2],r.squaredGLMM(fit6)[2],
        r.squaredGLMM(fit7)[2],r.squaredGLMM(fit8)[2],r.squaredGLMM(fit9)[2],r.squaredGLMM(fit10)[2],
        r.squaredGLMM(fit11)[2],r.squaredGLMM(fit12)[2],r.squaredGLMM(fit13)[2]),3)

round(AICc(nullmodel, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12,fit13)$AICc,2)




# plots
predscore1 <- fitted(fit1)
datapred1 <- unique(data.frame(cbind(predscore1 = predscore1, noMatch1Y = dTT$noMatch1Y, TT = dTT$TT)))
xyplot(predscore1 ~ noMatch1Y, data = datapred1, groups = TT, type = c("p", "l"), col = '#208F8C', ylab = 'predictions')

predscore <- fitted(fit5)
datapred <- unique(data.frame(cbind(predscore = predscore, noGoal = dTT$goalAbsT1Y, TT = dTT$TT)))
xyplot(predscore ~ noGoal, data = datapred, groups = TT, type = c("p", "l"), col = '#208F8C', ylab = 'predictions')







#-------------------------------------------------------------------#
#####################################################################
#-------------------------------------------------------------------#
# confronto modelli con piu esplicative
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
# parto dal modello indicato come migliore che e` fit 13 e  aggiugno variabili

fit14  <- lmer(log(price_tm) ~ noMatch1Y + I(goalAbsT1Y/100) + xg1Y + (1 | TT), data = dTT, REML = FALSE)

fit15 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB + d20 + min1Y 
              + (1 | TT), data = dTT, REML = FALSE)

fit16 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB + d20  
              + (1 | TT), data = dTT, REML = FALSE)

fit17 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
              + (1 | TT), data = dTT, REML = FALSE)

fit18 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + (1 | TT), data = dTT, REML = FALSE)

fit19 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countryS + (1 | TT), data = dTT, REML = FALSE)

fit20 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL+ (1 | TT), data = dTT, REML = FALSE)

fit21 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryB + (1 | TT), data = dTT, REML = FALSE)

fit22 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + (1 | TT), data = dTT, REML = FALSE)

fit23 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + (1 | TT), data = dTT, REML = FALSE)

fit24 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBFR + (1 | TT), data = dTT, REML = FALSE)

fit25 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countryBGB + countryBFR + (1 | TT), data = dTT, REML = FALSE)

fit26 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySNL + countryBGB + countryBFR + (1 | TT), data = dTT, REML = FALSE)

texreg(list(fit14, fit15,fit16, fit17, fit18, fit19,fit20, fit21,fit22,fit23,fit24,fit25,fit26))

round(AICc(fit13, fit14, fit15,fit16, fit17, fit18, fit19,fit20, fit21,fit22,fit23,fit24,fit25)$AICc,2)
round(c(r.squaredGLMM(fit14)[1],r.squaredGLMM(fit15)[1],
        r.squaredGLMM(fit16)[1],r.squaredGLMM(fit17)[1],r.squaredGLMM(fit18)[1],r.squaredGLMM(fit19)[1],
        r.squaredGLMM(fit20)[1],r.squaredGLMM(fit21)[1],r.squaredGLMM(fit22)[1],r.squaredGLMM(fit23)[1],
        r.squaredGLMM(fit24)[1],r.squaredGLMM(fit25)[1],r.squaredGLMM(fit26)[1]),3)
round(c(r.squaredGLMM(fit14)[2],r.squaredGLMM(fit15)[2],
        r.squaredGLMM(fit16)[2],r.squaredGLMM(fit17)[2],r.squaredGLMM(fit18)[2],r.squaredGLMM(fit19)[2],
        r.squaredGLMM(fit20)[2],r.squaredGLMM(fit22)[2],r.squaredGLMM(fit22)[2],r.squaredGLMM(fit23)[2],
        r.squaredGLMM(fit24)[2],r.squaredGLMM(fit25)[2],r.squaredGLMM(fit26)[2]),3)


cor(df$min1Y, df$goalAbsT1Y)


# metto anche le variabili del migliore modello lineare su dTF che sono assist eurotb e xgT1Y

fit27 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + euroTB + (1 | TT), data = dTT, REML = FALSE)

fit28 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + assist1Y+ (1 | TT), data = dTT, REML = FALSE)
# xg vanno a sostituire goals abs T
fit29 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + xgT1Y +(1 | TT), data = dTT, REML = FALSE)

# euroTS
fit30 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + I(goalAbsT1Y/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + euroTS+ (1 | TT), data = dTT, REML = FALSE)

# assist non serve e nemmeno euroCS, ma anche euro CB non fa migliorare così tanto.
# tra i 4 il modello migliore è quello in cui sostituisco il numero totale di goals con gli xg della squadra M29

# best da dredge
# actionS1Y I(age2/100) countryBFR countryBGB countrySGB countrySNL d20  dB   dC   highRecT1Y noMatch1Y xg1Y

# provoa ad aggiungere una alla volta altre variabili relative alla performance della squadra che vende, non riportiamo i risultati di tutte
fit31 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + xgT1Y + highRecT1Y +(1 | TT), data = dTT, REML = FALSE)

# son correlate quindi tolgo xgT1Y
fit32 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + highRecT1Y +(1 | TT), data = dTT, REML = FALSE)

fit33 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + highRecT1Y + euroTB +(1 | TT), data = dTT, REML = FALSE)

# proviamo a inserire una alla volta le variabili di performance del gioatore e troviamo che l'accuracy dei passaggi fa migliorare il modello


texreg(list(fit27,fit28,fit29,fit30,fit31,fit32,fit33))

round(AICc(fit27,fit28,fit29,fit30,fit31,fit32,fit33)$AICc,2)
round(c(r.squaredGLMM(fit27)[1],
        r.squaredGLMM(fit28)[1],r.squaredGLMM(fit29)[1],r.squaredGLMM(fit30)[1],r.squaredGLMM(fit31)[1],
        r.squaredGLMM(fit32)[1],r.squaredGLMM(fit33)[1]),3)
round(c(r.squaredGLMM(fit27)[2],
        r.squaredGLMM(fit28)[2],r.squaredGLMM(fit29)[2],r.squaredGLMM(fit30)[2],r.squaredGLMM(fit31)[2],
        r.squaredGLMM(fit32)[2],r.squaredGLMM(fit33)[2]),3)


########################################################
# random slope

fit33 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + highRecT1Y + euroTB +(1 | TT), data = dTT, REML = FALSE)

fit34 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + highRecT1Y + euroTB + (1 + d20| TT), data = dTT, REML = FALSE)

fit35 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + highRecT1Y + euroTB + (1 + dB| TT), data = dTT, REML = FALSE)

texreg(list(fit33,fit34, fit35), ci.force = T, ci.force.level = 0.99)
round(AICc(fit33,fit34, fit35)$AICc,2)
round(c(r.squaredGLMM(fit33)[1],r.squaredGLMM(fit34)[1],r.squaredGLMM(fit35)[1]),3)
round(c(r.squaredGLMM(fit33)[2],r.squaredGLMM(fit34)[2],r.squaredGLMM(fit35)[2]),3)


##############################################################################################
# BEST MODEL FOUND IS 33
##############################################################################################



# normality tests du fit33
u0 <- ranef(fit33, postVar = TRUE)
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
v <- as.matrix(u0[[1]]) # vedo ogni squadra se ha influenza + o -

vett_u0 <- (attr(u0[[1]], "postVar"))[1,,]
TT <- (rownames(u0[[1]]))
u0tab <- cbind(TT, u0[[1]], u0se)
colnames(u0tab) <- c("TT", "u0", "u0se")
# riordino per valori crescenti di u0
u0tab <- u0tab[order(u0tab$u0), ]


# aggiungo una colonna che riempio coi rank: così ho la tabella completa con nazione e la stima degli u_j uno per uno
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
colnames(u0tab)[4] <- "u0rank" #rinomino
u0tab <- u0tab[order(u0tab$TT),  ] # importante ordinarli se no il grafico fa schifo

# vedo se u_j son normali
par <- fitdistr(u0tab$u0,'normal')
ks.test(u0tab$u0,'pnorm',  mean = par$estimate[1],  sd= par$estimate[2])

qqPlot(u0tab$u0, dist = "norm", 
            estimate.params = TRUE, digits = 2, points.col = "#208F8C", pch=19,
            add.line = TRUE, main='QQplot for u_0', xlab = 'Quantiles of normal', ylab = 'Quantiles of u_0')


plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of RE for TT", ylim = c(-0.5,0.5))
# points(u0tab$u0rank,  u0tab$u0, col = "blue")
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se, col= '#64CB5D')
points(u0tab$u0rank,  u0tab$u0, col = "#208F8C", pch=19)
abline(h = 0, col = "black")









##############################################
# RI, RS and LM

# best multilevel
fit37<- lm(log(price_tm) ~ actionS1Y + I(age2/100) + d20 + assist1Y + noMatch1Y + xg1Y + countryBGB + countryBFR + xgT1Y + countrySGB + euroTB, data = dTT)

fit36<- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
             countrySGB + countrySNL + countryBGB + countryBFR + highRecT1Y + euroTB, data = dTT)

# lm 26
lm.fit25 = lm(log(price) ~  I(age2/100)+noMatch1Y+actionS1Y+xg1Y+dB+dC
              +I(goalAbsT1Y/100)+d20
              +countryBGB+countrySGB+countryBFR #+countryBRU
              +countrySNL
              , data = dTT)



# modello 30 che è modello lineare con le variabili del multilevel
plot(fit36, labels.id = dTT$name_final)

# modello 33 best multilevel
fit33 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 +
                + countrySGB + countrySNL + countryBGB + countryBFR + highRecT1Y + euroTB +(1 | TT), data = dTT, REML = FALSE)


texreg(list(fit33, fit36, lm.fit25, fit37))
round(c(r.squaredGLMM(fit33)[1],r.squaredGLMM(fit36)[1],r.squaredGLMM(lm.fit25)[1],r.squaredGLMM(fit37)[1]),3)
round(c(r.squaredGLMM(fit28)[2],r.squaredGLMM(fit36)[2],r.squaredGLMM(lm.fit25)[2],r.squaredGLMM(fit37)[2]),3)

round(AIC(fit33, fit36, lm.fit25, fit37)$AIC,2)
round(AICc(fit33, fit36, lm.fit25, fit37)$AICc,2)
round(BIC(fit33, fit36, lm.fit25, fit37)$BIC,2)


#################################################################################
# robusteness checks


# countryB fixed
plot(RI_TT)

RI_TT<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                   + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTT, REML = FALSE)

RS_TT<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
              + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1 + dB| TT), data = dTT, REML = FALSE)

RS_CB <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
              + countryB + highRecT1Y +(1 + dB| TT), data = dTT, REML = FALSE)

RS_CS <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
              + countryS + highRecT1Y +(1 + dB| TT), data = dTT, REML = FALSE)

RI_TTfixTF <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                      + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y+TF +(1 | TT), data = dTT, REML = FALSE)

RI_TTfixTFCB <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                         + countryB + highRecT1Y+TF +(1 | TT), data = dTT, REML = FALSE)

RI_TTfixTFCS <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                         + countryS + highRecT1Y+TF +(1 | TT), data = dTT, REML = FALSE)



texreg(list(RI_TT, RI_TTfixTF,RI_TTfixTFCB,RI_TTfixTFCS))
round(c(r.squaredGLMM(RI_TT)[1],r.squaredGLMM(RS_TT)[1],r.squaredGLMM(RS_CB)[1],r.squaredGLMM(RS_CS)[1],r.squaredGLMM(RI_TTfixTF)[1],r.squaredGLMM(RI_TTfixTFCB)[2],r.squaredGLMM(RI_TTfixTFCS)[2]),3)
round(c(r.squaredGLMM(RI_TT)[2],r.squaredGLMM(RS_TT)[2],r.squaredGLMM(RS_CB)[2],r.squaredGLMM(RS_CS)[2],r.squaredGLMM(RI_TTfixTF)[2],r.squaredGLMM(RI_TTfixTFCB)[2],r.squaredGLMM(RI_TTfixTFCS)[2]),3)

round(AICc(RI_TT, RS_TT, RS_CB,RS_CS,RI_TTfixTF,RI_TTfixTFCB,RI_TTfixTFCS)$AICc,2)


##############################################



fixTT <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
            + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y + TT, data = dTT)

fixTTCB <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
            + countryB + highRecT1Y + TT, data = dTT)

fixTTCS <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                  + countryS + highRecT1Y + TT, data = dTT)

fixTTCBCS <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                  + countryB + countryS + highRecT1Y + TT, data = dTT)

fixCBTF <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                  + countryB + highRecT1Y + TF, data = dTT)

fixCSTF <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                  + countryS + highRecT1Y + TF, data = dTT)

fixCBCSTF <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
                + countryB + countryS + highRecT1Y + TF, data = dTT)

#CB e Cs nel prossimo sono solo quelle selezionate dal modello best
fixTF <- lm(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
            + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y + TF, data = dTT)

texreg(list(fixTT, fixTTCB, fixTTCS,fixTTCBCS,fixCBTF,fixCSTF,fixCBCSTF,fixTF))

round(AIC(fixTT, fixTTCB, fixTTCS,fixTTCBCS,fixCBTF,fixCSTF,fixCBCSTF,fixTF)$AIC,2)
round(AICc(fixTT, fixTTCB, fixTTCS,fixTTCBCS,fixCBTF,fixCSTF,fixCBCSTF,fixTF)$AICc,2)
round(BIC(fixTT, fixTTCB, fixTTCS,fixTTCBCS,fixCBTF,fixCSTF,fixCBCSTF,fixTF)$BIC,2)




##################################################################
# DIVISION BASED ON PRICE

dTT_more20 <- dTT[which(dTT$price_tm>20),]
dTT_520 <- dTT[which((dTT$price_tm<20) & (dTT$price_tm>5)),]
dTT_less5 <- dTT[which(dTT$price_tm<5),]

dim(dTT_more20)
dim(dTT_520)
dim(dTT_less5)

RI_TT1<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB + d20 + euroTB
             + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTT_more20, REML = FALSE)
RI_TT2<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
             + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTT_520, REML = FALSE)
RI_TT3<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
             + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTT_less5, REML = FALSE)

# RS son tutte singolari quindi non le faccio

texreg(list(RI_TT1,RI_TT2,RI_TT3))
# no aic e bic perchè non ho stesso numero di oss
round(c(r.squaredGLMM(RI_TT1)[1],r.squaredGLMM(RI_TT2)[1],r.squaredGLMM(RI_TT3)[1]),3)
round(c(r.squaredGLMM(RI_TT1)[2],r.squaredGLMM(RI_TT2)[2],r.squaredGLMM(RI_TT3)[2]),3)



##################################################################
# DIVISION BASED ON PRICE

dTTD <- dTT[which(dTT$role == 'Defender'),]
dTTM <- dTT[which(dTT$role == 'Midfielder'),]
dTTA <- dTT[which(dTT$role == 'Attacker'),]

RI_TT4<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
              + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTTD, REML = FALSE)
RI_TT5<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
              + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTTM, REML = FALSE)
RI_TT6<- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
              + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTTA, REML = FALSE)

# RS son tutte singolari quindi non le faccio

texreg(list(RI_TT4,RI_TT5,RI_TT6))
# no aic e bic perchè non ho stesso numero di oss
round(c(r.squaredGLMM(RI_TT4)[1],r.squaredGLMM(RI_TT5)[1],r.squaredGLMM(RI_TT6)[1]),3)
round(c(r.squaredGLMM(RI_TT4)[2],r.squaredGLMM(RI_TT5)[2],r.squaredGLMM(RI_TT6)[2]),3)



# robustness
fit31 <- lmer(log(price_tm) ~ I(noMatch1Y/100) + xg1Y + actionS1Y + I(age2/100) + dC+ dB + d20 + euroTB
              + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTT, REML = FALSE)


fit31_6 <- lmer(log(price_tm) ~ I(noMatch6/100) + xg6 + actionS6 + I(age2/100) + dC+ dB + d20 + euroTB
              + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTT, REML = FALSE)

fit31_all <- lmer(log(price_tm) ~ I(noMatch/100) + xg + actionS + I(age2/100) + dC+ dB + d20 + euroTB
              + countryBGB + countryBFR + countrySNL + countrySGB + highRecT1Y +(1| TT), data = dTT, REML = FALSE)


texreg(list(fit31,fit31_6,fit31_all))
round(c(r.squaredGLMM(fit31)[1],r.squaredGLMM(fit31_6)[1],r.squaredGLMM(fit31_all)[1]),3)
round(c(r.squaredGLMM(fit31)[2],r.squaredGLMM(fit31_6)[2],r.squaredGLMM(fit31_all)[2]),3)
AICc(fit31,fit31_6,fit31_all)$AICc







#----------------------------
fitprova1 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + xgT1Y + dB+ dC +euroTB+ d20 
              + countryBGB + countryBFR + countrySGB + countrySNL + highRecT1Y + (1 | TT), data = dTT, REML = FALSE)

fitprova2 <- lmer(log(price_tm) ~ noMatch1Y + xg1Y + actionS1Y + I(age2/100) + dB+ dC + d20 
                  + countryBGB + countryBFR + countrySGB + countrySNL + highRecT1Y + passA1Y+ (1 | TT), data = dTT, REML = FALSE)


options(na.action = "na.fail")
#dr <- dredge(fitprova1, rank = 'AICc')
sel <- summary(model.avg(dr, subset=delta<2)) # scelgo quelli con AIC<2
sel

# 
# Call:
#   model.avg(object = dr, subset = delta < 2)
# 
# Component model call: 
#   lmer(formula = log(price_tm) ~ <3 unique rhs>, data = dTT, REML = FALSE)
# 
# Component models: 
#   df  logLik    AICc delta weight
# 1/2/3/4/5/6/7/8/9/10/11/12/13    16 -724.01 1480.85  0.00   0.47
# 1/2/3/4/5/6/7/8/9/10/11/12/13/14 17 -723.32 1481.56  0.72   0.33
# 1/2/3/4/5/6/7/8/9/11/12/13       15 -725.89 1482.50  1.66   0.20
# 
# Term codes: 
#   actionS1Y I(age2/100)  countryBFR  countryBGB  countrySGB  countrySNL         d20          dB          dC 
# 1           2           3           4           5           6           7           8           9 
# euroTB  highRecT1Y   noMatch1Y        xg1Y       xgT1Y 
# 10          11          12          13          14 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept) -0.175493   0.302746    0.303289   0.579 0.562836    
# actionS1Y    0.009055   0.003617    0.003623   2.499 0.012455 *  
#   I(age2/100) -0.132225   0.016718    0.016749   7.895  < 2e-16 ***
#   countryBFR   0.580446   0.200828    0.201195   2.885 0.003914 ** 
#   countryBGB   0.714353   0.116383    0.116594   6.127  < 2e-16 ***
#   countrySGB   0.218385   0.076808    0.076949   2.838 0.004539 ** 
#   countrySNL  -0.485736   0.128151    0.128385   3.783 0.000155 ***
#   d20          0.519671   0.120154    0.120345   4.318 1.57e-05 ***
#   dB          -0.694573   0.093889    0.094061   7.384  < 2e-16 ***
#   dC          -1.137284   0.235132    0.235563   4.828 1.40e-06 ***
#   euroTB       0.008281   0.006309    0.006316   1.311 0.189790    
# highRecT1Y   5.930696   1.900661    1.903507   3.116 0.001835 ** 
#   noMatch1Y    0.027013   0.002664    0.002669  10.122  < 2e-16 ***
#   xg1Y         1.408088   0.274155    0.274653   5.127 3.00e-07 ***
#   xgT1Y        0.048789   0.100535    0.100630   0.485 0.627791    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept) -0.175493   0.302746    0.303289   0.579 0.562836    
# actionS1Y    0.009055   0.003617    0.003623   2.499 0.012455 *  
#   I(age2/100) -0.132225   0.016718    0.016749   7.895  < 2e-16 ***
#   countryBFR   0.580446   0.200828    0.201195   2.885 0.003914 ** 
#   countryBGB   0.714353   0.116383    0.116594   6.127  < 2e-16 ***
#   countrySGB   0.218385   0.076808    0.076949   2.838 0.004539 ** 
#   countrySNL  -0.485736   0.128151    0.128385   3.783 0.000155 ***
#   d20          0.519671   0.120154    0.120345   4.318 1.57e-05 ***
#   dB          -0.694573   0.093889    0.094061   7.384  < 2e-16 ***
#   dC          -1.137284   0.235132    0.235563   4.828 1.40e-06 ***
#   euroTB       0.010408   0.005281    0.005291   1.967 0.049166 *  
#   highRecT1Y   5.930696   1.900661    1.903507   3.116 0.001835 ** 
#   noMatch1Y    0.027013   0.002664    0.002669  10.122  < 2e-16 ***
#   xg1Y         1.408088   0.274155    0.274653   5.127 3.00e-07 ***
#   xgT1Y        0.149001   0.126236    0.126468   1.178 0.238727    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#   actionS1Y I(age2/100) countryBFR countryBGB countrySGB countrySNL d20  dB   dC   highRecT1Y
# Importance:          1.00      1.00        1.00       1.00       1.00       1.00       1.00 1.00 1.00 1.00      
# N containing models:    3         3           3          3          3          3          3    3    3    3      
                    # noMatch1Y xg1Y euroTB xgT1Y
# Importance:          1.00      1.00 0.80   0.33 
# N containing models:    3         3    2      1 


AICc(fitprova1,fitprova2)
BIC(fitprova1,fitprova2)

# Call:
#   model.avg(object = dr, subset = delta < 2)
# 
# Component model call: 
#   lmer(formula = log(price_tm) ~ <3 unique rhs>, data = dTT, REML = FALSE)
# 
# Component models: 
#   df  logLik    AICc delta weight
# 1/2/3/4/5/6/7/8/9/10/11/12/13    16 -724.01 1480.85  0.00   0.47
# 1/2/3/4/5/6/7/8/9/10/11/12/13/14 17 -723.32 1481.56  0.72   0.33
# 1/2/3/4/5/6/7/8/9/11/12/13       15 -725.89 1482.50  1.66   0.20
# 
# Term codes: 
#   actionS1Y I(age2/100)  countryBFR  countryBGB  countrySGB  countrySNL         d20          dB          dC 
# 1           2           3           4           5           6           7           8           9 
# euroTB  highRecT1Y   noMatch1Y        xg1Y       xgT1Y 
# 10          11          12          13          14 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept) -0.175493   0.302746    0.303289   0.579 0.562836    
# actionS1Y    0.009055   0.003617    0.003623   2.499 0.012455 *  
#   I(age2/100) -0.132225   0.016718    0.016749   7.895  < 2e-16 ***
#   countryBFR   0.580446   0.200828    0.201195   2.885 0.003914 ** 
#   countryBGB   0.714353   0.116383    0.116594   6.127  < 2e-16 ***
#   countrySGB   0.218385   0.076808    0.076949   2.838 0.004539 ** 
#   countrySNL  -0.485736   0.128151    0.128385   3.783 0.000155 ***
#   d20          0.519671   0.120154    0.120345   4.318 1.57e-05 ***
#   dB          -0.694573   0.093889    0.094061   7.384  < 2e-16 ***
#   dC          -1.137284   0.235132    0.235563   4.828 1.40e-06 ***
#   euroTB       0.008281   0.006309    0.006316   1.311 0.189790    
# highRecT1Y   5.930696   1.900661    1.903507   3.116 0.001835 ** 
#   noMatch1Y    0.027013   0.002664    0.002669  10.122  < 2e-16 ***
#   xg1Y         1.408088   0.274155    0.274653   5.127 3.00e-07 ***
#   xgT1Y        0.048789   0.100535    0.100630   0.485 0.627791    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept) -0.175493   0.302746    0.303289   0.579 0.562836    
# actionS1Y    0.009055   0.003617    0.003623   2.499 0.012455 *  
#   I(age2/100) -0.132225   0.016718    0.016749   7.895  < 2e-16 ***
#   countryBFR   0.580446   0.200828    0.201195   2.885 0.003914 ** 
#   countryBGB   0.714353   0.116383    0.116594   6.127  < 2e-16 ***
#   countrySGB   0.218385   0.076808    0.076949   2.838 0.004539 ** 
#   countrySNL  -0.485736   0.128151    0.128385   3.783 0.000155 ***
#   d20          0.519671   0.120154    0.120345   4.318 1.57e-05 ***
#   dB          -0.694573   0.093889    0.094061   7.384  < 2e-16 ***
#   dC          -1.137284   0.235132    0.235563   4.828 1.40e-06 ***
#   euroTB       0.010408   0.005281    0.005291   1.967 0.049166 *  
#   highRecT1Y   5.930696   1.900661    1.903507   3.116 0.001835 ** 
#   noMatch1Y    0.027013   0.002664    0.002669  10.122  < 2e-16 ***
#   xg1Y         1.408088   0.274155    0.274653   5.127 3.00e-07 ***
#   xgT1Y        0.149001   0.126236    0.126468   1.178 0.238727    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#   actionS1Y I(age2/100) countryBFR countryBGB countrySGB countrySNL d20  dB   dC   highRecT1Y
# Importance:          1.00      1.00        1.00       1.00       1.00       1.00       1.00 1.00 1.00 1.00      
# N containing models:    3         3           3          3          3          3          3    3    3    3      
# noMatch1Y xg1Y euroTB xgT1Y
# Importance:          1.00      1.00 0.80   0.33 
# N containing models:    3         3    2      1 
