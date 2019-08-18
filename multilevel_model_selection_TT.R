
# multilevel TT

# MULTILEVEL MODELS WITH THE NEW VARIABLE SELECTION

rm(list=ls())

#-------------------------------

library(EnvStats)
library(lme4)
library(lattice)
require(leaps)
library(xtable)
library(viridis)
library(stargazer)
library(MASS)
library(texreg)
library(optimx)
library(MuMIn)

#-------------------------------------------
# ANALISI MULTILEVEL SU DF5

df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')
# select teams with more or equal than 5 exported players
df5 <- df[which(df$noImp > 4),]

dim(df5)

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

df5$age2 = (df5$Age)^2

fit <- lmer(log(price_tm) ~  goalAbsT1Y + d20 + I(age2/100) + euroTB + openB+
               noMatch1Y + min1Y + actionS1Y + assist1Y + xg1Y + lpassA1Y + crossA1Y + interc1Y + recOpp1Y +
               yell1Y + losTot1Y + dB + (1 | TT), data = df5,REML = FALSE)
options(na.action = "na.fail")
dr <- dredge(fit, rank = 'AICc')

sel <- summary(model.avg(dr, subset=delta<2)) # scelgo quelli con AIC<2
sel

# 
# Call:
#   model.avg(object = dr, subset = delta < 2)
# 
# Component model call: 
#   lmer(formula = log(price_tm) ~ <18 unique rhs>, data = df5, REML = FALSE)
# 
# Component models: 
#   df  logLik    AICc delta weight
# 1/2/5/6/8/12/13/14/16      12 -765.93 1556.33  0.00   0.11
# 1/2/4/5/6/8/12/13/14/16    13 -764.89 1556.33  0.01   0.11
# 1/2/5/6/8/11/12/13/14/16   13 -765.53 1557.62  1.29   0.06
# 1/2/3/4/5/6/8/12/13/14/16  14 -764.51 1557.65  1.32   0.06
# 1/2/4/5/6/8/11/12/13/14/16 14 -764.55 1557.73  1.40   0.06
# 1/2/3/5/6/8/12/13/14/16    13 -765.62 1557.79  1.46   0.05
# 1/2/5/6/7/8/12/13/14/16    13 -765.66 1557.87  1.54   0.05
# 1/2/5/6/8/12/13/16         11 -767.75 1557.89  1.56   0.05
# 1/2/4/5/6/7/8/12/13/14/16  14 -764.63 1557.89  1.57   0.05
# 1/2/5/6/8/12/13/14/15/16   13 -765.72 1558.00  1.67   0.05
# 1/2/4/5/6/8/12/13/14/15/16 14 -764.69 1558.02  1.69   0.05
# 1/2/4/5/6/8/12/13/16       12 -766.79 1558.05  1.72   0.05
# 1/2/4/5/6/8/10/12/13/14/16 14 -764.71 1558.06  1.74   0.05
# 1/2/5/6/8/10/12/13/14/16   13 -765.79 1558.13  1.80   0.05
# 1/2/5/6/8/9/12/13/14/16    13 -765.82 1558.20  1.87   0.04
# 1/2/5/6/8/12/13/14/16/17   13 -765.88 1558.30  1.98   0.04
# 1/2/4/5/6/8/9/12/13/14/16  14 -764.84 1558.31  1.98   0.04
# 1/2/4/5/6/8/12/13/14/16/17 14 -764.85 1558.32  2.00   0.04
# 
# Term codes: 
#   actionS1Y I(age2/100)    assist1Y    crossA1Y         d20          dB      euroTB  goalAbsT1Y    interc1Y 
# 1           2           3           4           5           6           7           8           9 
# losTot1Y    lpassA1Y       min1Y   noMatch1Y       openB    recOpp1Y        xg1Y      yell1Y 
# 10          11          12          13          14          15          16          17 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)  1.0107477  0.2781938   0.2786931   3.627 0.000287 ***
#   actionS1Y    0.0115244  0.0041475   0.0041549   2.774 0.005542 ** 
#   I(age2/100) -0.1106505  0.0171098   0.0171408   6.455  < 2e-16 ***
#   d20          0.5913461  0.1175826   0.1177947   5.020  5.0e-07 ***
#   dB          -0.5760734  0.0962215   0.0963968   5.976  < 2e-16 ***
#   goalAbsT1Y   0.0021863  0.0009232   0.0009249   2.364 0.018085 *  
#   min1Y       -0.0062114  0.0026498   0.0026545   2.340 0.019287 *  
#   noMatch1Y    0.0276393  0.0028860   0.0028912   9.560  < 2e-16 ***
#   openB        0.0022212  0.0014066   0.0014085   1.577 0.114800    
# xg1Y         1.3590635  0.2919329   0.2924585   4.647  3.4e-06 ***
#   crossA1Y    -0.0012006  0.0016922   0.0016937   0.709 0.478424    
# lpassA1Y    -0.0002583  0.0011471   0.0011484   0.225 0.822008    
# assist1Y     0.0335579  0.1538953   0.1540689   0.218 0.827576    
# euroTB       0.0004462  0.0023189   0.0023218   0.192 0.847608    
# recOpp1Y     0.0020387  0.0121132   0.0121294   0.168 0.866523    
# losTot1Y     0.0008128  0.0054334   0.0054411   0.149 0.881245    
# interc1Y    -0.0007815  0.0072926   0.0073043   0.107 0.914799    
# yell1Y       0.0002298  0.0026757   0.0026801   0.086 0.931683    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)  1.0107477  0.2781938   0.2786931   3.627 0.000287 ***
#   actionS1Y    0.0115244  0.0041475   0.0041549   2.774 0.005542 ** 
#   I(age2/100) -0.1106505  0.0171098   0.0171408   6.455  < 2e-16 ***
#   d20          0.5913461  0.1175826   0.1177947   5.020  5.0e-07 ***
#   dB          -0.5760734  0.0962215   0.0963968   5.976  < 2e-16 ***
#   goalAbsT1Y   0.0021863  0.0009232   0.0009249   2.364 0.018085 *  
#   min1Y       -0.0062114  0.0026498   0.0026545   2.340 0.019287 *  
#   noMatch1Y    0.0276393  0.0028860   0.0028912   9.560  < 2e-16 ***
#   openB        0.0024616  0.0012653   0.0012676   1.942 0.052144 .  
# xg1Y         1.3590635  0.2919329   0.2924585   4.647  3.4e-06 ***
#   crossA1Y    -0.0024150  0.0016814   0.0016845   1.434 0.151667    
# lpassA1Y    -0.0022779  0.0026462   0.0026510   0.859 0.390201    
# assist1Y     0.3029690  0.3635927   0.3642557   0.832 0.405551    
# euroTB       0.0043729  0.0059609   0.0059717   0.732 0.464010    
# recOpp1Y     0.0212858  0.0335010   0.0335622   0.634 0.525937    
# losTot1Y     0.0088706  0.0158332   0.0158621   0.559 0.576001    
# interc1Y    -0.0092204  0.0234454   0.0234880   0.393 0.694645    
# yell1Y       0.0027948  0.0089395   0.0089558   0.312 0.754989    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#   actionS1Y I(age2/100) d20  dB   goalAbsT1Y min1Y noMatch1Y xg1Y openB crossA1Y lpassA1Y assist1Y
# Importance:          1.00      1.00        1.00 1.00 1.00       1.00  1.00      1.00 0.90  0.50     0.11     0.11    
# N containing models:   18        18          18   18   18         18    18        18   16     9        2        2    
# euroTB recOpp1Y losTot1Y interc1Y yell1Y
# Importance:          0.10   0.10     0.09     0.08     0.08  
# N containing models:    2      2        2        2        2 

length(unique(df5$TT))





