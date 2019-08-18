# multilevel models
rm(list=ls())
#library(EnvStats)
library(lme4)
library(lattice)
require(leaps)
library(xtable)
library(stargazer)
library(MASS)
library(texreg)
#library(optimx)
library(MuMIn)

df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')
df5 <- df[which(df$noExp > 4),]

df5$countryBGB <- ifelse(df5$countryB == "GB", 1, 0)
df5$countrySGB <- ifelse(df5$countryS == "GB", 1, 0)
df5$countryBFR <- ifelse(df5$countryB == "FR", 1, 0)
df5$countrySNL <- ifelse(df5$countryS == "NL", 1, 0)

df5$age2 = (df5$Age)^2

# fit1 <- lmer(log(price_tm) ~  goalAbsT1Y + euroTB + euroCB + role + I(age2/100) + openB + dSheikh + d10 + d20 + 
#                noMatch1Y + min1Y + actionS1Y + assist1Y + xg1Y + lpassA1Y + crossA1Y + interc1Y + recOpp1Y +
#                yell1Y + losTot1Y + red1Y + dB + (1 | TF), data = df5,REML = FALSE)
# 
# options(na.action = "na.fail")
# d1 <- dredge(fit1, rank = 'AICc')
# 
# # media dei diversi modelli
# performance_selection <- summary(model.avg(d1, subset=delta<2)) # scelgo quelli con AIC<2
# #summary(get.models(d2,1)[[1]])

###################################################################
#+ countryBFR +countryBGB + countrySGB + countrySNL + role + dSheikh + euroCB

fit2 <- lmer(log(price_tm) ~  goalAbsT1Y + d20 + I(age2/100) +
               noMatch1Y + min1Y + actionS1Y + assist1Y + xg1Y + lpassA1Y + crossA1Y + interc1Y + recOpp1Y +
               yell1Y + losTot1Y + red1Y + dB + (1 | TF), data = df5,REML = FALSE)
options(na.action = "na.fail")
d2 <- dredge(fit2, rank = 'AICc')
sel2 <- summary(model.avg(d2, subset=delta<2))
sel2

# RESULTS FIT2





# 
# 
# Call:
#   model.avg(object = d2, subset = delta < 2)
# 
# Component model call: 
#   lmer(formula = log(price_tm) ~ <14 unique rhs>, data = df5, REML = FALSE)
# 
# Component models: 
#   df  logLik    AICc delta weight
# 1/2/3/4/5/6/10/11/12     12 -670.24 1365.06  0.00   0.13
# 1/2/3/4/5/6/9/10/11/12   13 -669.38 1365.42  0.37   0.11
# 1/2/3/4/5/10/11/12       11 -671.54 1365.57  0.51   0.10
# 1/2/3/4/5/10/12          10 -672.80 1366.00  0.94   0.08
# 1/2/3/4/5/6/7/9/10/11/12 14 -668.66 1366.10  1.04   0.08
# 1/2/3/4/5/6/9/10/12      12 -670.82 1366.21  1.16   0.07
# 1/2/3/4/5/7/10/11/12     12 -671.02 1366.61  1.55   0.06
# 1/2/3/4/5/6/10/12        11 -672.09 1366.67  1.61   0.06
# 1/2/3/4/6/9/10/11/12     12 -671.06 1366.68  1.62   0.06
# 1/2/3/4/5/8/10/11/12     12 -671.08 1366.72  1.66   0.06
# 1/2/3/4/5/6/8/9/10/11/12 14 -668.99 1366.74  1.68   0.05
# 1/2/3/4/5/6/10/11/12/13  13 -670.05 1366.77  1.71   0.05
# 1/2/3/4/5/6/8/10/11/12   13 -670.07 1366.80  1.75   0.05
# 1/2/3/4/10/11/12         10 -673.26 1366.93  1.87   0.05
# 
# Term codes: 
#   actionS1Y I(age2/100)    assist1Y         d20  goalAbsT1Y    interc1Y    losTot1Y 
# 1           2           3           4           5           6           7 
# lpassA1Y       min1Y   noMatch1Y    recOpp1Y        xg1Y      yell1Y 
# 8           9          10          11          12          13 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)  0.3363239  0.3624168   0.3631226   0.926  0.35434    
# actionS1Y    0.0151891  0.0055900   0.0056010   2.712  0.00669 ** 
#   I(age2/100) -0.1319814  0.0207553   0.0208007   6.345  < 2e-16 ***
#   assist1Y     1.2492558  0.4926567   0.4936454   2.531  0.01138 *  
#   d20          0.9288091  0.0841204   0.0843066  11.017  < 2e-16 ***
#   goalAbsT1Y   0.0021332  0.0013640   0.0013661   1.561  0.11841    
# interc1Y     0.0339157  0.0352503   0.0352848   0.961  0.33645    
# noMatch1Y    0.0292793  0.0034414   0.0034487   8.490  < 2e-16 ***
#   recOpp1Y    -0.0586222  0.0480847   0.0481481   1.218  0.22340    
# xg1Y         2.1863786  0.4446211   0.4454494   4.908    9e-07 ***
#   min1Y       -0.0021437  0.0036826   0.0036857   0.582  0.56082    
# losTot1Y     0.0033832  0.0120572   0.0120697   0.280  0.77924    
# lpassA1Y    -0.0004217  0.0016057   0.0016080   0.262  0.79313    
# yell1Y      -0.0003988  0.0031715   0.0031766   0.126  0.90008    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)  0.336324   0.362417    0.363123   0.926  0.35434    
# actionS1Y    0.015189   0.005590    0.005601   2.712  0.00669 ** 
#   I(age2/100) -0.131981   0.020755    0.020801   6.345  < 2e-16 ***
#   assist1Y     1.249256   0.492657    0.493645   2.531  0.01138 *  
#   d20          0.928809   0.084120    0.084307  11.017  < 2e-16 ***
#   goalAbsT1Y   0.002388   0.001214    0.001217   1.962  0.04973 *  
#   interc1Y     0.051631   0.031257    0.031316   1.649  0.09921 .  
# noMatch1Y    0.029279   0.003441    0.003449   8.490  < 2e-16 ***
#   recOpp1Y    -0.074044   0.042172    0.042263   1.752  0.07978 .  
# xg1Y         2.186379   0.444621    0.445449   4.908    9e-07 ***
#   min1Y       -0.005874   0.003905    0.003913   1.501  0.13335    
# losTot1Y     0.025167   0.023091    0.023139   1.088  0.27675    
# lpassA1Y    -0.002577   0.003194    0.003201   0.805  0.42085    
# yell1Y      -0.007355   0.011590    0.011616   0.633  0.52659    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 






############################################

fit3 <- lmer(log(price_tm) ~  goalAbsT1Y + d20 + I(age2/100) + euroTB + openB+
               noMatch1Y + min1Y + actionS1Y + assist1Y + xg1Y + lpassA1Y + crossA1Y + interc1Y + recOpp1Y +
               yell1Y + losTot1Y + dB + (1 | TF), data = df5,REML = FALSE)
options(na.action = "na.fail")
d3 <- dredge(fit3, rank = 'AICc')

sel3 <- summary(model.avg(d3, subset=delta<2)) # scelgo quelli con AIC<2
sel3

# 
# # RESULTS FIT 3
# 
# 
# Call:
#   model.avg(object = d3, subset = delta < 2)
# 
# Component model call:
#   lmer(formula = log(price_tm) ~ <27 unique rhs>, data = df5, REML = FALSE)
# 
# Component models:
#   df  logLik    AICc delta weight
# 1/2/3/4/5/6/7/11/12/13/14         14 -662.38 1353.53  0.00   0.07
# 1/2/3/4/5/6/7/10/11/12/13/14      15 -661.36 1353.61  0.08   0.06
# 1/2/3/4/5/7/10/11/12/13/14        14 -662.46 1353.69  0.16   0.06
# 1/2/3/4/5/6/7/8/10/11/12/13/14    16 -660.49 1353.99  0.46   0.05
# 1/2/3/4/5/7/8/10/11/12/13/14      15 -661.62 1354.13  0.60   0.05
# 1/2/3/4/5/6/11/12/13/14           13 -663.86 1354.39  0.86   0.04
# 1/2/3/4/5/6/7/10/11/12/14         14 -662.88 1354.53  0.99   0.04
# 1/2/3/4/5/11/12/13/14             12 -664.99 1354.56  1.03   0.04
# 1/2/3/4/5/7/10/11/12/14           13 -664.03 1354.73  1.20   0.04
# 1/2/3/4/5/6/7/11/12/13/14/15      15 -661.94 1354.76  1.23   0.04
# 1/2/3/4/5/7/11/12/13/14           13 -664.08 1354.82  1.29   0.04
# 1/2/3/4/5/7/9/10/11/12/13/14      15 -662.00 1354.88  1.35   0.03
# 1/2/3/4/5/6/11/12/14              12 -665.17 1354.92  1.39   0.03
# 1/2/3/4/5/6/7/9/10/11/12/13/14    16 -660.97 1354.95  1.42   0.03
# 1/2/3/4/5/7/10/11/12/13/14/15     15 -662.05 1354.99  1.46   0.03
# 1/2/3/4/5/6/7/10/11/12/13/14/15   16 -661.01 1355.03  1.50   0.03
# 1/2/3/4/6/7/10/11/12/13/14        14 -663.16 1355.08  1.55   0.03
# 1/2/3/4/7/10/11/12/13/14          13 -664.21 1355.09  1.55   0.03
# 1/2/3/4/5/6/8/11/12/13/14         14 -663.25 1355.27  1.74   0.03
# 1/2/3/4/6/7/11/12/13/14           13 -664.32 1355.30  1.77   0.03
# 1/2/3/4/5/6/7/9/11/12/13/14       15 -662.22 1355.32  1.79   0.03
# 1/2/3/4/5/6/7/8/10/11/12/13/14/15 17 -660.10 1355.33  1.80   0.03
# 1/2/3/4/5/7/8/10/11/12/13/14/15   16 -661.17 1355.35  1.82   0.03
# 1/2/3/4/5/6/7/11/12/14            13 -664.35 1355.36  1.83   0.03
# 1/2/3/4/5/6/7/10/11/12/14/15      15 -662.29 1355.47  1.94   0.03
# 1/2/3/4/6/11/12/13/14             12 -665.47 1355.51  1.98   0.03
# 1/2/3/4/5/7/10/11/12/14/15        14 -663.37 1355.51  1.98   0.03
# 
# Term codes:
#   actionS1Y I(age2/100)    assist1Y         d20      euroTB  goalAbsT1Y    interc1Y    losTot1Y    lpassA1Y       min1Y   noMatch1Y
# 1           2           3           4           5           6           7           8           9          10          11
# openB    recOpp1Y        xg1Y      yell1Y
# 12          13          14          15
# 
# Model-averaged coefficients:
#   (full average)
# Estimate Std. Error Adjusted SE z value Pr(>|z|)
# (Intercept)  0.3631629  0.3719706   0.3726715   0.974 0.329816
# actionS1Y    0.0154352  0.0057641   0.0057748   2.673 0.007521 **
#   I(age2/100) -0.1423061  0.0208833   0.0209287   6.800  < 2e-16 ***
#   assist1Y     1.2061656  0.4945914   0.4955926   2.434 0.014942 *
#   d20          0.8148649  0.1036407   0.1038462   7.847  < 2e-16 ***
#   euroTB       0.0078793  0.0051626   0.0051706   1.524 0.127542
# goalAbsT1Y   0.0011939  0.0013224   0.0013239   0.902 0.367140
# interc1Y     0.0491971  0.0367574   0.0368016   1.337 0.181282
# noMatch1Y    0.0299618  0.0035458   0.0035531   8.432  < 2e-16 ***
#   openB        0.0054822  0.0014471   0.0014503   3.780 0.000157 ***
#   recOpp1Y    -0.0617380  0.0482429   0.0483073   1.278 0.201240
# xg1Y         2.3265399  0.4544770   0.4553080   5.110    3e-07 ***
#   min1Y       -0.0041120  0.0045308   0.0045350   0.907 0.364550
# losTot1Y     0.0057424  0.0159511   0.0159660   0.360 0.719099
# yell1Y      -0.0023023  0.0069280   0.0069368   0.332 0.739971
# lpassA1Y    -0.0002476  0.0012512   0.0012529   0.198 0.843352
# 
# (conditional average)
# Estimate Std. Error Adjusted SE z value Pr(>|z|)
# (Intercept)  0.363163   0.371971    0.372672   0.974 0.329816
# actionS1Y    0.015435   0.005764    0.005775   2.673 0.007521 **
#   I(age2/100) -0.142306   0.020883    0.020929   6.800  < 2e-16 ***
#   assist1Y     1.206166   0.494591    0.495593   2.434 0.014942 *
#   d20          0.814865   0.103641    0.103846   7.847  < 2e-16 ***
#   euroTB       0.008901   0.004584    0.004594   1.938 0.052666 .
# goalAbsT1Y   0.001910   0.001196    0.001199   1.593 0.111126
# interc1Y     0.059353   0.032050    0.032112   1.848 0.064554 .
# noMatch1Y    0.029962   0.003546    0.003553   8.432  < 2e-16 ***
#   openB        0.005482   0.001447    0.001450   3.780 0.000157 ***
#   recOpp1Y    -0.076144   0.042113    0.042204   1.804 0.071204 .
# xg1Y         2.326540   0.454477    0.455308   5.110    3e-07 ***
#   min1Y       -0.006770   0.003975    0.003983   1.700 0.089206 .
# losTot1Y     0.030854   0.024337    0.024390   1.265 0.205853
# yell1Y      -0.011178   0.011568    0.011594   0.964 0.334967
# lpassA1Y    -0.002604   0.003214    0.003221   0.809 0.418771
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance:
#   actionS1Y I(age2/100) assist1Y d20  noMatch1Y openB xg1Y euroTB interc1Y recOpp1Y goalAbsT1Y min1Y yell1Y
# Importance:          1.00      1.00        1.00     1.00 1.00      1.00  1.00 0.89   0.83     0.81     0.63       0.61  0.21
# N containing models:   27        27          27       27   27        27    27   23     22       21       17         16     7
# losTot1Y lpassA1Y
# Importance:          0.19     0.10
# N containing models:    5        3


#############################################

fit4 <- lmer(log(price_tm) ~  goalAbsT1Y + d20 + I(age2/100) + euroTB + openB+
               noMatch1Y + min1Y + actionS1Y + assist1Y + xg1Y + lpassA1Y + crossA1Y + interc1Y + recOpp1Y +
               yell1Y + losTot1Y + dB + (1 | TF), data = df5,REML = FALSE)
options(na.action = "na.fail")
d4 <- dredge(fit4, rank = 'BIC')

sel4 <- summary(model.avg(d4, subset=delta<2)) # scelgo quelli con AIC<2
sel4

# Call:
#   model.avg(object = d4, subset = delta < 2)
# 
# Component model call: 
#   lmer(formula = log(price_tm) ~ <2 unique rhs>, data = df5, REML = FALSE)
# 
# Component models: 
#   df  logLik     BIC delta weight
# 124567   9 -670.89 1398.70   0.0   0.66
# 1234567 10 -668.38 1400.01   1.3   0.34
# 
# Term codes: 
#   actionS1Y I(age2/100)    assist1Y         d20   noMatch1Y       openB        xg1Y 
# 1           2           3           4           5           6           7 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)  0.316498   0.317448    0.318141   0.995 0.319816    
# actionS1Y    0.015724   0.004731    0.004741   3.316 0.000912 ***
#   I(age2/100) -0.146086   0.020526    0.020571   7.102  < 2e-16 ***
#   d20          0.934782   0.083874    0.084058  11.121  < 2e-16 ***
#   noMatch1Y    0.029806   0.003237    0.003244   9.189  < 2e-16 ***
#   openB        0.005379   0.001435    0.001438   3.741 0.000183 ***
#   xg1Y         2.271564   0.377008    0.377814   6.012  < 2e-16 ***
#   assist1Y     0.340314   0.537580    0.537856   0.633 0.526915    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)  0.316498   0.317448    0.318141   0.995 0.319816    
# actionS1Y    0.015724   0.004731    0.004741   3.316 0.000912 ***
#   I(age2/100) -0.146086   0.020526    0.020571   7.102  < 2e-16 ***
#   d20          0.934782   0.083874    0.084058  11.121  < 2e-16 ***
#   noMatch1Y    0.029806   0.003237    0.003244   9.189  < 2e-16 ***
#   openB        0.005379   0.001435    0.001438   3.741 0.000183 ***
#   xg1Y         2.271564   0.377008    0.377814   6.012  < 2e-16 ***
#   assist1Y     0.992744   0.441968    0.442947   2.241 0.025011 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#   actionS1Y I(age2/100) d20  noMatch1Y openB xg1Y assist1Y
# Importance:          1.00      1.00        1.00 1.00      1.00  1.00 0.34    
# N containing models:    2         2           2    2         2     2    1  















##############################################################

fit5 <- lmer(log(price_tm) ~  I(passT1Y/100)+passAT1Y
             +possT1Y
             #age2+noMatch1Y+actionS1Y+xg1Y
             +I(goalAbsT1Y/100)
             +xgT1Y
             +losTot1Y+lowLosT1Y
             +recTot1Y+shotST1Y
             #+highRecT1Y
             #+I(challT1Y/100) #+I(challST1Y/100)
             +I(goalOppT1Y/100)
             + (1 | TF), data = df5,REML = FALSE)

options(na.action = "na.fail")
d5 <- dredge(fit5, rank = 'AICc')

sel5 <- summary(model.avg(d5, subset=delta<2))
sel5

# RESULTS FIT5

# 
# Call:
#   model.avg(object = d5, subset = delta < 2)
# 
# Component model call: 
#   lmer(formula = log(price_tm) ~ <17 unique rhs>, data = df5, REML = FALSE)
# 
# Component models: 
#   df  logLik    AICc delta weight
# 349     6 -826.45 1665.06  0.00   0.09
# 3489    7 -825.48 1665.16  0.10   0.09
# 3459    7 -825.53 1665.26  0.20   0.08
# 1349    7 -825.54 1665.29  0.23   0.08
# 3479    7 -825.67 1665.53  0.47   0.07
# 34589   8 -824.68 1665.63  0.57   0.07
# 13489   8 -824.74 1665.75  0.69   0.07
# 34789   8 -824.76 1665.78  0.72   0.06
# 3469    7 -825.86 1665.93  0.87   0.06
# 34689   8 -824.98 1666.22  1.16   0.05
# 13459   8 -825.09 1666.43  1.37   0.05
# 13479   8 -825.10 1666.46  1.40   0.05
# 13469   8 -825.31 1666.88  1.82   0.04
# 134789  9 -824.32 1666.96  1.90   0.04
# 134589  9 -824.33 1667.00  1.94   0.04
# 2349    7 -826.40 1667.00  1.94   0.04
# 23459   8 -825.40 1667.05  1.99   0.03
# 
# Term codes: 
#   I(goalAbsT1Y/100) I(goalOppT1Y/100)          losTot1Y         lowLosT1Y          passAT1Y    I(passT1Y/100) 
# 1                 2                 3                 4                 5                 6 
# possT1Y          recTot1Y             xgT1Y 
# 7                 8                 9 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)       -2.789269   1.435888    1.437993   1.940 0.052417 .  
# losTot1Y           0.106309   0.019421    0.019463   5.462  < 2e-16 ***
#   lowLosT1Y          8.828423   3.157300    3.164038   2.790 0.005267 ** 
#   xgT1Y              1.118939   0.310027    0.310535   3.603 0.000314 ***
#   recTot1Y          -0.006512   0.010993    0.011005   0.592 0.554032    
# passAT1Y           0.007145   0.016507    0.016524   0.432 0.665443    
# I(goalAbsT1Y/100)  0.115754   0.239407    0.239698   0.483 0.629155    
# possT1Y            0.003448   0.009289    0.009300   0.371 0.710794    
# I(passT1Y/100)     0.012174   0.044373    0.044427   0.274 0.784074    
# I(goalOppT1Y/100)  0.010171   0.098975    0.099160   0.103 0.918301    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)       -2.78927    1.43589     1.43799   1.940 0.052417 .  
# losTot1Y           0.10631    0.01942     0.01946   5.462  < 2e-16 ***
#   lowLosT1Y          8.82842    3.15730     3.16404   2.790 0.005267 ** 
#   xgT1Y              1.11894    0.31003     0.31054   3.603 0.000314 ***
#   recTot1Y          -0.01589    0.01208     0.01211   1.312 0.189439    
# passAT1Y           0.02658    0.02229     0.02234   1.190 0.234213    
# I(goalAbsT1Y/100)  0.33222    0.30428     0.30493   1.089 0.275939    
# possT1Y            0.01573    0.01416     0.01419   1.108 0.267678    
# I(passT1Y/100)     0.08191    0.08681     0.08700   0.942 0.346438    
# I(goalOppT1Y/100)  0.14710    0.34861     0.34938   0.421 0.673725    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#   losTot1Y lowLosT1Y xgT1Y recTot1Y I(goalAbsT1Y/100) passAT1Y possT1Y I(passT1Y/100)
# Importance:          1.00     1.00      1.00  0.41     0.35              0.27     0.22    0.15          
# N containing models:   17       17        17     7        7                 5        4       3          
# I(goalOppT1Y/100)
# Importance:          0.07             
# N containing models:    2  