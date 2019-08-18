#--------------------------------- 
# Test Ks distribuzione gamma e lnorm per price_tm
# Analisi della distribuzione:
rm(list=ls())
# install.packages('EnvStats')

#-------------------------------

library(EnvStats)
library(lme4)
library(lattice)
require(leaps)
library(xtable)
library(viridis)
library(stargazer)
#install.packages('texreg')
library(MASS)
library(texreg)
#install.packages("optimx")
library(optimx)

#---------------------------------
setwd("C:/Users/letym/Desktop/15.Analysis_new")
df <- read.csv(file = 'data_diff_nozeri.csv', header = TRUE, sep = ',')
sum1 <- summary(df)

#Shapiro-Wilk test is used for testing normality on the transformed observations.
install.packages('goft')
library(goft)


x <- log(df$price_tm)
mean(x)
median(x)

skewness(x)
kurtosis(x)

# 1.964072
# 2.014903

# rapporto tra 0,95-1.05: 0.9747725

# near to 0
#The values for asymmetry and kurtosis between -2 and +2 are considered acceptable in order to prove normal univariate distribution (George & Mallery, 2010). 

# Compute Shapiro-Wilk Goodness-of-Fit statistic for the
# Reference Area TcCB data assuming a lognormal distribution
#-----------------------------------------------------------
sw.list <- gofTest(price_tm ~ 1, data =df, dist = "lnorm")
sw.list

sw.list <- gofTest(price_tm ~ 1, data =df, dist = "gamma")
sw.list



gofTest(df$price_tm, dist = "lnorm")$p.value
gofTest(df$price_tm, dist = "gamma")$p.value


#The Shapiro-Wilk test is a test of normality in frequentist statistics. It was published in 1965 by Samuel Sanford Shapiro and Martin Wilk.
#-----------------------------------------------------------------------#




fitdistr(df$price_tm,"lognormal")
ks.test(df$price_tm,'plnorm',  meanlog = 1.96407150,  sdlog = 1.14906662  )

fitdistr(df$price_tm, 'gamma')
ks.test(df$price_tm,'pgamma',  shape= 0.974192526 ,  rate =  0.075429347  )

     qqPlot(df$price_tm, dist = "lnorm", 
            estimate.params = TRUE, digits = 2,points.col = "#208F8C", pch=19,
            add.line = TRUE, main='Lognormal QQplot for price', xlab = 'Quantiles of normal', ylab = 'Quantiles of log(price)')
 
qqPlot(df$price_tm, dist = "gamma",estimate.params = TRUE, digits = 2, points.col = "#208F8C", pch=19, 
            add.line = TRUE, main='Gamma QQplot for price', xlab = 'Quantiles of gamma', ylab = 'Quantiles of price')

mean(df$price_tm)
length(df$price_tm)
length(which(df$price_tm > 40))



# anche qui rigetta:  K-S test is not suitable when estimating the parameters from the data. You can use the following code, which relies on the Anderson-Darling test for normality
install.packages("nortest")
library(nortest)
ad.test(log(df$price_tm))


#The null-hypothesis of this test is that the population is normally distributed. Thus, on the one hand, 
#if the p value is less than the chosen alpha level, then the null hypothesis is rejected and there is 
#evidence that the data tested are not normally distributed. On the other hand, if the p value is greater 
#than the chosen alpha level, then the null hypothesis that the data came from a normally distributed 
#population can not be rejected (e.g., for an alpha level of .05, a data set with a p value of less than .05 rejects the null hypothesis that the data are from a normally distributed population)
#Like most statistical significance tests, if the sample size is sufficiently large this test may detect 
#even trivial departures from the null hypothesis (i.e., although there may be some statistically significant effect, it may be too small to be of any practical significance); thus, additional investigation of the effect size is typically advisable, e.g., a Q-Q plot in this case.