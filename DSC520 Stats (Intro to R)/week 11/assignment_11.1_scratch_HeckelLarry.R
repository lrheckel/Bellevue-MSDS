# # libraries to use
# library(ggplot2)
# # library(readxl)
# #library(DataExplorer)
# # library(ggm)
library(readr)
library(dplyr)
# # # library(stringr)
# # # library(psych)
# # # library(janitor)
# # library(car)
# # library(QuantPsyc)
# # # library(pastecs)
# # # library(sqldf)
# #library(esquisse)
# #library(FNN)
# library(caret)
# # library(class)
# library(factoextra)
# library(purrr)
# library(farff)
# library(MLmetrics)

options(scipen=100)
options(digits=5)

#Using the Beta distribution and the probability function P(p) = Beta(k + 1,n - k + 1),  
#plot the probability distributions for the following values.

p = seq(0,1, length=100)
# No data collected. k = 0, n = 0.
k=0
n=0
plot(p, dbeta(p, (k+1), (n-k+1)), ylab="density", type ="l", col="blue")

# k = 2, n = 2
k=2
n=2
plot(p, dbeta(p, (k+1), (n-k+1)), ylab="density", type ="l", col="blue")

# k = 4, n = 15 
k=4
n=15
plot(p, dbeta(p, (k+1), (n-k+1)), ylab="density", type ="l", col="blue")

# k = 132, n = 500
k=132
n=500
plot(p, dbeta(p, (k+1), (n-k+1)), ylab="density", type ="l", col="blue")

#trials=500
#successes=132

resultFrame <- data.frame(n = c(0,2,15,500), k = c(0,2,4,132))
str(resultFrame)
probSuccess = sum(resultFrame$k)/sum(resultFrame$n)
probSuccess

#binom.test(0, 0, probSuccess)
succRateEst <- binom.test(2, 2, probSuccess)
succRateEst
succRateEst <- binom.test(4, 15, probSuccess)
succRateEst
succRateEst <- binom.test(132, 500, probSuccess)
succRateEst

# read the data files
abTest <- read_csv("ab_test.csv")
glimpse(abTest)
set.seed(1206)

## Load the data from ab_test.csv. Using all the data, 
#plot probability distribution for the test case A and B on the same plot. 
#Based on these plots, which one has the higher conversion rates?

p = seq(0,1, length=100)

# using subset function 
Bdata <- subset(abTest, label=="B")
Adata <- subset(abTest, label=="A")

#get the numbers for A and B trials and successes
bSuccess <- sum(Bdata$is_success)
bSuccess
bTrials <- nrow(Bdata)
bTrials

aSuccess <- sum(Adata$is_success)
aSuccess
aTrials <- nrow(Adata)
aTrials

#plot the two distibutions on the same graph
plot(p, dbeta(p, aSuccess+1, aTrials-aSuccess+1), ylab="density", type ="l", col="blue")
lines(p, dbeta(p, bSuccess+1, bTrials-bSuccess+1), type ="l", col="red")
legend(0.2,50, c("Be(A)","Be(B)"),lty=c(1,1),col=c("blue", "red"))

#95% CI for Label A
qbetaA <- qbeta( c(0.025, 0.975), aSuccess+1, aTrials-aSuccess+1)
qbetaA

#95% CI for Label B
qbetaB <- qbeta( c(0.025, 0.975), bSuccess+1, bTrials-bSuccess+1)
qbetaB


# do the steps by date 1
Bdata1 <- subset(Bdata, as.Date(Bdata$timestamp) <= as.Date("2009-09-01"))
glimpse(Bdata1)

Adata1 <- subset(Adata, as.Date(Adata$timestamp) <= as.Date("2009-09-01"))
glimpse(Adata1)

#get the numbers for A and B trials and successes
bSuccess1 <- sum(Bdata1$is_success)
bSuccess1
bTrials1 <- nrow(Bdata1)
bTrials1

aSuccess1 <- sum(Adata1$is_success)
aSuccess1
aTrials1 <- nrow(Adata1)
aTrials1

#plot the two distibutions on the same graph
plot(p, dbeta(p, aSuccess1+1, aTrials1-aSuccess1+1), ylab="density", type ="l", col="blue")
lines(p, dbeta(p, bSuccess1+1, bTrials1-bSuccess1+1), type ="l", col="red")
legend(0.2,50, c("Be(A)","Be(B)"),lty=c(1,1),col=c("blue", "red"))

#95% CI for Label A
qbetaA <- qbeta( c(0.025, 0.975), aSuccess1+1, aTrials1-aSuccess1+1)
qbetaA

#95% CI for Label B
qbetaB1 <- qbeta( c(0.025, 0.975), bSuccess1+1, bTrials1-bSuccess1+1)
qbetaB1

############


# do the steps by date 2
Bdata2 <- subset(Bdata, as.Date(Bdata$timestamp) <= as.Date("2009-10-15"))
glimpse(Bdata2)

Adata2 <- subset(Adata, as.Date(Adata$timestamp) <= as.Date("2009-10-15"))
glimpse(Adata2)

#get the numbers for A and B trials and successes
bSuccess2 <- sum(Bdata2$is_success)
bSuccess2
bTrials2 <- nrow(Bdata2)
bTrials2

aSuccess2 <- sum(Adata2$is_success)
aSuccess2
aTrials2 <- nrow(Adata2)
aTrials2

#plot the two distibutions on the same graph
plot(p, dbeta(p, aSuccess2+1, aTrials2-aSuccess2+1), ylab="density", type ="l", col="blue")
lines(p, dbeta(p, bSuccess2+1, bTrials2-bSuccess2+1), type ="l", col="red")
legend(0.2,50, c("Be(A)","Be(B)"),lty=c(1,1),col=c("blue", "red"))

#95% CI for Label A
qbetaA2 <- qbeta( c(0.025, 0.975), aSuccess2+1, aTrials2-aSuccess2+1)
qbetaA2

#95% CI for Label B
qbetaB2 <- qbeta( c(0.025, 0.975), bSuccess2+1, bTrials2-bSuccess2+1)
qbetaB2

###################
# do the steps by date 3
Bdata3 <- subset(Bdata, as.Date(Bdata$timestamp) <= as.Date("2009-12-24"))
glimpse(Bdata3)

Adata3 <- subset(Adata, as.Date(Adata$timestamp) <= as.Date("2009-12-24"))
glimpse(Adata3)

#get the numbers for A and B trials and successes
bSuccess3 <- sum(Bdata3$is_success)
bSuccess3
bTrials3 <- nrow(Bdata3)
bTrials3

aSuccess3 <- sum(Adata3$is_success)
aSuccess3
aTrials3 <- nrow(Adata3)
aTrials3

#plot the two distibutions on the same graph
plot(p, dbeta(p, aSuccess3+1, aTrials3-aSuccess3+1), ylab="density", type ="l", col="blue")
lines(p, dbeta(p, bSuccess3+1, bTrials3-bSuccess3+1), type ="l", col="red")
legend(0.2,50, c("Be(A)","Be(B)"),lty=c(1,1),col=c("blue", "red"))

#95% CI for Label A
qbetaA3 <- qbeta( c(0.025, 0.975), aSuccess3+1, aTrials3-aSuccess3+1)
qbetaA3

#95% CI for Label B
qbetaB3 <- qbeta( c(0.025, 0.975), bSuccess3+1, bTrials3-bSuccess3+1)
qbetaB3


# good above here

