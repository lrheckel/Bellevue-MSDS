  # libraries to use
#library(ggplot2)
# library(readxl)
#library(DataExplorer)
# library(ggm)
#library(readr)
library(dplyr)
# # library(stringr)
# # library(psych)
# # library(janitor)
# library(car)
# library(QuantPsyc)
# # library(pastecs)
# # library(sqldf)
#library(esquisse)
#library(FNN)
#library(caret)
#library(class)
library(foreign)
library(farff)

options(scipen=100)
options(digits=5)


# read the file
thorSurgery <- readARFF("ThoraricSurgery.arff")
glimpse(thorSurgery)

# logRegrThorSurg <- glm(Risk1Yr ~ DGN+PRE4+PRE5+PRE6+PRE7+PRE8+PRE9+
#                          PRE10+PRE11+PRE14+PRE17+PRE19+PRE25+PRE30+
#                          PRE32+AGE, data = thorSurgery, family = "binomial")

#logRegrThorSurgrel <- glm(Risk1Yr ~ ., data = thorSurgery, family = "binomial")
#summary(logRegrThorSurg)
#relevel the survival variable to set TRUE to 0.
#TRUE is if the patient died, so we want FALSE, or lived,
#to be set to 1
#thorSurgery$Risk1Yr <- relevel(thorSurgery$Risk1Yr, "T")

logRegrThorSurgrel <- glm(Risk1Yr ~ ., data = thorSurgery, family = "binomial")
summary(logRegrThorSurgrel)

#find the probabilities of every observation in the model
thorSurgery$modelProb <- predict(logRegrThorSurgrel, thorSurgery, type="response")

#Pick my decision threshold and set the 1's and 0's
thorSurgery <- thorSurgery %>% mutate(modelPredict = 1*(modelProb > .55) +0,
                                      diedBinary = 1*(Risk1Yr=="F") + 0)
glimpse(thorSurgery)

#pred_y <- as.numeric(pred>0)
#true_y <- as.numeric(thorSurgery$Risk1Yr=="T")
true_pos <- (thorSurgery$diedBinary==1) & (thorSurgery$modelPredict==1)
true_neg <- (thorSurgery$diedBinary==0) & (thorSurgery$modelPredict==0)
false_pos <- (thorSurgery$diedBinary==0) & (thorSurgery$modelPredict==1)
false_neg <- (thorSurgery$diedBinary==1) & (thorSurgery$modelPredict==0)

confMatrix <- matrix(c(sum(true_pos), sum(false_pos),
                       sum(false_neg), sum(true_neg)),
                       2,2)
colnames(confMatrix) <- c('Yhat=1', 'Yhat=0')
rownames(confMatrix) <- c('Y = 1', 'Y = 0')
confMatrix

regrAccuracy <- (sum(true_pos)+sum(true_neg)) / (sum(true_pos)+sum(true_neg)+sum(false_pos)+sum(false_neg))
regrAccuracy




# good above here

