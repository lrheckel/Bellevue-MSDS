# libraries to use
library(ggplot2)
# library(readxl)
#library(DataExplorer)
# library(ggm)
library(readr)
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
library(caret)
# library(class)
library(factoextra)
library(purrr)
library(farff)
library(MLmetrics)

options(scipen=100)
options(digits=5)

# read the data files
binClass <- read_csv("binary-classifier-data.csv")
glimpse(binClass)
set.seed(1206)

#create the model with the data set
binClassModel <- glm(label ~ ., data = binClass, family = "binomial")
summary(binClassModel)

#find the probabilities of every observation in the test data set
binClass$modelProb <- predict(binClassModel, binClass, type="response")

#Pick my decision threshold and set the 1's and 0's
binClass <- binClass %>% mutate(modelPredict = 1*(modelProb > .50) +0)
glimpse(binClass)

true_pos <- (binClass$label==1) & (binClass$modelPredict==1)
true_neg <- (binClass$label==0) & (binClass$modelPredict==0)
false_pos <- (binClass$label==0) & (binClass$modelPredict==1)
false_neg <- (binClass$label==1) & (binClass$modelPredict==0)

confMatrix <- matrix(c(sum(true_pos), sum(false_pos),
                       sum(false_neg), sum(true_neg)),
                     2,2)
colnames(confMatrix) <- c('Yhat=1', 'Yhat=0')
rownames(confMatrix) <- c('Y = 1', 'Y = 0')
confMatrix

regrAccuracy <- (sum(true_pos)+sum(true_neg)) / (sum(true_pos)+sum(true_neg)+sum(false_pos)+sum(false_neg))
regrAccuracy





#good above here




precision <- sum(true_pos) / (sum(true_pos) + sum(false_pos))
precision

modRecall <- sum(true_pos) / (sum(true_pos) + sum(false_neg))
modRecall

modF1Score <- F1_Score(thorTesting$diedBinary, thorTesting$modelPredict, positive = 1)
modF1Score


idx <- order(-thorTesting$modelProb)
recall <- cumsum(true_pos[idx]==1)/sum(true_pos==1)
specif <- (sum(true_pos==0) - cumsum(true_pos[idx]==0))/sum(true_pos==0)
roc_df <- data.frame(recall=recall, specificity=specif)
ggplot(roc_df, aes(x=specificity, y=recall)) +
        geom_line(color='blue') +
        scale_x_reverse(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        geom_line(data=data.frame(x=(0:100/100)), aes(x=x, y=1-x),linetype='dotted', color='red')

modAUC <- sum(roc_df$recall[-1] * diff(1-roc_df$specificity))
modAUC


# good above here

