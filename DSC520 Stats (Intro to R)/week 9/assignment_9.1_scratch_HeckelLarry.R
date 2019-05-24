# libraries to use
library(ggplot2)
# library(readxl)
#library(DataExplorer)
# library(ggm)
library(readr)
# library(dplyr)
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
library(class)

options(scipen=100)
options(digits=5)


# read the files
binaryData <- read_csv("binary-classifier-data.csv")
trinaryData <- read_csv("trinary-classifier-data.csv")

#esquisser()

ggplot(data = binaryData) +
  aes(x = x, y = y) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Binary Classifier Data',
    x = 'X data',
    y = 'Y data') +
  theme_grey()

ggplot(data = trinaryData) +
  aes(x = x, y = y) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Trinary Classifier Data',
    x = 'X data',
    y = 'Y data') +
  theme_grey()

#set the seed
set.seed(1206)

#create the training and test sets
binTrain <- createDataPartition(y=binaryData$label, p=0.7, list = FALSE)
binTraining <- binaryData[binTrain,]
binTesting <- binaryData[-binTrain,]

triTrain <- createDataPartition(y=trinaryData$label, p=0.7, list = FALSE)
triTraining <- trinaryData[triTrain,]
triTesting <- trinaryData[-triTrain,]

#convert the label fields to factors
binLabel = factor(binTraining[["label"]])
triLabel = factor(triTraining[["label"]])

#check the dimensions of the data
dim(binTraining); dim(binTesting)
dim(triTraining); dim(triTesting)


K <- 3
#create the knn model
binModelknn <- knn(binTraining, binTesting, binLabel, k=K)
binModelknn

#create the confusion matrix
tabBinTesting <- table(binModelknn,binTesting$label)
tabBinTesting

#compute the model's accuracy
accuracyBin3 <- (tabBinTesting[1,1] + tabBinTesting[2,2])/(tabBinTesting[1,1] + tabBinTesting[2,2]+tabBinTesting[1,2]+tabBinTesting[2,1])
accuracyBin3

#create data frame for graphing k versus accuracy
#add values to the data frame
frameAccuracy <- data.frame("K"=c(K), "Accuracy"=c(accuracyBin3))
str(frameAccuracy)

K <- 5
#create the knn model
binModelknn <- knn(binTraining, binTesting, binLabel, k=K)
binModelknn

#create the confusion matrix
tabBinTesting <- table(binModelknn,binTesting$label)
tabBinTesting

#compute the model's accuracy
accuracyBin5 <- (tabBinTesting[1,1] + tabBinTesting[2,2])/(tabBinTesting[1,1] + tabBinTesting[2,2]+tabBinTesting[1,2]+tabBinTesting[2,1])
accuracyBin5

#add values to the graphing data frame
frameAccuracy <- rbind(frameAccuracy,list(K,accuracyBin5))
str(frameAccuracy)

K <- 10
#create the knn model
binModelknn <- knn(binTraining, binTesting, binLabel, k=K)
binModelknn

#create the confusion matrix
tabBinTesting <- table(binModelknn,binTesting$label)
tabBinTesting

#compute the model's accuracy
accuracyBin10 <- (tabBinTesting[1,1] + tabBinTesting[2,2])/(tabBinTesting[1,1] + tabBinTesting[2,2]+tabBinTesting[1,2]+tabBinTesting[2,1])
accuracyBin10

#add values to the graphing data frame
frameAccuracy <- rbind(frameAccuracy,list(K,accuracyBin10))
str(frameAccuracy)

K <- 15
#create the knn model
binModelknn <- knn(binTraining, binTesting, binLabel, k=K)
binModelknn

#create the confusion matrix
tabBinTesting <- table(binModelknn,binTesting$label)
tabBinTesting

#compute the model's accuracy
accuracyBin15 <- (tabBinTesting[1,1] + tabBinTesting[2,2])/(tabBinTesting[1,1] + tabBinTesting[2,2]+tabBinTesting[1,2]+tabBinTesting[2,1])
accuracyBin15

#add values to the graphing data frame
frameAccuracy <- rbind(frameAccuracy,list(K,accuracyBin15))
str(frameAccuracy)

K <- 20
#create the knn model
binModelknn <- knn(binTraining, binTesting, binLabel, k=K)
binModelknn

#create the confusion matrix
tabBinTesting <- table(binModelknn,binTesting$label)
tabBinTesting

#compute the model's accuracy
accuracyBin20 <- (tabBinTesting[1,1] + tabBinTesting[2,2])/(tabBinTesting[1,1] + tabBinTesting[2,2]+tabBinTesting[1,2]+tabBinTesting[2,1])
accuracyBin20

#add values to the graphing data frame
frameAccuracy <- rbind(frameAccuracy,list(K,accuracyBin20))
str(frameAccuracy)

K <- 25
#create the knn model
binModelknn <- knn(binTraining, binTesting, binLabel, k=K)
binModelknn

#create the confusion matrix
tabBinTesting <- table(binModelknn,binTesting$label)
tabBinTesting

#compute the model's accuracy
accuracyBin25 <- (tabBinTesting[1,1] + tabBinTesting[2,2])/(tabBinTesting[1,1] + tabBinTesting[2,2]+tabBinTesting[1,2]+tabBinTesting[2,1])
accuracyBin25

#add values to the graphing data frame
frameAccuracy <- rbind(frameAccuracy,list(K,accuracyBin25))
str(frameAccuracy)

ggplot(data = frameAccuracy) +
  aes(x = K, y = Accuracy) +
  geom_line(color = '#0c4c8a') +
  labs(title = 'Binary KNN Accuracy',
    x = 'K Value',
    y = 'Accuracy') +
  theme_grey()

#### Now let's do the trinary data
K <- 3
#create the knn model
triModelknn <- knn(triTraining, triTesting, triLabel, k=K)
triModelknn

#create the confusion matrix
tabTriTesting <- table(triModelknn,triTesting$label)
tabTriTesting

#compute the model's accuracy
accuracyTri3 <- (tabTriTesting[1,1] + tabTriTesting[2,2]+tabTriTesting[3,3])/(tabTriTesting[1,1] + tabTriTesting[1,2]+tabTriTesting[1,3]+
                                                                                tabTriTesting[2,1] + tabTriTesting[2,2]+tabTriTesting[2,3]+
                                                                                tabTriTesting[3,1] + tabTriTesting[3,2]+tabTriTesting[3,3])
                                                                                
accuracyTri3

#create data frame for graphing k versus accuracy
#add values to the data frame
frameAccuracyTri <- data.frame("K"=c(K), "Accuracy"=c(accuracyTri3))
str(frameAccuracyTri)

K <- 5
#create the knn model
triModelknn <- knn(triTraining, triTesting, triLabel, k=K)
triModelknn

#create the confusion matrix
tabTriTesting <- table(triModelknn,triTesting$label)
tabTriTesting

#compute the model's accuracy
accuracyTri5 <- (tabTriTesting[1,1] + tabTriTesting[2,2]+tabTriTesting[3,3])/(tabTriTesting[1,1] + tabTriTesting[1,2]+tabTriTesting[1,3]+
                                                                                tabTriTesting[2,1] + tabTriTesting[2,2]+tabTriTesting[2,3]+
                                                                                tabTriTesting[3,1] + tabTriTesting[3,2]+tabTriTesting[3,3])

accuracyTri5

#create data frame for graphing k versus accuracy
#add values to the data frame
frameAccuracyTri <- rbind(frameAccuracyTri,list(K,accuracyTri5))
str(frameAccuracyTri)

K <- 10
#create the knn model
triModelknn <- knn(triTraining, triTesting, triLabel, k=K)
triModelknn

#create the confusion matrix
tabTriTesting <- table(triModelknn,triTesting$label)
tabTriTesting

#compute the model's accuracy
accuracyTri10 <- (tabTriTesting[1,1] + tabTriTesting[2,2]+tabTriTesting[3,3])/(tabTriTesting[1,1] + tabTriTesting[1,2]+tabTriTesting[1,3]+
                                                                                tabTriTesting[2,1] + tabTriTesting[2,2]+tabTriTesting[2,3]+
                                                                                tabTriTesting[3,1] + tabTriTesting[3,2]+tabTriTesting[3,3])

accuracyTri10

#create data frame for graphing k versus accuracy
#add values to the data frame
frameAccuracyTri <- rbind(frameAccuracyTri,list(K,accuracyTri10))
str(frameAccuracyTri)

K <- 15
#create the knn model
triModelknn <- knn(triTraining, triTesting, triLabel, k=K)
triModelknn

#create the confusion matrix
tabTriTesting <- table(triModelknn,triTesting$label)
tabTriTesting

#compute the model's accuracy
accuracyTri15 <- (tabTriTesting[1,1] + tabTriTesting[2,2]+tabTriTesting[3,3])/(tabTriTesting[1,1] + tabTriTesting[1,2]+tabTriTesting[1,3]+
                                                                                tabTriTesting[2,1] + tabTriTesting[2,2]+tabTriTesting[2,3]+
                                                                                tabTriTesting[3,1] + tabTriTesting[3,2]+tabTriTesting[3,3])

accuracyTri15

#create data frame for graphing k versus accuracy
#add values to the data frame
frameAccuracyTri <- rbind(frameAccuracyTri,list(K,accuracyTri15))
str(frameAccuracyTri)

K <- 20
#create the knn model
triModelknn <- knn(triTraining, triTesting, triLabel, k=K)
triModelknn

#create the confusion matrix
tabTriTesting <- table(triModelknn,triTesting$label)
tabTriTesting

#compute the model's accuracy
accuracyTri20 <- (tabTriTesting[1,1] + tabTriTesting[2,2]+tabTriTesting[3,3])/(tabTriTesting[1,1] + tabTriTesting[1,2]+tabTriTesting[1,3]+
                                                                                tabTriTesting[2,1] + tabTriTesting[2,2]+tabTriTesting[2,3]+
                                                                                tabTriTesting[3,1] + tabTriTesting[3,2]+tabTriTesting[3,3])

accuracyTri20

#create data frame for graphing k versus accuracy
#add values to the data frame
frameAccuracyTri <- rbind(frameAccuracyTri,list(K,accuracyTri20))
str(frameAccuracyTri)

K <- 25
#create the knn model
triModelknn <- knn(triTraining, triTesting, triLabel, k=K)
triModelknn

#create the confusion matrix
tabTriTesting <- table(triModelknn,triTesting$label)
tabTriTesting

#compute the model's accuracy
accuracyTri25 <- (tabTriTesting[1,1] + tabTriTesting[2,2]+tabTriTesting[3,3])/(tabTriTesting[1,1] + tabTriTesting[1,2]+tabTriTesting[1,3]+
                                                                                tabTriTesting[2,1] + tabTriTesting[2,2]+tabTriTesting[2,3]+
                                                                                tabTriTesting[3,1] + tabTriTesting[3,2]+tabTriTesting[3,3])

accuracyTri25

#create data frame for graphing k versus accuracy
#add values to the data frame
frameAccuracyTri <- rbind(frameAccuracyTri,list(K,accuracyTri25))
str(frameAccuracyTri)

ggplot(data = frameAccuracyTri) +
  aes(x = K, y = Accuracy) +
  geom_line(color = '#0c4c8a') +
  labs(title = 'Trinary KNN Accuracy',
       x = 'K Value',
       y = 'Accuracy') +
  theme_grey()


# good above here

#this is good knn for when you want R to choose the best k value
# binModel <- train(label ~., data=binTraining, method="knn",
#                   trControl =trainControl("cv", number=10),
#                   preProcess=c("center","scale"),
#                   tuneLength=3)
# binModel
# plot(binModel)
