
# libraries to be used
library(ggplot2)
library(readr)
library(pastecs)
library(psych)

# read the file
examDataX <- read_delim("exams.dat", delim = '\t')
#View(examDataX)
#str(examDataX)

#removing the extraneous column in the data
examData <- within(examDataX, rm(X7))
View(examData)
str(examData)

###################



profUni <- factor(examData$uni, levels=c(0:2), labels=c("George","Jeff","Tushmann"))



describeBy(examData$exam, list(profUni),mat=TRUE,digits=3)
describeBy(examData$computer, list(profUni),mat=TRUE,digits=3)
describeBy(examData$lectures, list(profUni),mat=TRUE,digits=3)
describeBy(examData$numeracy, list(profUni),mat=TRUE,digits=3)
describeBy(examData$stats, list(profUni),mat=TRUE,digits=3)


by(examData$exam, list(profUni), shapiro.test)

shapiro.test(examDataTushmann$exam)
shapiro.test(examDataTushmann$computer)
shapiro.test(examDataTushmann$lectures)
shapiro.test(examDataTushmann$numeracy)
shapiro.test(examDataTushmann$stats)
