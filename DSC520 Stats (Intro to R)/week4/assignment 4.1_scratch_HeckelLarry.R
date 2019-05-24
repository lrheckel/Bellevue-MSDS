# libraries to be used
library(ggplot2)
library(readr)
library(pastecs)

# read the file
examData <- read_delim("exams.dat", delim = '\t')
View(examData)
str(examData)

################

options(scipen=100)
options(digits=2)

# Analyze the entire data set.
examStats     <- stat.desc(examData$exam, norm = TRUE)
computerStats <- stat.desc(examData$computer, norm = TRUE)
lecturesStats <- stat.desc(examData$lectures, norm = TRUE)
numeracyStats <- stat.desc(examData$numeracy,norm = TRUE)

examStats
computerStats
lecturesStats
numeracyStats

histexamData_exam <- ggplot(data=examData,aes(x=exam)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Exam Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_exam

histexamData_examNorm <- ggplot(data=examData,aes(x=exam)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("exam") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_examNorm

histexamData_examProb <- ggplot(data=examData,aes(sample=exam)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamData_examProb


###################
computerStats

histexamData_computer <- ggplot(data=examData,aes(x=computer)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Computer Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_computer

histexamData_computerNorm <- ggplot(data=examData,aes(x=computer)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("computer") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_computerNorm

histexamData_computerProb <- ggplot(data=examData,aes(sample=computer)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamData_computerProb

#########################

lecturesStats

histexamData_lectures <- ggplot(data=examData,aes(x=lectures)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Lectures Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_lectures

histexamData_lecturesNorm <- ggplot(data=examData,aes(x=lectures)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(lectures), sd = sd(lectures)))) +
  scale_x_continuous("lectures") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_lecturesNorm

histexamData_lecturesProb <- ggplot(data=examData,aes(sample=lectures)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamData_lecturesProb

########################

numeracyStats

histexamData_numeracy <- ggplot(data=examData,aes(x=numeracy)) + 
  geom_histogram(aes(fill=..count..),binwidth=1) +
  ggtitle("Numeracy Scores") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_numeracy

histexamData_numeracyNorm <- ggplot(data=examData,aes(x=numeracy)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(numeracy), sd = sd(numeracy))))+
  scale_x_continuous("numeracy") +
  ggtitle("Normal Curve with Histogram") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamData_numeracyNorm

histexamData_numeracyProb <- ggplot(data=examData,aes(sample=numeracy)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamData_numeracyProb

#subset the data set
examDataGeorge <- subset(examData, uni=="0",
                         select=c("exam", "computer", "lectures", "numeracy", "stats"))

examStatsGeorge     <- stat.desc(examDataGeorge$exam, norm = TRUE)
computerStatsGeorge <- stat.desc(examDataGeorge$computer, norm = TRUE)
lecturesStatsGeorge <- stat.desc(examDataGeorge$lectures, norm = TRUE)
numeracyStatsGeorge <- stat.desc(examDataGeorge$numeracy,norm = TRUE)

examStatsGeorge

histexamDataGeorge_exam <- ggplot(data=examDataGeorge,aes(x=exam)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Exam Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_exam

histexamDataGeorge_examNorm <- ggplot(data=examDataGeorge,aes(x=exam)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("exam") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_examNorm

histexamDataGeorge_examProb <- ggplot(data=examDataGeorge,aes(sample=exam)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataGeorge_examProb

###################
computerStatsGeorge

histexamDataGeorge_computer <- ggplot(data=examDataGeorge,aes(x=computer)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Computer Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_computer

histexamDataGeorge_computerNorm <- ggplot(data=examDataGeorge,aes(x=computer)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("computer") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_computerNorm

histexamDataGeorge_computerProb <- ggplot(data=examDataGeorge,aes(sample=computer)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataGeorge_computerProb

#########################

lecturesStatsGeorge

histexamDataGeorge_lectures <- ggplot(data=examDataGeorge,aes(x=lectures)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Lectures Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_lectures

histexamDataGeorge_lecturesNorm <- ggplot(data=examDataGeorge,aes(x=lectures)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(lectures), sd = sd(lectures)))) +
  scale_x_continuous("lectures") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_lecturesNorm

histexamDataGeorge_lecturesProb <- ggplot(data=examDataGeorge,aes(sample=lectures)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataGeorge_lecturesProb

########################

numeracyStatsGeorge

histexamDataGeorge_numeracy <- ggplot(data=examDataGeorge,aes(x=numeracy)) + 
  geom_histogram(aes(fill=..count..),binwidth=1) +
  ggtitle("Numeracy Scores") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_lectures

histexamDataGeorge_numeracyNorm <- ggplot(data=examData,aes(x=numeracy)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(numeracy), sd = sd(numeracy))))+
  scale_x_continuous("numeracy") +
  ggtitle("Normal Curve with Histogram") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataGeorge_numeracyNorm

histexamDataGeorge_numeracyProb <- ggplot(data=examDataGeorge,aes(sample=numeracy)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataGeorge_numeracyProb

examDataJeff  <- subset(examData, uni=="1",
                        select=c("exam", "computer", "lectures", "numeracy", "stats"))
examStatsJeff     <- stat.desc(examDataJeff$exam, norm = TRUE)
computerStatsJeff <- stat.desc(examDataJeff$computer, norm = TRUE)
lecturesStatsJeff <- stat.desc(examDataJeff$lectures, norm = TRUE)
numeracyStatsJeff <- stat.desc(examDataJeff$numeracy,norm = TRUE)

examStatsJeff

histexamDataJeff_exam <- ggplot(data=examDataJeff,aes(x=exam)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Exam Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_exam

histexamDataJeff_examNorm <- ggplot(data=examDataJeff,aes(x=exam)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("exam") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_examNorm

histexamDataJeff_examProb <- ggplot(data=examDataJeff,aes(sample=exam)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataJeff_examProb

###################
computerStatsJeff

histexamDataJeff_computer <- ggplot(data=examDataJeff,aes(x=computer)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Computer Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_computer

histexamDataJeff_computerNorm <- ggplot(data=examDataJeff,aes(x=computer)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("computer") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_computerNorm

histexamDataJeff_computerProb <- ggplot(data=examDataJeff,aes(sample=computer)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataJeff_computerProb

#########################
lecturesStatsJeff

histexamDataJeff_lectures <- ggplot(data=examDataJeff,aes(x=lectures)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Lectures Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_lectures

histexamDataJeff_lecturesNorm <- ggplot(data=examDataJeff,aes(x=lectures)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(lectures), sd = sd(lectures)))) +
  scale_x_continuous("lectures") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_lecturesNorm

histexamDataJeff_lecturesProb <- ggplot(data=examDataJeff,aes(sample=lectures)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamData_lecturesProb

########################
numeracyStatsJeff

histexamDataJeff_numeracy <- ggplot(data=examDataJeff,aes(x=numeracy)) + 
  geom_histogram(aes(fill=..count..),binwidth=1) +
  ggtitle("Numeracy Scores") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_numeracy

histexamDataJeff_numeracyNorm <- ggplot(data=examDataJeff,aes(x=numeracy)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(numeracy), sd = sd(numeracy))))+
  scale_x_continuous("numeracy") +
  ggtitle("Normal Curve with Histogram") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataJeff_numeracyNorm

histexamDataJeff_numeracyProb <- ggplot(data=examDataJeff,aes(sample=numeracy)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataJeff_numeracyProb

examDataTushmann  <- subset(examData, uni=="2",
                            select=c("exam", "computer", "lectures", "numeracy", "stats"))
examStatsTushmann     <- stat.desc(examDataTushmann$exam, norm = TRUE)
computerStatsTushmann <- stat.desc(examDataTushmann$computer, norm = TRUE)
lecturesStatsTushmann <- stat.desc(examDataTushmann$lectures, norm = TRUE)
numeracyStatsTushmann <- stat.desc(examDataTushmann$numeracy,norm = TRUE)

examStatsTushmann

histexamDataTushmann_exam <- ggplot(data=examDataTushmann,aes(x=exam)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Exam Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_exam

histexamDataTushmann_examNorm <- ggplot(data=examDataTushmann,aes(x=exam)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("exam") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_examNorm

histexamDataTushmann_examProb <- ggplot(data=examDataTushmann,aes(sample=exam)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamData_examProb

###################
computerStatsTushmann

histexamDataTushmann_computer <- ggplot(data=examDataTushmann,aes(x=computer)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Computer Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_computer

histexamDataTushmann_computerNorm <- ggplot(data=examDataTushmann,aes(x=computer)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(exam), sd = sd(exam)))) +
  scale_x_continuous("computer") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_computerNorm

histexamDataTushmann_computerProb <- ggplot(data=examDataTushmann,aes(sample=computer)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataTushmann_computerProb

#########################
lecturesStatsTushmann

histexamDataTushmann_lectures <- ggplot(data=examDataTushmann,aes(x=lectures)) + 
  geom_histogram(aes(fill=..count..),binwidth=2) +
  ggtitle("Lectures Scores") +
  xlab("Percent Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_lectures

histexamDataTushmann_lecturesNorm <- ggplot(data=examDataTushmann,aes(x=lectures)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(lectures), sd = sd(lectures)))) +
  scale_x_continuous("lectures") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_lecturesNorm

histexamDataTushmann_lecturesProb <- ggplot(data=examDataTushmann,aes(sample=lectures)) + 
  stat_qq() + stat_qq_line(col = "Red")
histexamDataTushmann_lecturesProb

########################
numeracyStatsTushmann

histexamData_numeracy <- ggplot(data=examDataTushmann,aes(x=numeracy)) + 
  geom_histogram(aes(fill=..count..),binwidth=1) +
  ggtitle("Numeracy Scores") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_lectures

histexamDataTushmann_numeracyNorm <- ggplot(data=examDataTushmann,aes(x=numeracy)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=2) +
  stat_function(fun = dnorm, args=with(data=examData, c(mean = mean(numeracy), sd = sd(numeracy))))+
  scale_x_continuous("numeracy") +
  ggtitle("Normal Curve with Histogram") +
  xlab("Number Achieved") + ylab("Number of Students") +
  scale_fill_gradient("Number of Students", low="blue", high="green")
histexamDataTushmann_numeracyNorm

histexamDataTushmann_numeracyProb <- ggplot(data=examDataTushmann,aes(sample=numeracy)) +
 stat_qq() + stat_qq_line(col = "Red")
histexamDataTushmann_numeracyProb








