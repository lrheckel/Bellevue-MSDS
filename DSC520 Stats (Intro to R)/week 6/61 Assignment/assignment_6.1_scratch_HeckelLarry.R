# libraries to be used
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(psych)
library(janitor)
library(car)
library(QuantPsyc)
library(pastecs)
library(ggm)

options(scipen=100)
options(digits=2)

gss2016 <- read_csv("gss-2016.csv", guess_max = 3000)
View(gss2016)
str(gss2016)
head(gss2016)
distinct(gss2016, MAR5)

gss2016Clean <- remove_empty(gss2016, which = c("rows","cols"))
str(gss2016Clean)

sibChildplot <- ggplot(gss2016, aes(x=SIBS,y=CHILDS)) +
                      geom_point() + geom_jitter() +
                      stat_smooth(method=lm, na.rm=TRUE)
sibChildplot

gss2016SibChild <- subset(gss2016,SIBS<23, select = c(SIBS,CHILDS))
sibChildbox <- ggplot(gss2016SibChild, aes(x=SIBS,y=CHILDS)) +
                      geom_boxplot(aes(group = SIBS))
sibChildbox

#this works, but it is an ugly graph, so try some other things
gss2016GraphData <- ggplot(data=gss2016,aes(as.factor(SIBS),CHILDS))

gss2016box <- gss2016GraphData + geom_boxplot() +
  ggtitle("Siblings and Children") +
  xlab("Number of Siblings") + ylab("Number of Children")
gss2016box


gss2016GraphData3 <- ggplot(data=gss2016,aes(x=as.factor(CHILDS)))
histgss2016col <- gss2016GraphData3 + geom_histogram(stat="count") +
                    ggtitle("Families # of Children") +
                    xlab("Number of Children") + ylab("Count of Families")
histgss2016col
histgss2016col + facet_wrap( ~ SIBS)


sibChildlm <- lm(gss2016$CHILDS ~ gss2016$SIBS, na.action=na.omit)
sibChildlm
summary(sibChildlm)

covSibChild <- cov(gss2016$CHILDS, gss2016$SIBS, use = "pairwise.complete.obs")
covSibChild

corSibChild <- cor.test(gss2016$SIBS, gss2016$CHILDS, use="pairwise.complete.obs", method="pearson")
corSibChild

cor(gss2016$SIBS, gss2016$CHILDS, use="pairwise.complete.obs", method="pearson")
cor(gss2016$SIBS, gss2016$CHILDS, use="pairwise.complete.obs", method="pearson")^2


gss2016GraphData3 <- ggplot(data=gss2016,aes(x=as.factor(CHILDS)))

histgss2016box <- gss2016GraphData + geom_boxplot() +
   ggtitle("Siblings and Children") +
   xlab("Number of Siblings") + ylab("Number of Children")
histgss2016box

#subset the data for siblings up to 23
gss2016SibChild23 <- subset(gss2016,SIBS<24, select = c(SIBS,CHILDS))

gss2016GraphData2 <- ggplot(data=gss2016SibChild23,aes(x=as.factor(CHILDS)))
histgss201623 <- gss2016GraphData2 + geom_histogram(stat="count") +
  ggtitle("Number of Siblings") +
  xlab("Number of Children") + ylab("Count of Families")
histgss201623


gss2016SibChild <- subset(gss2016,SIBS<15, select = c(SIBS,CHILDS))
gss2016GraphData3 <- ggplot(data=gss2016SibChild,aes(x=as.factor(CHILDS)))
histgss2016 <- gss2016GraphData3 + geom_histogram(stat="count") +
                  ggtitle("Number of Siblings") +
                  xlab("Number of Children") + ylab("Count of Families")
histgss2016
histgss2016 + facet_wrap( ~SIBS, scales="free_y")



options(scipen=100)
options(digits=2)
stat.desc(gss2016$SIBS)
stat.desc(gss2016$CHILDS, basic = TRUE, norm = TRUE)

gss2016SCS <- subset(gss2016, select = c(SIBS,CHILDS,SEX))
str(gss2016SCS)

gss2016SCSclean <- gss2016SCS[complete.cases(gss2016SCS),]
str(gss2016SCSclean)



partCor <- pcor(c("SIBS","CHILDS","SEX"), var(gss2016SCSclean))
partCor
partCor^2

#Part 2

regrSibsChilds <- lm(CHILDS ~ SIBS, gss2016)
regrSibsChilds
summary(regrSibsChilds)
sqrt(.03954)
#################################
