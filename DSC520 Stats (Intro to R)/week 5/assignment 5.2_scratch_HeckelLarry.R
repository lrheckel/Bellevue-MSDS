# libraries to be used
library(ggplot2)
library(readr)
library(Hmisc)
library(ggm)
library(polycor)
library(data.table)
library(purrr)

options(scipen=100)
options(digits=2)


# read the files
washDonation <- read_csv("donations-2015-2016-DC.txt")
nebrDonation <- read_csv("donations-2015-2016-NE.txt")
View(washDonation)
str(washDonation)
View(nebrDonation)
str(nebrDonation)

comboDonation <- rbind(washDonation, nebrDonation)
View(comboDonation)
str(comboDonation)

# list rows of data that have missing values 
comboNA <- comboDonation[!complete.cases(comboDonation),]
comboNA
View(comboNA)
str(comboNA)
colSums(is.na(comboNA))

comboClean <- na.omit(comboDonation)
View(comboClean)
comboClean

comboNAtry <- comboNA

library(dplyr)
comboNAtry <- comboNA %>% mutate(donor_zip=ifelse(is.na(donor_zip), 99999, donor_zip))
comboNAtry <- comboNAtry %>% mutate(donor_name=ifelse(is.na(donor_name), "Not Given", donor_name))
comboNAtry <- comboNAtry %>% mutate(donor_city=ifelse(is.na(donor_city), "Not Given", donor_city))
comboNAtry <- comboNAtry %>% mutate(donor_employer=ifelse(is.na(donor_employer), "Not Given", donor_employer))
comboNAtry <- comboNAtry %>% mutate(donor_occupation=ifelse(is.na(donor_occupation), "Not Given", donor_occupation))
comboNAtry <- comboNAtry %>% mutate(committee_party=ifelse(is.na(committee_party), party_affiliation, committee_party))

library(stringr)
# mandel <- subset.data.frame(comboDonation, comboDonation$candidate_name=="LINDBECK, STEVE")
# View(mandel)

mandel <- comboDonation %>%
  filter(str_detect(candidate_name, "LINDB"))
mandel
###########################
colSums(is.na(comboNAtry))

comboNAremaining <- comboNAtry[!complete.cases(comboNAtry),]
str(comboNAremaining)
View(comboNAremaining)


str(comboNAtry)

hist_comboNAtry <- ggplot(data=comboNAtry,aes(party_affiliation)) + 
     geom_bar()
hist_comboNAagain <- ggplot(data=comboNA,aes(party_affiliation)) + 
  geom_bar() 
hist_comboNAtry
hist_comboNAagain

hist_comboClean <- ggplot(data=comboClean,aes(x=donation_amount)) + 
  geom_histogram(binwidth=20000) 
hist_comboClean

min(comboClean$donation_amount)
max(comboClean$donation_amount)
mean(comboClean$donation_amount)
median(comboClean$donation_amount)
donCount <- count(comboClean, vars=donation_amount > 0)
View(donCount)

comboCleanDonation <- subset(comboClean,subset = (donation_amount > 0))
str(comboCleanDonation)

boxplot_comboCleanDonation <- ggplot(data=comboCleanDonation,aes(y=donation_amount)) +
  geom_boxplot() 
boxplot_comboCleanDonation

hist_comboCleanDonation_Norm <- ggplot(data=comboCleanDonation,aes(x=donation_amount)) + 
  geom_histogram(aes(y=..count.., fill=..count..)) +
  scale_x_log10("donation_amount") +
  ggtitle("Donation Amount Histogram") +
  ylab("Number of Donations") 
hist_comboCleanDonation_Norm

summary(comboCleanDonation$donation_amount)

donationHigh <- subset(comboCleanDonation, comboCleanDonation$donation_amount > 10000)
str(donationHigh)
View(donationHigh)

str(comboCleanDonation)

AvgDonAmt <- c(mean(comboCleanDonation$donation_amount))
names(AvgDonAmt) <- c("Average Donation Amount")
print(AvgDonAmt)

comboCleanDonationAvg <- cbind(comboCleanDonation, AvgDonAmt)
str(comboCleanDonationAvg)

comboNAremaining1 <- comboCleanDonationAvg[complete.cases(comboCleanDonationAvg),]
str(comboNAremaining1)


library(psych)
options(scipen=100)
options(digits=2)
summary(comboCleanDonation$donation_amount)
describe(comboCleanDonationAvg$donation_amount)
#donationStats <- stat.desc(comboCleanDonationAvg$donation_amount,norm = TRUE)
#donationStats
################

