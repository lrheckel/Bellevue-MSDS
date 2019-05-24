#libraries to use
library(ggplot2)
library(readxl)
library(DataExplorer)
library(ggm)
library(readr)
library(dplyr)
# library(stringr)
# library(psych)
# library(janitor)
library(car)
library(QuantPsyc)
# library(pastecs)
# library(sqldf)


options(scipen=100)
options(digits=5)


# read the file
housing <- read_excel("week-7-housing.xlsx")

# mean(housing$`Sale Price`)
# range(housing$`Sale Price`)
# 
# mean(housing$bedrooms)
# range(housing$bedrooms)
# 
# mean(housing$bath_full_count)
# range(housing$bath_full_count)

View(housing)
#str(housing)
#head(housing)
glimpse(housing)

# config <- list(
#   "introduce" = list(),
#   "plot_str" = list(
#     "type" = "diagonal",
#     "fontSize" = 35,
#     "width" = 1000,
#     "margin" = list("left" = 350, "right" = 250)
#   ),
#   "plot_missing" = list(),
#   "plot_histogram" = list(),
#   "plot_qq" = list(sampled_rows = 1000L),
#   "plot_bar" = list(),
#   "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
#   #  "plot_prcomp" = list(),
#   "plot_boxplot" = list(),
#   "plot_scatterplot" = list(sampled_rows = 1000L)
# )
# #create_report(housing, config=config)


plot_missing(housing)

# library(esquisse)
# esquisse::esquisser() #helps in launching the add-in

ggplot(data = housing) +
  aes(x = bedrooms) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  theme_grey()

ggplot(data = housing) +
  aes(x = bath_full_count) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  theme_grey()

ggplot(data = housing) +
  aes(x = `Sale Price`) +
  geom_histogram(bins = 50, fill = '#0c4c8a') +
  theme_grey()

ggplot(data = housing) +
  aes(x = sq_ft_lot) +
  geom_histogram(bins = 50, fill = '#0c4c8a') +
  theme_grey()

ggplot(data = housing) +
  aes(x = log10(sq_ft_lot)) +
  geom_histogram(bins = 50, fill = '#0c4c8a') +
  theme_grey()

dplyr::count(housing, bedrooms)
dplyr::count(housing, bath_full_count)


summary(housing$sq_ft_lot)
str(filter(housing, bath_full_count == 23))

housingClean <- subset(housing, bath_full_count <= 6)
glimpse(housingClean)

regrLotSize <- lm(`Sale Price` ~ log10(sq_ft_lot), data=housingClean)
regrBedBath <- lm(`Sale Price` ~ bedrooms + bath_full_count, data=housingClean)
#regrBathBed <- lm(`Sale Price` ~ bedrooms + bath_full_count, data=housingClean)
#regrBathBed1 <- lm(`Sale Price` ~ bedrooms + bath_full_count, data=housing)


summary(regrLotSize) 
summary(regrBedBath)
#summary(regrBathBed)
#summary(regrBathBed1)

lm.beta(regrLotSize)
lm.beta(regrBedBath)

confint(regrLotSize)
confint(regrBedBath)

housingClean$residuals <- resid(regrBedBath)
housingClean$standardized.residuals <- rstandard(regrBedBath)
housingClean$studentized.residuals <- rstudent(regrBedBath)
housingClean$cooks.distance <- cooks.distance(regrBedBath)
housingClean$dfbeta <- dfbeta(regrBedBath)
housingClean$dffit <- dffits(regrBedBath)
housingClean$leverage <- hatvalues(regrBedBath)
housingClean$covariance.ratios <- covratio(regrBedBath)

write.table(housingClean, "Housing Clean with Diagnostics.csv", sep=",", row.names=FALSE)

housingClean$large.residual <- housingClean$standardized.residuals > 2 | housingClean$standardized.residuals < -2

glimpse(housingClean$large.residual)

sum(housingClean$large.residual)

housingLarge <- subset(housingClean, large.residual == TRUE)
glimpse(housingLarge)
nrow(housingLarge)
percentLarge = 100 * (sum(housingClean$large.residual)/nrow(housingClean))
percentLarge

#housingInvestigate <- housingLarge[c("cooks.distance", "leverage", "covariance.ratios")]
#View(housingInvestigate)
summary(housingLarge$cooks.distance)
summary(housingLarge$leverage)
summary(housingLarge$covariance.ratios)

leverage2avg <- mean(housingClean$leverage) * 2
leverage2avg
housingLeverage <- subset(housingLarge, housingLarge$leverage > leverage2avg)
nrow(housingLeverage)

lowCVR = 1 - 3 * (mean(housingClean$leverage))
highCVR = 1 + 3 * (mean(housingClean$leverage))
lowCVR
highCVR

housingCVR <- subset(housingLarge, (housingLarge$covariance.ratios<lowCVR | housingLarge$covariance.ratios>highCVR))
nrow(housingCVR)

durbinWatsonTest(regrBedBath)

vif(regrBedBath)
1/vif(regrBedBath)
mean(vif(regrBedBath))

plot(regrBedBath)

regrBedBath$model[[1]][2851]
regrBedBath$model[[1]][8886]
regrBedBath$model[[1]][11981]

hist(housingClean$standardized.residuals)
hist(housingClean$studentized.residuals)

mse


##########################################################



#housing
ggplot(data = housing) +
  aes(x = log10(sq_ft_lot), y = `Sale Price`) +
  geom_point(color = '#0c4c8a') +
  geom_smooth(method=lm, span = 0.75) +
  labs(title = 'Housing Graph',
       x = 'Square Footage',
       y = 'Sale Price') +
  theme_minimal()

#housingPrice
ggplot(data = housingPrice) +
  aes(x = log10(sq_ft_lot), y = `Sale Price`) +
  geom_point(color = '#0c4c8a') +
  geom_smooth(method=lm, span = 0.75) +
  labs(title = 'Housing Price Graph',
       x = 'Square Footage',
       y = 'Sale Price') +
  theme_minimal()

#covariance
housingCov <- cov(log10(housing$sq_ft_lot), housing$`Sale Price`, method="pearson")
housingPriceCov <- cov(log10(housingPrice$sq_ft_lot), housingPrice$`Sale Price`, method="pearson")
housingCov
housingPriceCov

#spearman correlation test
housingSpear <- cor.test(log10(housing$sq_ft_lot), housing$`Sale Price`, method = "spearman")
housingPriceSpear <- cor.test(log10(housingPrice$sq_ft_lot), housingPrice$`Sale Price`, method = "spearman")
housingSpear
housingPriceSpear

#correlation and coeff of determination
housingCor <- cor(log10(housing$sq_ft_lot),housing$`Sale Price`, method = "spearman")
housingCoefDet <- cor(log10(housing$sq_ft_lot),housing$`Sale Price`, method = "spearman")^2
housingPriceCor <- cor(log10(housingPrice$sq_ft_lot),housingPrice$`Sale Price`, method = "spearman")
housingPriceCoefDet <- cor(log10(housingPrice$sq_ft_lot),housingPrice$`Sale Price`, method = "spearman")^2

housingCor
housingPriceCor
housingCoefDet
housingPriceCoefDet

#graph
#housing
ggplot(data = housing) +
  aes(x = log10(sq_ft_lot), y = `Sale Price`) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Housing Graph',
       x = 'Square Footage',
       y = 'Sale Price') +
  theme_minimal()

#housingPrice
ggplot(data = housingPrice) +
  aes(x = log10(sq_ft_lot), y = `Sale Price`) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Housing Price Graph',
       x = 'Square Footage',
       y = 'Sale Price') +
  theme_minimal()

#partial correlation
housingPcor <- pcor(c('Sale Price',"sq_ft_lot", "bedrooms","square_feet_total_living"),var(housing))
housingPricePcor <- pcor(c('Sale Price',"sq_ft_lot", "bedrooms","square_feet_total_living"),var(housingPrice))
housingPcor
housingPricePcor
housingPcor^2
housingPricePcor^2

#regression analysis
housingRegr <- lm(`Sale Price` ~ sq_ft_lot, data=housing)
housingPriceRegr <- lm(`Sale Price` ~ sq_ft_lot, data=housingPrice)
housingRegr
housingPriceRegr
summary(housingRegr)
summary(housingPriceRegr)
sqrt(0.0144) #housing corr coeff
sqrt(0.0154) #housingPrice corr coeff

#Sale Price averages
housingSaleMean <- mean(housing$`Sale Price`)
housingPriceSaleMean <- mean(housing$`Sale Price`)

#Get the model results
housingModelSale <- mean(housing$sq_ft_lot)*0.851 + 641821.406
housingPriceModelSale <- mean(housingPrice$sq_ft_lot)*0.586 + 608263.293

#how good are the results
housingSaleMean
housingModelSale


housingPriceSaleMean
housingPriceModelSale

