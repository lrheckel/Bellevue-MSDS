#libraries to use
library(ggplot2)
library(readxl)
library(DataExplorer)
library(ggm)
# library(readr)
# library(dplyr)
# library(stringr)
# library(psych)
# library(janitor)
# library(car)
# library(QuantPsyc)
# library(pastecs)
# library(sqldf)


options(scipen=100)
options(digits=2)


# read the files
housing <- read_excel("week-6-housing.xlsx")

View(housing)
str(housing)
head(housing)

config <- list(
  "introduce" = list(),
  "plot_str" = list(
    "type" = "diagonal",
    "fontSize" = 35,
    "width" = 1000,
    "margin" = list("left" = 350, "right" = 250)
  ),
  "plot_missing" = list(),
  "plot_histogram" = list(),
  "plot_qq" = list(sampled_rows = 1000L),
  "plot_bar" = list(),
  "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
  #  "plot_prcomp" = list(),
  "plot_boxplot" = list(),
  "plot_scatterplot" = list(sampled_rows = 1000L)
)



plot_missing(housing)
#create_report(housing, config=config)

library(esquisse)
esquisse::esquisser() #helps in launching the add-in

ggplot(data = housing) +
  aes(x = `Sale Price`) +
  geom_histogram(bins = 50, fill = '#0c4c8a') +
  theme_minimal()

ggplot(data = housing) +
  aes(x = sq_ft_lot) +
  geom_histogram(bins = 50, fill = '#0c4c8a') +
  theme_minimal()

ggplot(data = housing) +
  aes(x = log10(sq_ft_lot)) +
  geom_histogram(bins = 50, fill = '#0c4c8a') +
  theme_minimal()

housingPrice <- subset(housing, `Sale Price` <= 2000000)
head(housingPrice)

str(housing)
str(housingPrice)

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

########################3
########################








