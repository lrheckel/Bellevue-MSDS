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
library(esquisse)
#library(FNN)
#library(caret)
#library(class)
library(factoextra)
library(purrr)

options(scipen=100)
options(digits=5)


# read the files
clusterData <- read_csv("clustering-data.csv")

#esquisser()
set.seed(1206)

ggplot(data = clusterData) +
  aes(x = x, y = y) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Cluster Data',
    x = 'X data',
    y = 'Y data') +
  theme_grey()

### Cluster k=2

k2 <- kmeans(clusterData, centers=2, nstart=25)
str(k2)
k2
k2_graph <- fviz_cluster(k2, clusterData)
k2_graph


k3 <- kmeans(clusterData, centers=3, nstart=25)
str(k3)
k3
k3_graph <- fviz_cluster(k3, clusterData)
k3_graph


k4 <- kmeans(clusterData, centers=4, nstart=25)
str(k4)
k4
k4_graph <- fviz_cluster(k4, clusterData)
k4_graph


k5 <- kmeans(clusterData, centers=5, nstart=25)
str(k5)
k5
k5_graph <- fviz_cluster(k5, clusterData)
k5_graph


k6 <- kmeans(clusterData, centers=6, nstart=25)
str(k6)
k6
k6_graph <- fviz_cluster(k6, clusterData)
k6_graph


k7 <- kmeans(clusterData, centers=7, nstart=25)
str(k7)
k7
k7_graph <- fviz_cluster(k7, clusterData)
k7_graph


k8 <- kmeans(clusterData, centers=8, nstart=25)
str(k8)
k8
k8_graph <- fviz_cluster(k8, clusterData)
k8_graph


k9 <- kmeans(clusterData, centers=9, nstart=25)
str(k9)
k9
k9_graph <- fviz_cluster(k9, clusterData)
k9_graph


k10 <- kmeans(clusterData, centers=10, nstart=25)
str(k10)
k10
k10_graph <- fviz_cluster(k10, clusterData)
k10_graph


k11 <- kmeans(clusterData, centers=11, nstart=25)
str(k11)
k11
k11_graph <- fviz_cluster(k11, clusterData)
k11_graph


k12 <- kmeans(clusterData, centers=12, nstart=25)
str(k12)
k12
k12_graph <- fviz_cluster(k12, clusterData)
k12_graph


withinSumSq <- function(k) {kmeans(clusterData, k, nstart=10)$tot.withinss/4022}

#set the values of k, from 2-12
kValues <- 2:12

#compute the within Sum of Squares for 2-12 clusters
withinSSValues <- map_dbl(kValues, withinSumSq)

#plot the line chart of k-value versus Sum of Squares
plot(kValues, withinSSValues, type="b", pch=19, frame=FALSE,
     xlab="Number of Clusters K", ylab="Average within-clusters Sum of Squares")

########  good above here

