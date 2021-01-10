getwd()
setwd("/onedrive/OneDrive - Nokia/scicolab/ist/MA/2020/proj/GIT-AM2020-project/AM2020-project/data")

library(caret)
library(e1071)
library(psych)
library(cluster)
library(fpc)
library(readr)
library(dplyr)
library(tidyverse)
library(ClusterR)
library(vegan)
library(fastDummies)
library(randomForest)
library(yardstick)
library(pROC)
library(ggplot2)
library(factoextra)
library(NbClust)
library(ggpubr)
library(purrr)

set.seed(42)

#-------------data load--------------------------

WDI <- read_csv(".//WDI_shortnames.csv")
data <- WDI[,-c(1,71,72)] # removing the outcome variables
data <- data[, order(names(data))]
outcome <- WDI[,c(71,72)]
colnames(outcome) <- c('HDI_var', 'HDI_rank')
outcome$`HDI_rank` <- factor(outcome$`HDI_rank`, levels = c(0,1,2,3), labels = c("Negative", "Low", "Medium", "High"))

#--------Getting subsets of the data by theme--------

economical <- data %>% select(starts_with('eco')) # economical instant values
demographic <- data %>% select(starts_with('dem')) #demographic instant values
education_science <- data %>% select(starts_with('sci')) #education and science instant values
geographic <- data %>% select(starts_with('geo')) #geographic instant values
health_sanitation <-data %>% select(starts_with('hs')) #health and sanitation instant values

#---------Naive Bayes-----------------------

trainIndex=createDataPartition(outcome$`HDI_rank`, p=0.7)$Resample1
y_train=outcome[trainIndex, ]
y_test=outcome[-trainIndex, ]
x_train=data[trainIndex, ]
x_test=data[-trainIndex, ]

showMetrics=function(model){
  handlem<-function(model){
    tryCatch(predict(model, newdata=x_test, type="class"), error = function(e) model)
  }
  pred=handlem(model)

  conf=table(pred,y_test$`HDI_rank`)
  message("Confusion matrix")
  print(conf)
  
  accuracy=sum(diag(conf))/sum(conf)
  message("Accuracy")
  print(round(accuracy,3))
  
  bAccuracy=bal_accuracy_vec(y_test$`HDI_rank`,pred,estimator="macro")
  message("Balanced accuracy")
  print(round(bAccuracy,3))
}

NBclassifier=naiveBayes(y_train$`HDI_rank` ~ ., data=x_train, laplace=0.1)
confusionMatrix(predict(NBclassifier, newdata=x_test, type="class"), y_test$HDI_rank)
showMetrics(NBclassifier)


#----------------------K-means clustering----------------------------------

#SELECT K
#Elbow method - based on the plot, select k=3 (elbow)
wssplot <- function(x_train, nc=10, seed=1234){
  wss <- (nrow(x_train)-1)*sum(apply(x_train,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(x_train, centers=i)$tot.withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Total within cluster sum of squares")}

wssplot(x_train)

#Silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(x_train, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(x_train))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#Gap Statistic
gap_stat <- clusGap(x_train, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#Based on the previous methods, use k=3 and k=4, and compare results
k3 = KMeans_rcpp(x_train, clusters = 3, num_init = 25)
k3

k4 = KMeans_rcpp(x_train, clusters = 4, num_init = 25)
k4

k3$between.SS_DIV_total.SS
k4$between.SS_DIV_total.SS

data$clusters <- as.factor(predict_KMeans(data, k4$centroids))

clusplot(x_train, k3$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)


#-------------k-medoids clustering---------------------------

# economical
eco.train <- economical[trainIndex, ]
k.eco <- Cluster_Medoids(eco.train,clusters=3,distance_metric="manhattan")
data$eco.clusters <- as.factor(predict_Medoids(economical,MEDOIDS=k.eco$medoids, distance_metric="manhattan")$clusters)
dis = dist(eco.train)
sil = silhouette (k.eco$clusters, dis)
#windows() 
#plot(sil)
clusplot(eco.train, k.eco$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# demographic
dem.train <- demographic[trainIndex, ]
k.dem <- Cluster_Medoids(demographic[trainIndex, ],clusters=2,distance_metric="manhattan")
data$dem.clusters <- as.factor(predict_Medoids(demographic,MEDOIDS=k.dem$medoids, distance_metric="manhattan")$clusters)
dis = dist(dem.train)
sil = silhouette (k.dem$clusters, dis)
#windows() 
#plot(sil)
clusplot(dem.train, k.dem$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# education_science
sci.train <- education_science[trainIndex, ]
k.sci <- Cluster_Medoids(education_science[trainIndex, ],clusters=3,distance_metric="manhattan")
data$sci.clusters <- as.factor(predict_Medoids(education_science,MEDOIDS=k.sci$medoids, distance_metric="manhattan")$clusters)
dis = dist(sci.train)
sil = silhouette (k.sci$clusters, dis)
#windows() 
#plot(sil)
clusplot(sci.train, k.sci$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# geographic
geo.train <- geographic[trainIndex, ]
k.geo <- Cluster_Medoids(geographic[trainIndex, ],clusters=4,distance_metric="manhattan")
data$geo.clusters <- as.factor(predict_Medoids(geographic,MEDOIDS=k.geo$medoids, distance_metric="manhattan")$clusters)
dis = dist(geo.train)
sil = silhouette (k.geo$clusters, dis)
#windows() 
#plot(sil)
clusplot(geo.train, k.geo$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# health_sanitation
hs.train <- health_sanitation[trainIndex, ]
k.hs <- Cluster_Medoids(health_sanitation[trainIndex, ],clusters=3,distance_metric="manhattan")
data$hs.clusters <- as.factor(predict_Medoids(health_sanitation,MEDOIDS=k.hs$medoids, distance_metric="manhattan")$clusters)
dis = dist(hs.train)
sil = silhouette(k.hs$clusters, dis)
#windows() 
#plot(sil)
clusplot(hs.train, k.hs$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

#----------classification using cluster output--------------------------

c_data <- dummy_cols(data %>% select(ends_with('clusters'))) %>% select(!ends_with('clusters'))
x_c_train = c_data[trainIndex, ]
x_c_test = c_data[-trainIndex, ]

Fit.RF <- randomForest(x_c_train,y_train$HDI_rank,
                       classwt = c(0.25,0.25,0.25,0.25),
                       mtry=16,
                       ntrees=1000,
                       importance=TRUE
                       ) 

pred <- predict(Fit.RF, x_c_test)
confusionMatrix(pred, y_test$HDI_rank)
showMetrics(pred)
importance(Fit.RF)

#----------classification using wdi data--------------------------

Fit.RF <- randomForest(x_train,y_train$HDI_rank,
                       classwt = c(0.25,0.25,0.25,0.25),
                       mtry=16,
                       ntrees=1000,
                       importance=TRUE
                       ) 

pred <- predict(Fit.RF, x_test)
confusionMatrix(pred, y_test$HDI_rank)
showMetrics(pred)
importance(Fit.RF)

#----------classification using both--------------------------

Fit.RF <- randomForest(merge(x_train, x_c_train, by.x = 0, by.y = 0),y_train$HDI_rank,
                       classwt = c(0.25,0.25,0.25,0.25),
                       mtry=16,
                       ntrees=1000,
                       importance=TRUE
                       ) 

pred <- predict(Fit.RF, merge(x_test, x_c_test, by.x = 0, by.y = 0))
confusionMatrix(pred, y_test$HDI_rank)
showMetrics(pred)
importance(Fit.RF)

