library(dplyr)
library(caret)
library(e1071)
library(psych)
library(cluster)
library(fpc)
library(readr)
library(tidyverse)
library(ClusterR)
library(randomForest)
library(yardstick)
library(ggplot2)

set.seed(42)

#-------------data load--------------------------

WDI <- read_csv(".//WDI_shortnames.csv")
data <- WDI[,-c(1,71,72)] # removing the outcome variables
data <- data[, order(names(data))]
outcome <- WDI[,c(71,72)]
colnames(outcome) <- c('HDI_var', 'HDI_rank')
outcome$HDI_rank <- factor(outcome$HDI_rank, levels = c(0,1,2,3), 
                           labels = c("Negative", "Low", "Medium", "High"))

#--------------test train split------------------

trainIndex=createDataPartition(outcome$HDI_rank, p=0.75)$Resample1
y_train=outcome[trainIndex, ]
y_test=outcome[-trainIndex, ]
x_train=data[trainIndex, ]
x_test=data[-trainIndex, ]

#-----------Report metrics summary----------------

showMetrics=function(pred,obs){
  confmatrix<-confusionMatrix(data=pred,reference=obs);print(confmatrix)
  accuracy=sum(diag(confmatrix$table))/sum(confmatrix$table)
  message("Accuracy");print(round(accuracy,3))
  bAccuracy=bal_accuracy_vec(truth=obs, estimate=pred,estimator="macro")
  message("Balanced accuracy");print(round(bAccuracy,3))
}

#---------Standardization for kmeans-----------------

preproc.param <- x_train %>% preProcess(method = c("center", "scale"))
data.scaled <- preproc.param %>% predict(data)
x_train.scaled <- preproc.param %>% predict(x_train)
x_test.scaled <- preproc.param %>% predict(x_test)

#--------Getting subsets of the data by theme--------

economical <- data.scaled %>% dplyr::select(starts_with('eco')) # economical instant values
demographic <- data.scaled %>% dplyr::select(starts_with('dem')) #demographic instant values
education_science <- data.scaled %>% dplyr::select(starts_with('sci')) #education and science instant values
geographic <- data.scaled %>% dplyr::select(starts_with('geo')) #geographic instant values
health_sanitation <- data.scaled %>% dplyr::select(starts_with('hs')) #health and sanitation instant values

#-------------k-means clustering---------------------------

# economical
eco.train <- economical[trainIndex, ]
k.eco <- KMeans_rcpp(eco.train, clusters = 2, num_init = 25)
data$eco.clusters <- as.factor(predict_KMeans(economical, k.eco$centroids))
dis = dist(eco.train)
sil = silhouette (k.eco$clusters, dis)
windows() 
plot(sil)
clusplot(eco.train, k.eco$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# demographic
dem.train <- demographic[trainIndex, ]
k.dem <- KMeans_rcpp(dem.train, clusters = 4, num_init = 25)
data$dem.clusters <- as.factor(predict_KMeans(demographic, k.dem$centroids))
dis = dist(dem.train)
sil = silhouette (k.dem$clusters, dis)
windows() 
plot(sil)
clusplot(dem.train, k.dem$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# education_science
sci.train <- education_science[trainIndex, ]
k.sci <- KMeans_rcpp(sci.train, clusters = 2, num_init = 25)
#data$sci.clusters <- as.factor(predict_KMeans(education_science, k.sci$centroids))
dis = dist(sci.train)
sil = silhouette (k.sci$clusters, dis)
windows() 
plot(sil)
clusplot(sci.train, k.sci$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# geographic
geo.train <- geographic[trainIndex, ]#9
k.geo <- KMeans_rcpp(geo.train, clusters = 4, num_init = 25)
data$geo.clusters <- as.factor(predict_KMeans(geographic, k.geo$centroids))
dis = dist(geo.train)
sil = silhouette (k.geo$clusters, dis)
windows() 
plot(sil)
clusplot(geo.train, k.geo$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

# health_sanitation
hs.train <- health_sanitation[trainIndex, ] #15
k.hs <- KMeans_rcpp(hs.train, clusters = 5, num_init = 25)
data$hs.clusters <- as.factor(predict_KMeans(health_sanitation, k.hs$centroids))
dis = dist(hs.train)
sil = silhouette(k.hs$clusters, dis)
windows() 
plot(sil)
clusplot(hs.train, k.hs$clusters, color=TRUE, shade=TRUE, labels=4, lines=0)

#----------classification using cluster output--------------------------

c_data <- data %>% dplyr::select(ends_with('clusters'))
x_c_train = c_data[trainIndex, ]
x_c_test = c_data[-trainIndex, ]

Fit.RF <- randomForest(x_c_train, y_train$HDI_rank,
                       strata = y_train$HDI_rank,
                       mtry=3,
                       ntrees=2000,
                       importance=TRUE
                       ) 

pred <- predict(Fit.RF, x_c_test)
confusionMatrix(pred, y_test$HDI_rank)
showMetrics(pred, y_test$HDI_rank)
importance(Fit.RF)
