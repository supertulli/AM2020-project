setwd("/Users/andre/OneDrive/Ambiente de Trabalho/Project AM")
library(cluster)
library(purrr)

#-------MRMR dataset------------------------------------------------------------------------
data <- read.csv("WDI_afterMRMR.csv", header = TRUE, sep = ",")
row.names(data) = data[,1]
data = data[, -1]

#------------ kmeans---------- 
#SELECT K
#Elbow method - based on the plot, select k=3 (elbow)
wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$tot.withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Total within cluster sum of squares")}

wssplot(data)

#Silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:10
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#Choose k=2 or k=3
k2 = kmeans(data, 2, nstart=20)
k2$betweenss/k2$totss
k2
clusplot(data, k2$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'MRMR Kmeans, k=2')

k3 = kmeans(data, 3, nstart=20)
k3$betweenss/k3$totss
k3
clusplot(data, k3$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'MRMR Kmeans, k=3')

library(ClusterR)
#-------------kmedoids-----------
k2 = Cluster_Medoids(data,clusters=2,distance_metric="manhattan")
k2
clusplot(data, k2$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'MRMR Kmedoids, k=2')

k3 = Cluster_Medoids(data, clusters=3,distance_metric="manhattan")
k3
clusplot(data, k3$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'MRMR Kmedoids, k=3')

#------------Classical PCA----------------------------------------------------------------

data <- read.csv("WDI_classicalPCA.csv", header = TRUE, sep = ",")
row.names(data) = data[,1]
data = data[, -1]

#------------ kmeans---------- 
#SELECT K
#Elbow method - based on the plot, select k=3 (elbow)
wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$tot.withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Total within cluster sum of squares")}

wssplot(data)

#Silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:10
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#Choose k=2 or k=3
k2 = kmeans(data, 2, nstart=20)
k2$tot.withinss
k2
clusplot(data, k2$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'PCA Kmeans, k=2')

k3 = kmeans(data, 3, nstart=20)
k3$tot.withinss
k3
clusplot(data, k3$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'PCA Kmeans, k=3')

#-------------kmedoids-----------
k2 = Cluster_Medoids(data, clusters=2,distance_metric="manhattan")
k2
clusplot(data, k2$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'PCA Kmedoids, k=2')

k3 = Cluster_Medoids(data, clusters=3,distance_metric="manhattan")
k3
clusplot(data, k3$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'PCA Kmedoids, k=3')

#------------Robust PCA---------------------------------------------

data <- read.csv("WDI_robustPCA.csv", header = TRUE, sep = ",")
row.names(data) = data[,1]
data = data[, -1]

#------------ kmeans---------- 
#SELECT K
#Elbow method - based on the plot, select k=3 (elbow)
wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$tot.withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Total within cluster sum of squares")}

wssplot(data)

#Silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:10
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#Choose k=2 or k=3
k2 = kmeans(data, 2, nstart=20)
k2$tot.withinss
k2
clusplot(data, k2$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'ROBPCA Kmeans, k=2')

k3 = kmeans(data, 3, nstart=20)
k3$tot.withinss
k3
clusplot(data, k3$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'ROBPCA Kmeans, k=3')

#-------------kmedoids-----------
k2 = Cluster_Medoids(data, clusters=2,distance_metric="manhattan")
k2
clusplot(data, k2$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'ROBPCA Kmedoids, k=2')

k3 = Cluster_Medoids(data, clusters=3,distance_metric="manhattan")
k3
clusplot(data, k3$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'ROBPCA Kmedoids, k=3')
