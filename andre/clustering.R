getwd()
setwd("/Users/andre/OneDrive/Ambiente de Trabalho/Project AM")

library(cluster)
library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)
library(ggpubr)
library(purrr)

data <- read.csv("WDI.csv", header = TRUE, sep = ",")
row.names(data) <- data[,1]
#REMOVE row names and LABELS - UNSUPERVISED LEARNING
data <- select(data, c(-1, -72, -71))

#----------------------KMEANS----------------------------------

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

#Gap Statistic
set.seed(123)
gap_stat <- clusGap(data, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
#fviz_gap_stat(gap_stat)


#Based on the previous methods, use k=3 and k=4, and compare results
set.seed(3)
k3 = kmeans(data, 3, nstart=20)
k3$tot.withinss
k3

k4 = kmeans(data, 4, nstart=20)
k4$tot.withinss
k4

clust_names <- k4$cluster
data <- cbind(data, clust_names)

#----------------------Hierarchical Clustering----------------------------------

single<-agnes(data, metric = "euclidean",
               stand = FALSE, method = "single", keep.data = FALSE)
complete<-agnes(data, metric = "euclidean",
                stand = FALSE, method = "complete", keep.data = FALSE)
avg<-agnes(data, metric = "euclidean",
           stand = FALSE, method = "average", keep.data = FALSE)
ward<-agnes(data, metric = "euclidean",
            stand = FALSE, method = "ward", keep.data = FALSE)

pltree(single,main="Single linkage", cex=0.5, xlab="")
pltree(complete,main="Complete linkage",cex=0.5,xlab="")
pltree(avg,main="Average linkage",cex=0.5, xlab="")
pltree(ward,main="Ward Method",cex=0.5, xlab="")
#dendrograms also indicate that there should be either 3 or 4 clusters in our data
