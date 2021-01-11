# generic libraries
library(devtools) # close png
library(caret)

#-----------------------------------------------------------------
# import data
data<-read.csv("..//mRMR_reduced_data.csv", header = TRUE, sep = ",")
row.names(data) = data[,1] # set row names
data = data[, -1] # remove row names

# save the oucome in classes
outcome <- data[,15] 
outcome <- factor(outcome, levels = c(0,1,2,3), 
                  labels = c("Negative", "Low", "Medium", "High"))

data = data[, -15] # remove the outcome

trainIndex = createDataPartition(outcome, p=0.8)$Resample1

# Split data into train and test
x_train=data[trainIndex, ]
x_test=data[-trainIndex, ]
y_train=outcome[trainIndex]
y_test=outcome[-trainIndex]


#----------Principal Component Analysis----------------------------
#libaries
library(rrcov)
library(DataExplorer)

# Classical PCA --------------------------------------------------
# apply classical pca
results_pcaClassic<-PcaClassic(x_train, scale = TRUE)
summary(results_pcaClassic)

# choice of k (number of principal components)
# 1.Find *k_1* such that lambda_i >= \bar{lambda} (which is 1, since we are working with standardized variables) for i = 1:k_1
k_1 = 0; for (value in results_pcaClassic$eigenvalues){ if (value >=  1) k_1 = k_1+1};k_1 # show the value of k_1
# 2.Find k_0.8 such that the first k_0.8 PC's explain 80% of the variance
{k_0.8 = 0;
  # compute the cumulative proportion of explained variance
cumVar_pcaClassic<-cumsum(results_pcaClassic$eigenvalues/sum(results_pcaClassic$eigenvalues));
for (value in cumVar_pcaClassic){if (value < 0.8) k_0.8 = k_0.8+1}
if(cumVar_pcaClassic[k_0.8] < 0.8) k_0.8 = k_0.8+1 # if the threshold of the 80% has not yet been reached, add one more (PC_k_0.8 is the first component after reaching the 80% threshold)
k_0.8} # show the value of k_0.8
# 3.Choose the minimum k
k = min(k_1,k_0.8);k

# confirm the value of k by plotting the eigenvalues and finding the elbow
png(filename = "figures/scree_pcaClassic.png", width = 800, height = 600)
screeplot(results_pcaClassic, 
          type="lines",
          cex = 0.2, 
          npcs = length(results_pcaClassic$eigenvalues),
          main = "Eigenvalues of each component"); abline(h = mean(results_pcaClassic$eigenvalues), col = 3)
dev.off()

# loadings visual analysis
bp_classic_names <- c(rep("PC1" , dim(results_pcaClassic$loadings)[1]) , rep("PC2" , dim(results_pcaClassic$loadings)[1]) , 
                rep("PC3" , dim(results_pcaClassic$loadings)[1]), rep("PC4" , dim(results_pcaClassic$loadings)[1]))
variables <- rep(rownames(results_pcaClassic$loadings),4)
bp_classic_loadings <- c(results_pcaClassic$loadings[,1], results_pcaClassic$loadings[,2], results_pcaClassic$loadings[,3], results_pcaClassic$loadings[,4])
bp_data_classic <- data.frame(bp_classic_names,variables,bp_classic_loadings)
# plot
png(filename = "figures/loadings_pcaClassic.png", width = 800, height = 600)
ggplot(bp_data_classic, aes(fill=bp_classic_loadings, y=variables, x=bp_classic_loadings, size=4)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~bp_classic_names, nrow = 2) +
  theme(legend.position="none") +
  xlab("Relative Importance")
dev.off()

# tranform both the training and test set
data_afterPCA<-predict(results_pcaClassic, data)[,1:k] # using the full dataset 


# Robust PCA --------------------------------------------------
# apply robust pca
results_pcaRobust<-PcaHubert(x_train,scale=TRUE,crit.pca.distances = 0.999)
summary(results_pcaRobust)

# plot the eigenvalues and find the elbow
png(filename = "figures/scree_pcaRobust.png", width = 800, height = 600)
screeplot(results_pcaRobust, 
          type="lines",
          cex = 0.2, 
          npcs = length(results_pcaRobust$eigenvalues),
          main = "Eigenvalues of each component"); abline(h = mean(results_pcaRobust$eigenvalues), col = 3)
dev.off()

#plot the outliers detected by the robust pca
png(filename = "figures/outliers_pcaRobust.png", width = 800, height = 600)
plot(results_pcaRobust,pch=20,lwd=2,col=(2-results_pcaRobust$flag))
dev.off()

# loadings visual analysis
bp_robust_names <- c(rep("PC1" , dim(results_pcaRobust$loadings)[1]) , rep("PC2" , dim(results_pcaRobust$loadings)[1]) , rep("PC3" , dim(results_pcaRobust$loadings)[1]))
variables <- rep(rownames(results_pcaRobust$loadings),3)
bp_robust_loadings <- c(results_pcaRobust$loadings[,1], results_pcaRobust$loadings[,2], results_pcaRobust$loadings[,3])
bp_robust_data <- data.frame(bp_robust_names,variables,bp_robust_loadings)
# plot
png(filename = "figures/loadings_pcaRobust.png", width = 800, height = 600)
ggplot(bp_robust_data, aes(fill=bp_robust_loadings, y=variables, x=bp_robust_loadings)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~bp_robust_names) +
  theme(legend.position="none") +
  xlab("Relative Importance")
dev.off()

# transform data (only 3 PC's so consider the whole set)
data_afterROBPCA<-predict(results_pcaRobust,data)

write.csv(data_afterPCA, file = "data_afterPCA.csv")
write.csv(data_afterROBPCA, file = "data_afterROBPCA.csv")

#----------------------------------------------------------------
#-----------Clustering-------------------------------------------
#----------------------------------------------------------------
library(dplyr)
library(cluster)
library(purrr)
library(ClusterR)
library(tidyverse)

#-------MRMR dataset------------------------------------------------------------------------
#standardize data
data.unscaled=data
data=scale(data, center = TRUE, scale = TRUE)

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
clusplot(data, k3$cluster, color=TRUE, labels=4, lines=0, main = 'MRMR Kmeans, k=3', col.p = outcome)
legend("topright", inset=.02, title="HDI class",
       c("Negative", "Low", "Medium", "High"), fill=c("yellow", "black", "red", "green"), cex=0.7)

#write to csv
clust_names <- k3$cluster
data <- cbind(data, clust_names)
write.csv(data, 'WDI_CLUSTERING.csv')


#-------------kmedoids-----------
k2 = Cluster_Medoids(data,clusters=2,distance_metric="manhattan")
k2
clusplot(data, k2$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'MRMR Kmedoids, k=2')

k3 = Cluster_Medoids(data, clusters=3,distance_metric="manhattan")
k3
clusplot(data, k3$clusters, color=TRUE, labels=4, lines=0,
         main = 'MRMR Kmedoids, k=3', col.p = outcome)
legend("topright", inset=.02, title="HDI class",
       c("Negative", "Low", "Medium", "High"), fill=c("yellow", "black", "red", "green"), cex=0.7)


#------------Classical PCA----------------------------------------------------------------
data_afterPCA

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

wssplot(data_afterPCA)

#Silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(data_afterPCA, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data_afterPCA))
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
k2 = kmeans(data_afterPCA, 2, nstart=20)
k2$tot.withinss
k2
clusplot(data_afterPCA, k2$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'PCA Kmeans, k=2')

k3 = kmeans(data_afterPCA, 3, nstart=20)
k3$tot.withinss
k3
clusplot(data_afterPCA, k3$cluster, color=TRUE, labels=4, lines=0, main = 'PCA Kmeans, k=3', col.p = outcome)
legend("topleft", inset=.02, title="HDI class",
       c("Negative", "Low", "Medium", "High"), fill=c("yellow", "black", "red", "green"), cex=0.7)

#-------------kmedoids-----------
k2 = Cluster_Medoids(data_afterPCA, clusters=2,distance_metric="manhattan")
k2
clusplot(data_afterPCA, k2$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'PCA Kmedoids, k=2')

k3 = Cluster_Medoids(data_afterPCA, clusters=3,distance_metric="manhattan")
k3
clusplot(data_afterPCA, k3$clusters, color=TRUE, labels=4, lines=0, main = 'PCA Kmedoids, k=3', col.p = outcome)
legend("topleft", inset=.02, title="HDI class",
       c("Negative", "Low", "Medium", "High"), fill=c("yellow", "black", "red", "green"), cex=0.7)

#------------Robust PCA---------------------------------------------
#perform clustering only on the 2 first robust principal components
data_afterROBPCA <- data_afterROBPCA[,-3]

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

wssplot(data_afterROBPCA)

#Silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(data_afterROBPCA, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data_afterROBPCA))
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
k2 = kmeans(data_afterROBPCA, 2, nstart=20)
k2$tot.withinss
k2
clusplot(data_afterROBPCA, k2$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'ROBPCA Kmeans, k=2')

k3 = kmeans(data_afterROBPCA, 3, nstart=20)
k3$tot.withinss
k3
clusplot(data_afterROBPCA, k3$cluster, color=TRUE, labels=4, lines=0, main = 'ROBPCA Kmeans, k=3', col.p = outcome)
legend("bottomright", inset=.02, title="HDI class",
       c("Negative", "Low", "Medium", "High"), fill=c("yellow", "black", "red", "green"), cex=0.6)

#-------------kmedoids-----------
k2 = Cluster_Medoids(data_afterROBPCA, clusters=2,distance_metric="manhattan")
k2
clusplot(data_afterROBPCA, k2$clusters, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'ROBPCA Kmedoids, k=2')

k3 = Cluster_Medoids(data_afterROBPCA, clusters=3, distance_metric="manhattan")
k3
clusplot(data_afterROBPCA, k3$clusters, color=TRUE, labels=4, lines=0, main = 'ROBPCA Kmedoids, k=3', col.p = outcome)
legend("bottomright", inset=.02, title="HDI class",
       c("Negative", "Low", "Medium", "High"), fill=c("yellow", "black", "red", "green"), cex=0.6)


#--------------------------Classification----------------

library(e1071)
library(tidyverse)
library(MASS)
library(klaR)
library(yardstick)
library(nnet)

set.seed(42)

#-------------data load--------------------------

# data sets so far:
head(data_afterROBPCA)
head(data_afterPCA)

# scaled data
head(data)
# unscaled data
head(data.unscaled)

#--------------test train split------------------

x_ROBPCA_train=data_afterROBPCA[trainIndex, ]
x_ROBPCA_test=data_afterROBPCA[-trainIndex, ]
x_PCA_train=data_afterPCA[trainIndex, ]
x_PCA_test=data_afterPCA[-trainIndex, ]


#-----------Report metrics summary----------------

showMetrics=function(pred,obs){
  confmatrix<-confusionMatrix(data=pred,reference=obs);print(confmatrix)
  accuracy=sum(diag(confmatrix$table))/sum(confmatrix$table)
  message("Accuracy");print(round(accuracy,3))
  bAccuracy=bal_accuracy_vec(truth=obs, estimate=pred,estimator="macro")
  message("Balanced accuracy");print(round(bAccuracy,3))
}

#---------Naive Bayes (baseline)-------------------------------

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(y=y_train$HDI_rank, x=x_train, trControl=train_control, method="nb")
showMetrics(model$pred[,1],model$pred[,2])

#------filter taking statistical analysis into account----------

saOut = c("dem.MortalityInfant","dem.BirthRate.var",
          "hs.DrinkingWater","dem.PopGrowth",
          "dem.MortalityUnder5.var","eco.CO2Emissions",
          "sci.EduExpense","dem.LifeExpectancy",
          "dem.DeathRate.var","eco.AgeDependancyRate",
          "hs.BasicSanitation","hs.GovHealthExpend",
          "dem.AdolescentFertRate.var","dem.BirthRate")

x_train <- x_train[,saOut]
x_test <- x_test[,saOut]

#---------Standardization (convenient for LDA)------------------

preproc.param <- x_train %>% preProcess(method = c("center", "scale"))
x_train.scaled <- preproc.param %>% predict(x_train)
x_test.scaled <- preproc.param %>% predict(x_test)

#-----------------------LDA-------------------------------------

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(y=y_train$HDI_rank, x=x_train.scaled, trControl=train_control, 
              prior = c(0.07,0.31,0.31,0.31), method="lda")
showMetrics(model$pred[,1],model$pred[,2])

#-----------------------QDA-------------------------------------

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(y=y_train$HDI_rank, x=x_train.scaled, trControl=train_control, 
              prior = c(0.07,0.31,0.31,0.31), method="qda")
showMetrics(model$pred[,1],model$pred[,2])

#-----------------------KNN-------------------------------------

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(y=y_train$HDI_rank, x=x_train.scaled, trControl=train_control, 
              method="knn", tuneLength = 20)
model$results
# The final value used for the model was k = 15.
showMetrics(model$pred[,1],model$pred[,2])

#--------------multinomial Logistic regression-------------------

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(y=y_train$HDI_rank, x=x_train.scaled, trControl=train_control, 
              method="multinom")
model$results
# The final value used for the model was k = 15.
showMetrics(model$pred[,1],model$pred[,2])




#-----------------------final-------------------------------------
# LDA outperformed others

LDAclassifier <- lda(x=x_train.scaled, grouping=y_train$HDI_rank, 
                     prior = c(0.07,0.31,0.31,0.31), CV=FALSE)
predictions <- predict(LDAclassifier, newdata=x_test.scaled)
confmatrix<-confusionMatrix(predictions$class, y_test$HDI_rank)
showMetrics(predictions$class, y_test$HDI_rank)
lda.data <- cbind(x_train.scaled, predict(LDAclassifier)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = y_train$`HDI_rank`))


#--------------Classification after Clustering----------------

library(randomForest)
library(caret)
library(klaR)
library(dplyr)
library(yardstick)

#-------------data load--------------------------


#Dataset after clustering
WDI_CLUSTERING <- read.csv("WDI_CLUSTERING.csv")

cluster_data <- WDI_CLUSTERING[, -c(1)]

c_data <- cluster_data[, -c(15)]

#Cluster output
c_data_outcome <- cluster_data[,c(14,15)]

colnames(c_data_outcome) <- c('HDI_var', 'HDI_rank')

c_data_outcome$`HDI_rank` <- factor(c_data_outcome$`HDI_rank`, levels = c(1,2,3), labels = c("One", "Two", "Three"))


#--------------test train split------------------

#Cluster data
x_cluster_train = c_data[trainIndex, ]
x_cluster_test = c_data[-trainIndex, ]

y_cluster_train = c_data_outcome[trainIndex, ]
y_cluster_test = c_data_outcome[-trainIndex, ]

#-----------Report metrics summary----------------

showMetrics=function(model, x, y){
  handlem<-function(model){
    tryCatch(predict(model, newdata=x, type="class"), error = function(e) model)
  }
  pred=handlem(model)
  
  conf=table(pred,y$`HDI_rank`)
  cat("\nConfusion matrix:")
  print(conf)
  
  accuracy=sum(diag(conf))/sum(conf)
  cat("\nAccuracy", round(accuracy,3) * 100, sep = ": ")
  
  bAccuracy=bal_accuracy_vec(y$`HDI_rank`,pred,estimator="macro")
  cat("\n\nBalanced accuracy", round(bAccuracy,3)*100, sep = ": ")
  
  diag = diag(conf)
  rowsums = apply(conf, 1, sum)
  colsums = apply(conf, 2, sum)
  
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  
  cat("\n\n")
  print(data.frame(precision, recall, f1))
}


data <- list(x_train, x_test, y_train, y_test, x_cluster_train, x_cluster_test, y_cluster_train, y_cluster_test)


runClassification=function(data){
  
  #Dataset Data
  x_train=data.frame(data[[1]])
  x_test=data.frame(data[[2]])
  
  y_train=data.frame(data[[3]])
  y_test=data.frame(data[[4]])
  
  #Dataset with clusters
  x_cluster_train = data.frame(data[[5]])
  x_cluster_test = data.frame(data[[6]])
  
  y_cluster_train = data.frame(data[[7]])
  y_cluster_test = data.frame(data[[8]])
  
  #--------Random Forest--------
  
  #--------classification using cluster output--------
  
  #verify if random forest is adequate with cross-validation
  
  Fit.RF <- rfcv(x_cluster_train,
                 y_cluster_train$HDI_rank, 
                 ntrees=200,
                 cv.fold=5) 
  
  with(Fit.RF, plot(n.var, error.cv, type="b", col="red")) 
  
  # error close to 0 -> optimal algorithm
  
  
  model<-randomForest(y_cluster_train$HDI_rank ~., 
                      data=x_cluster_train,
                      ntrees=200)
  
  # verify for test set
  rf.pred=predict(model, x_cluster_test, type="class")
  predMatrix = with(x_cluster_test, table(rf.pred, y_cluster_test$HDI_rank))
  
  showMetrics(rf.pred, x_cluster_test, y_cluster_test)
  importance(model)
  
  
  #----------classification using wdi data--------------------------
  
  #verify if random forest is adequate with cross-validation
  
  Fit.RF <- rfcv(x_train,
                 y_train$HDI_rank, 
                 ntrees=200,
                 cv.fold=5) 
  
  with(Fit.RF, plot(n.var, error.cv, type="b", col="red")) 
  
  # error does not tend to 0 (tends to 0.50), 
  # probably not an optimal algorithm or dataset not otimized
  
  
  model<-randomForest(y_train$HDI_rank ~., 
                      data=x_train,
                      ntrees=200)
  
  # verify for test set
  rf.pred=predict(model, x_test, type="class")
  predMatrix = with(x_test, table(rf.pred, y_test$HDI_rank))
  
  showMetrics(rf.pred, x_test, y_test)
  importance(model)
  
  
  #----------Naive Bayes----------
  
  #--------classification using cluster output--------
  
  NBclassifier <- train(x_cluster_train, 
                        y_cluster_train$HDI_rank, 
                        method = "nb", 
                        trControl = trainControl(method = "cv", number = 5))
  
  pred <- predict(NBclassifier, newdata=x_cluster_test, type="raw")
  
  showMetrics(pred, x_cluster_test, y_cluster_test)
  
  
  
  #----------classification using wdi data--------------------------
  
  NBclassifier <- train(x_train, 
                        y_train$HDI_rank, 
                        method = "nb", 
                        trControl = trainControl(method = "cv", number = 5))
  
  pred <- predict(NBclassifier, newdata=x_test, type="raw")
  
  showMetrics(pred, x_test, y_test)
}

runClassification(data)