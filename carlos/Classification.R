setwd("C:/Users/carli/Documents/GitHub/AM2020-project")

library(randomForest)
library(caret)
library(klaR)
library(dplyr)
library(yardstick)


set.seed(42)

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

#-------------data load--------------------------

WDI <- read.csv("data/WDI.csv")

data_outcome <- WDI[,c(71,72)]

colnames(data_outcome) <- c('HDI_var', 'HDI_rank')

data_outcome$`HDI_rank` <- factor(data_outcome$`HDI_rank`, levels = c(0,1,2,3), labels = c("Negative", "Low", "Medium", "High"))


data <- read.csv("data/WDI_afterMRMR.csv")


cluster_data <- read.csv("data/WDI_CLUSTERING.csv")

c_data <- cluster_data[, -c(16)]

c_data_outcome <- cluster_data[,c(15,16)]

colnames(c_data_outcome) <- c('HDI_var', 'HDI_rank')

c_data_outcome$`HDI_rank` <- factor(c_data_outcome$`HDI_rank`, levels = c(1,2,3), labels = c("One", "Two", "Three"))



# get centroid



#--------Split the data into training and test set--------

trainIndex = createDataPartition(data_outcome$`HDI_rank`, p=0.8)$Resample1

x_train=data[trainIndex, ]
x_test=data[-trainIndex, ]

y_train=data_outcome[trainIndex, ]
y_test=data_outcome[-trainIndex, ]

x_cluster_train = c_data[trainIndex, ]
x_cluster_test = c_data[-trainIndex, ]

y_cluster_train = c_data_outcome[trainIndex, ]
y_cluster_test = c_data_outcome[-trainIndex, ]


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


# verify for training set -> expecting accuracy of 100%
rf.pred.training=predict(model,x_cluster_train,type="class")

showMetrics(rf.pred.training, x_cluster_train, y_cluster_train)

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

# verify for training set -> expecting accuracy of 100%
rf.pred.training=predict(model,x_train,type="class")

showMetrics(rf.pred.training, x_train, y_train)

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

