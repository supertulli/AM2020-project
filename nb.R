library(caret)
library(e1071)
set.seed(42)

d <- read.csv("..//data//WDI.csv")
d <- d[ , !(names(d) %in% rev(names(d))[2])]
d$cat_HDI...Delta <- factor(d$cat_HDI...Delta, levels = c(0,1,2,3), labels = c("Negative", "Low", "Medium", "High"))

trainIndex=createDataPartition(d$cat_HDI...Delta, p=0.7)$Resample1
train=d[trainIndex, -c(1),drop=FALSE]
test=d[-trainIndex, -c(1),drop=FALSE]

NBclassifier=naiveBayes(cat_HDI...Delta ~ ., data=train, laplace=1)

showMetrics=function(model){
  pred=predict(model, newdata=test, type="class")
  conf=table(test$cat_HDI...Delta, pred)
  message("Confusion matrix")
  print(conf)
  
  accuracy=(conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4])/sum(conf)
  message("Accuracy")
  print(round(accuracy,3))
  
  bAccuracy=(conf[1,1]/sum(conf[,1])+conf[2,2]/sum(conf[,2])+conf[3,3]/sum(conf[,3])+conf[4,4]/sum(conf[,4]))/4
  message("Balanced accuracy")
  print(round(bAccuracy,3))
}

showMetrics(NBclassifier)