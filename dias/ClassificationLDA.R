getwd()
setwd("/onedrive/OneDrive - Nokia/scicolab/ist/MA/2020/proj/GIT-AM2020-project/AM2020-project/data")

library(caret)
library(e1071)
library(tidyverse)
library(MASS)
library(klaR)
library(yardstick)

set.seed(42)

#-------------data load--------------------------

WDI <- read_csv(".//WDI_shortnames.csv")
data <- WDI[,-c(1,71,72)] # removing the outcome variables
data <- data[, order(names(data))]
outcome <- WDI[,c(71,72)]
colnames(outcome) <- c('HDI_var', 'HDI_rank')
outcome$HDI_rank <- factor(outcome$`HDI_rank`, levels = c(0,1,2,3), labels = c("Negative", "Low", "Medium", "High"))

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
model<- train(y=y_train$HDI_rank, x=x_train.scaled, trControl=train_control, prior = c(0.07,0.31,0.31,0.31), method="lda")
showMetrics(model$pred[,1],model$pred[,2])

#-----------------------QDA-------------------------------------

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(y=y_train$HDI_rank, x=x_train.scaled, trControl=train_control, prior = c(0.07,0.31,0.31,0.31), method="qda")
showMetrics(model$pred[,1],model$pred[,2])

#-----------------------KNN-------------------------------------

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(y=y_train$HDI_rank, x=x_train.scaled, trControl=train_control, method="knn", tuneLength = 20)
model$results
# The final value used for the model was k = 15.
showMetrics(model$pred[,1],model$pred[,2])

#-----------------------final-------------------------------------
# LDA outperformed others

LDAclassifier <- lda(x=x_train.scaled, grouping=y_train$HDI_rank, prior = c(0.07,0.31,0.31,0.31), CV=FALSE)
predictions <- predict(LDAclassifier, newdata=x_test.scaled)
confmatrix<-confusionMatrix(predictions$class, y_test$HDI_rank)
showMetrics(predictions$class, y_test$HDI_rank)
lda.data <- cbind(x_train.scaled, predict(LDAclassifier)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = y_train$`HDI_rank`))


