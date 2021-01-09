getwd()
setwd("/onedrive/OneDrive - Nokia/scicolab/ist/MA/2020/proj/GIT-AM2020-project/AM2020-project/data")

library(caret)
library(e1071)
library(tidyverse)
library(caret)
library(MASS)

set.seed(42)

#-------------data load--------------------------

WDI <- read_csv(".//WDI_shortnames.csv")
data <- WDI[,-c(1,71,72)] # removing the outcome variables
data <- data[, order(names(data))]
outcome <- WDI[,c(71,72)]
colnames(outcome) <- c('HDI_var', 'HDI_rank')
outcome$`HDI_rank` <- factor(outcome$`HDI_rank`, levels = c(0,1,2,3), labels = c("Negative", "Low", "Medium", "High"))

#--------------test train split------------------

trainIndex=createDataPartition(outcome$`HDI_rank`, p=0.7)$Resample1
y_train=outcome[trainIndex, ]
y_test=outcome[-trainIndex, ]
x_train=data[trainIndex, ]
x_test=data[-trainIndex, ]

#-----------Report metrics summary----------------

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

#---------Naive Bayes (baseline)-------------------------------

NBclassifier = naiveBayes(y_train$`HDI_rank` ~ ., data=x_train, laplace=0.1)
predictions = predict(NBclassifier, newdata=x_test, type="class")

confusionMatrix(predictions, y_test$HDI_rank)
showMetrics(NBclassifier)

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

#---------Standardization (convinient for LDA)------------------

preproc.param <- x_train %>% preProcess(method = c("center", "scale"))
x_train.scaled <- preproc.param %>% predict(x_train)
x_test.scaled <- preproc.param %>% predict(x_test)

#-----------------------LDA-------------------------------------

LDAclassifier <- lda(y_train$`HDI_rank`~., data = x_train.scaled)
predictions = predict(LDAclassifier, newdata=x_test.scaled)

confusionMatrix(predictions$class, y_test$HDI_rank)
showMetrics(predictions$class)

lda.data <- cbind(x_train.scaled, predict(LDAclassifier)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = y_train$`HDI_rank`))
