library(readr)
WDI <- read_csv("Documents/MECD/SEM01/AM/Proj/AM2020-project/data/WDI_shortnames.csv")
View(WDI)

colnames(WDI)

data <- WDI[,-74] # removing the continuous outcome
View(data)

colnames(data)

data$`cat_HDI - Delta` <- as.factor(data$`cat_HDI - Delta`)

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

set.seed(1234)

# Createing a binary categorization of HDI
data_bin <- subset(data, select = -74)
data_bin$'Sigma' <- as.factor(sapply(WDI$`HDI - Delta`,function(x) if(x<0) 0 else 1))
data_bin$Sigma



# Multinomial Logistic Regressing Classifier

relevel(data, ref = 'cat_HDI - Delta')

test <- multinom(formula =`cat_HDI - Delta`~ ., 
                 data = data,
                 family ='multinomial',
                 maxit =10000,
                 MaxNWts = 10000)

summary(test)

