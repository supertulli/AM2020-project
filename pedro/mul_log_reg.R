library(readr)
library(tidyverse)
WDI <- read_csv("Documents/MECD/SEM01/AM/Proj/AM2020-project/data/WDI_shortnames.csv")
View(WDI)

data <- WDI[,-c(71,72)] # removing the outcome variables
View(data)

data <- data[, order(names(data))]
colnames(data)

outcome <- WDI[,c(71,72)]

colnames(outcome) <- c('HDI_var', 'HDI_rank')

outcome$`HDI_rank` <- as.factor(outcome$`HDI_rank`)

data %>% select((starts_with('eco') & -ends_with('var')))




library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

set.seed(1234)

# Creating a binary categorization of HDI
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

