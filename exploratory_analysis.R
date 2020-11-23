#import libraries
library(plotly)

#read data
data <- read.csv("Development Index.csv", header = TRUE, sep = ",")

#view data
head(data)
summary(data)
View(data)
dim(data)
glimpse(data)

pairs.panels(data[,1:6], col=data[,7], smooth=FALSE, density=TRUE, 
             elipses=TRUE, digits=2, hist.col='green', cor= TRUE)

boxplot(Literacy....~Development.Index,data, 
        main = "Literacy vs Development Index",
        xlab = "Development index",
        ylab = "Literacy",
        col = "green",
        border = "black")

boxplot(Infant.mortality~Development.Index,data, 
        main = "Infant Mortality vs Development Index",
        xlab = "Development index",
        ylab = "Infant Mortality",
        col = "green",
        border = "black")
boxplot(GDP....per.capita.~Development.Index,data, 
        main = "GDP vs Development Index",
        xlab = "Development index",
        ylab = "GDP",
        col = "green",
        border = "black")

