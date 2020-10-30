library(plotly)
library(dplyr)
library(hablar)

# read the data into memory

# data <- read.csv("./data/Absenteeism_at_work.csv", header = TRUE, sep = ";")
data <- read.csv("./data/Development Index.csv", header = TRUE, sep = ",")
 
head(data)
summary(data)
View(data)

dim(data)
glimpse(data)

# data$ID <- factor(data$ID)

# data$ID %>% convert(chr(ID))
# data %>% convert(fct(Reason.for.absence))

library(DataExplorer)
DataExplorer::create_report(data)

library((ggplot2))
ggplot(data = data)

library(plotly)
boxplot(data[,-7])
boxplot(Literacy....~Development.Index,data)
boxplot(Infant.mortality~Development.Index,data)
boxplot(GDP....per.capita.~Development.Index,data)

pairs(data)

library(psych)

pairs.panels(data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)
