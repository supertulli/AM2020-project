library(readr)
library(dplyr)
library(psych)
library(tidyverse)
WDI <- read_csv("Documents/MECD/SEM01/AM/Proj/AM2020-project/data/WDI_shortnames.csv")
head(WDI)

data <- WDI[,-c(71,72)] # removing the outcome variables
head(data)

data <- data[, order(names(data))]
colnames(data)

outcome <- WDI[,c(71,72)]

colnames(outcome) <- c('HDI_var', 'HDI_rank')
outcome$`HDI_rank` <- as.factor(outcome$`HDI_rank`)
summary(outcome)
sapply(outcome[,1], sd)
describe(outcome)

# Getting subsets of the data by theme:
economical <- data %>% select((starts_with('eco') & -ends_with('var'))) # economical instant values
summary(economical)

economical.var <- data %>% select((starts_with('eco') & ends_with('var'))) #economical variation values
summary(economical.var)

demographic <- data %>% select((starts_with('dem') & -ends_with('var'))) #demographic instant values
summary(demographic)

demographic.var <- data %>% select((starts_with('dem') & ends_with('var'))) #demographic variation values
summary(demographic.var)

education_science <- data %>% select((starts_with('sci') & -ends_with('var'))) #education and science instant values
summary(education_science)

education_science.var <- data %>% select((starts_with('sci') & ends_with('var'))) #education and science variation values
summary(education_science.var)

geographic <- data %>% select((starts_with('geo') & -ends_with('var'))) #geographic instant values
summary(geographic)

geographic.var <- data %>% select((starts_with('geo') & ends_with('var'))) #geographic variation values
summary(geographic.var)

health_sanitation <-data %>% select((starts_with('hs') & -ends_with('var'))) #health and sanitation instant values
summary(health_sanitation)

health_sanitation.var <- data %>% select((starts_with('hs') & ends_with('var'))) #health and sanitation variation values
summary(health_sanitation.var)

# histograms and correlations per group
library(psych)
# for economical instant values. GDP EXPORTS and IMPORTS are in a different scale so are grouped separately
pairs.panels(economical[,-c(4,6,7)], smooth = FALSE, ellipses = FALSE)
pairs.panels(economical[,c(4,6,7)], smooth = F, ellipses = F)

boxplot(economical[,-c(4,6,7)])
boxplot(economical[,c(4,6,7)], log="y") # make notice of log scale in y axes

# for economical variation values. GDP EXPORTS and IMPORTS are in a different scale so are grouped separately
pairs.panels(economical.var[,-c(4,6,7)], smooth = FALSE, ellipses = FALSE)
pairs.panels(economical.var[,c(4,6,7)], smooth = F, ellipses = F)
boxplot(economical.var[,-c(4,6,7)])
boxplot(economical.var[,c(4,6,7)])

# for demographic values variables
pairs.panels(demographic, smooth = F, ellipses = F)
boxplot(demographic)
pairs.panels(demographic.var, smooth = F, ellipses = F)
boxplot(demographic.var)

# for education and science variables
pairs.panels(education_science, smooth = F, ellipses = F)
boxplot(education_science[,1])
boxplot(education_science[,-1])
pairs.panels(education_science.var, smooth = F, ellipses = F)
boxplot(education_science.var)
boxplot(education_science.var[,-1])

# Some variables that are interesting to see by outcome:
sci_out <- bind_cols(education_science, outcome)
boxplot(sci.Internet ~ HDI_rank, data = sci_out)

# for geographic variables
pairs.panels(geographic, smooth = F, ellipses = F)
boxplot(geographic[,-c(3,4)])
boxplot(geographic[,c(3,4)])

geo_out <- bind_cols(geographic, outcome)
boxplot(geo.RuralPopGrowth ~ `HDI_rank`,data = geo_out)

pairs.panels(geographic.var, smooth = F, ellipses = F)
boxplot(geographic.var)

geo.var_out <- bind_cols(geographic.var, outcome)
boxplot(geo.ArableLand.var ~ `HDI_rank`,data = geo.var_out)

# for health and sanitation variables
pairs.panels(health_sanitation, smooth = F, ellipses = F)
boxplot(health_sanitation)
boxplot(health_sanitation[,-3])

pairs.panels(health_sanitation.var, smooth = F, ellipses = F)
boxplot(health_sanitation.var)
boxplot(health_sanitation.var[,-3])

hs.var_out <- bind_cols(health_sanitation.var, outcome)
par(mfrow=c(1,3),cex = 0.7)
boxplot(hs.BasicSanitation.var ~ `HDI_rank`,data = hs.var_out)
boxplot(hs.DrinkingWater.var ~ `HDI_rank`,data = hs.var_out)
boxplot(hs.OpenDefecation.var ~ `HDI_rank`,data = hs.var_out)


# par(mfrow=c(3,3),cex = 0.5)


# mRMR feature selection:

dd <- mRMR.data(data = data[,-1])
mrmr <- mRMR.classic(data = dd, target_indices = c(1), feature_count = 16)
feature_index <- mrmr@filters$'1'
colnames(data[,-1])[feature_index]

mrmr <- mRMR.ensemble(data = dd, target_indices = c(1), solution_count = 5, feature_count = 16)
feature_index <- mrmr@filters
feature_index$'1'[,1]
colnames(data[,-1])[feature_index]

#the same with full data set

WDI_full <- read_csv("/home/pedro/Documents/MECD/SEM01/AM/Proj/AM2020-project/data/WDI_full.csv")
head(WDI_full)

data_full <- WDI_full[,-c(369,370)]
dd_full <- mRMR.data(data_full[,-1])
mrmr <- mRMR.classic(data = dd_full, target_indices = c(1), feature_count = 30)
feature_index <- mrmr@filters$'1'
colnames(data_full[,-1])[feature_index]
