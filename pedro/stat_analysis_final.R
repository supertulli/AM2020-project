library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(DataExplorer)
library(mRMRe)

options(digits=3)

WDI <- read_csv("data/WDI_shortnames.csv")
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

#######################################
# Getting subsets of the data by theme:
##### Economical
economical <- data %>% select((starts_with('eco') & -ends_with('var'))) # economical instant values
#summary(economical)
describe(economical)
pairs.panels(economical, ellipses = F, smooth = F)
plot_correlation(economical, title = "Economical features correlations heatmap")
plot_boxplot(bind_cols(economical,outcome[,2]), by='HDI_rank', title = "Economical features by HDI_rank")
plot_boxplot(bind_cols(economical[,-c(3:4,6:8)],outcome[,2]), by='HDI_rank')
plot_boxplot(bind_cols(economical[,c(3:4,6:8)],outcome[,2]), by='HDI_rank')
plot_qq(economical, title = "Economical features Q-Q plots")

economical.var <- data %>% select((starts_with('eco') & ends_with('var'))) #economical variation values
#summary(economical.var)
describe(economical.var)
pairs.panels(economical.var, ellipses = F, smooth = F)
plot_correlation(economical.var, title = "Economical variation features correlations heatmap")
plot_boxplot(bind_cols(economical.var,outcome[,2]), by='HDI_rank', title = "Economical variation features by HDI_rank")
plot_qq(economical.var, title = "Economical variation features Q-Q plots")

##### Demographic
demographic <- data %>% select((starts_with('dem') & -ends_with('var'))) #demographic instant values
#summary(demographic)
describe(demographic)
pairs.panels(demographic, ellipses = F, smooth = F)
plot_correlation(demographic, title = "Demographic features correlations heatmap")
plot_boxplot(bind_cols(demographic,outcome[,2]), by='HDI_rank', title = "Demographic features by HDI_rank")
plot_qq(demographic, title = "Demographic features Q-Q plots")

demographic.var <- data %>% select((starts_with('dem') & ends_with('var'))) #demographic variation values
#summary(demographic.var)
describe(demographic.var)
pairs.panels(demographic.var, ellipses = F, smooth = F)
plot_correlation(demographic.var, title = "Demographic variation features correlations heatmap")
plot_boxplot(bind_cols(demographic.var,outcome[,2]), by='HDI_rank', title = "Demographic variation features by HDI_rank")
plot_qq(demographic.var, title = "Demographic features Q-Q plots", ncol = 4, nrow = 3)

##### Education_Science
education_science <- data %>% select((starts_with('sci') & -ends_with('var'))) #education and science instant values
# summary(education_science)
describe(education_science)
pairs.panels(education_science, ellipses = F, smooth = F)
plot_correlation(education_science, title = "Education and Science features correlations heatmap")
plot_boxplot(bind_cols(education_science,outcome[,2]), by='HDI_rank', ncol = 3, nrow = 2 ,title = "Education and Science features by HDI_rank")
plot_qq(education_science, title = "Education and Science features Q-Q plots")#, ncol = 4, nrow = 3)

education_science.var <- data %>% select((starts_with('sci') & ends_with('var'))) #education and science variation values
#summary(education_science.var)
pairs.panels(education_science.var, ellipses = F, smooth = F)
plot_correlation(education_science.var, title = "Education and Science variation features correlations heatmap")
plot_boxplot(bind_cols(education_science.var,outcome[,2]), by='HDI_rank', ncol = 3, nrow = 2 ,title = "Education and Science variation features by HDI_rank")
plot_qq(education_science.var, title = "Education and Science variation features Q-Q plots")#, ncol = 4, nrow = 3)

##### Geographic
geographic <- data %>% select((starts_with('geo') & -ends_with('var'))) #geographic instant values
#summary(geographic)
describe(geographic)
pairs.panels(geographic, ellipses = F, smooth = F)
# correlation between Urban and Rural population is -1 as to be expected
plot_correlation(geographic, title = "Geographic features correlations heatmap")
plot_boxplot(bind_cols(geographic, outcome[,2]), by='HDI_rank', title = "Geographic features by HDI_rank", ncol = 3, nrow = 2)
plot_qq(geographic, title = "Geographic features Q-Q plots")#, ncol = 4, nrow = 3)

geographic.var <- data %>% select((starts_with('geo') & ends_with('var'))) #geographic variation values
#summary(geographic.var)
describe(geographic.var)
pairs.panels(geographic.var, ellipses = F, smooth = F)
# correlation between Urban and Rural population variations is also -1 as to be expected
plot_correlation(geographic.var, title = "Geographic variation features correlations heatmap")
plot_boxplot(bind_cols(geographic.var, outcome[,2]), by='HDI_rank', title = "Geographic variation features by HDI_rank", ncol = 1, nrow = 3)
plot_qq(geographic.var, title = "Geographic variation features Q-Q plots")#, ncol = 4, nrow = 3)

##### Health and Sanitation
health_sanitation <-data %>% select((starts_with('hs') & -ends_with('var'))) #health and sanitation instant values
#summary(health_sanitation)
describe(health_sanitation)
pairs.panels(health_sanitation, ellipses = F, smooth = F)
plot_correlation(health_sanitation, title = "Health and sanitation features correlations heatmap")
plot_boxplot(bind_cols(health_sanitation, outcome[,2]), by='HDI_rank', title = "Health and sanitation features by HDI_rank", ncol = 2, nrow = 2)
plot_qq(health_sanitation, title = "Health and sanitation features Q-Q plots", ncol = 2, nrow = 2)

health_sanitation.var <- data %>% select((starts_with('hs') & ends_with('var'))) #health and sanitation variation values
# summary(health_sanitation.var)
describe(health_sanitation.var)
pairs.panels(health_sanitation.var, ellipses = F, smooth = F)
plot_correlation(health_sanitation.var, title = "Health and sanitation variation features correlations heatmap")
plot_boxplot(bind_cols(health_sanitation.var, outcome[,2]), by='HDI_rank', title = "Health and sanitation variation features by HDI_rank", ncol = 2, nrow = 2)
plot_qq(health_sanitation.var, title = "Health and sanitation variation features Q-Q plots", ncol = 2, nrow = 2)

# mRMR feature selection:

dd <- mRMR.data(data = data[,-1])
mrmr <- mRMR.classic(data = dd, target_indices = c(1), feature_count = 16)
feature_index <- mrmr@filters$'1'
colnames(data[,-1])[feature_index]

mrmr <- mRMR.ensemble(data = dd, target_indices = c(1), solution_count =1, feature_count = 16)
feature_index <- mrmr@filters$'1'
selected_features <- colnames(data[,-1])[feature_index]
selected_features

# ensemble would allow for union set of features. We'll stick to classic... 

data_selected <- data %>% select(all_of(selected_features))
describe(data_selected)
pairs.panels(data_selected, ellipses = F, smooth = F)
cor(data_selected)

data_out <- cbind(data_selected,outcome[,2])
describeBy(data_out[,-17]~HDI_rank)
boxplot(data_out$dem.MortalityInfant ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$dem.BirthRate.var ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$eco.CO2Emissions ~ data_out$HDI_rank, data = data_out)

#introduce(data_out)
create_report(data_out)
plot_correlation(data_out[,1:16])
plot_boxplot(data_out, by='HDI_rank', nrow = 4, ncol = 4)
plot_qq(data_out[,1:16], nrow = 4, ncol = 4)
plot_histogram(data_out)
plot_density(data_out)
plot_prcomp(data_out[,-17], variance_cap = 0.90)

create_report(data_out)

#######################################
#the same with full data set

WDI_full <- read_csv("/home/pedro/Documents/MECD/SEM01/AM/Proj/AM2020-project/data/WDI_full.csv")
head(WDI_full)

outcome_full <- WDI_full[,c(369,370)]
colnames(outcome_full) <- c('HDI_var', 'HDI_rank')
outcome_full$`HDI_rank` <- as.factor(outcome_full$`HDI_rank`)

# WDI_full[,c(369,370)]
data_full <- WDI_full[,-c(369,370)]
dd_full <- mRMR.data(data_full[,-1])
mrmr <- mRMR.classic(data = dd_full, target_indices = c(1), feature_count = 16)
feature_index <- mrmr@filters$'1'
selected_features <- colnames(data_full[,-1])[feature_index]
selected_features
data_selected <- data_full %>% select(selected_features)
describe(data_selected)
pairs.panels(data_selected, ellipses = F, smooth = F)
cor(data_selected)

data_out_full <- cbind(data_selected,outcome_full[,2])
describeBy(data_out_full[,-17]~HDI_rank)
boxplot(data_out_full$`Fixed telephone subscriptions - Delta`~data_out_full$HDI_rank, data = data_out_full)
