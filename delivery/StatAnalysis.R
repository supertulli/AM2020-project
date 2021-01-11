library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(DataExplorer)
library(mRMRe)

options(digits=3)

WDI <- read_csv(".//WDI.csv")
head(WDI)

data <- WDI[,-c(71,72)] # removing the outcome variables
head(data)

data <- data[, order(names(data))]
colnames(data)

outcome <- WDI[,c(71,72)]
colnames(outcome) <- c('HDI_var', 'HDI_rank')
outcome$`HDI_rank` <- factor(outcome$`HDI_rank`, ordered = T, levels = c(0,1,2,3))
head(outcome)
summary(outcome)
sapply(outcome[,1], sd)
describe(outcome)

#######################################
# Getting subsets of the data by theme:
##### Economical
economical <- data %>% select((starts_with('eco') & -ends_with('var'))) # economical instant values
#summary(economical)
describe(economical)
# plot pairs
pairs.panels(economical, ellipses = F, smooth = F)
#plot correlation heatmap
plot_correlation(economical, title = "Economical features correlations heatmap")
# boxplot
plot_boxplot(bind_cols(economical,outcome[,2]), by='HDI_rank', title = "Economical features by HDI_rank")
# boxplot of eco.AgeDependencyRate, eco.CleanCook, eco.FoodProdIdx, and eco.MerchTrade
plot_boxplot(bind_cols(economical[,-c(3:4,6:8)],outcome[,2]), by='HDI_rank')
# boxplot of the remaining variables
plot_boxplot(bind_cols(economical[,c(3:4,6:8)],outcome[,2]), by='HDI_rank')
# qq plot
plot_qq(economical, title = "Economical features Q-Q plots")

economical.var <- data %>% select((starts_with('eco') & ends_with('var'))) #economical variation values
#summary(economical.var)
describe(economical.var)
# plot pairs
pairs.panels(economical.var, ellipses = F, smooth = F)
# plot correlation heatmap
plot_correlation(economical.var, title = "Economical variation features correlations heatmap")
# box plot
plot_boxplot(bind_cols(economical.var,outcome[,2]), by='HDI_rank', title = "Economical variation features by HDI_rank")
# qq plot
plot_qq(economical.var, title = "Economical variation features Q-Q plots")

##### Demographic
demographic <- data %>% select((starts_with('dem') & -ends_with('var'))) #demographic instant values
#summary(demographic)
describe(demographic)
# plot pairs
pairs.panels(demographic, ellipses = F, smooth = F)
# plot correlation heatmap
plot_correlation(demographic, title = "Demographic features correlations heatmap")
# box plot
plot_boxplot(bind_cols(demographic,outcome[,2]), by='HDI_rank', title = "Demographic features by HDI_rank")
# qq plot
plot_qq(demographic, title = "Demographic features Q-Q plots")

demographic.var <- data %>% select((starts_with('dem') & ends_with('var'))) #demographic variation values
#summary(demographic.var)
describe(demographic.var)
# plot pairs
pairs.panels(demographic.var, ellipses = F, smooth = F)
# plot correlation heatmap
plot_correlation(demographic.var, title = "Demographic variation features correlations heatmap")
# box plot
plot_boxplot(bind_cols(demographic.var,outcome[,2]), by='HDI_rank', title = "Demographic variation features by HDI_rank")
# qq plot
plot_qq(demographic.var, title = "Demographic features Q-Q plots", ncol = 4, nrow = 3)

##### Education_Science
education_science <- data %>% select((starts_with('sci') & -ends_with('var'))) #education and science instant values
# summary(education_science)
describe(education_science)
# plot pairs
pairs.panels(education_science, ellipses = F, smooth = F)
# plot correlation heatmap
plot_correlation(education_science, title = "Education and Science features correlations heatmap")
# box plot
plot_boxplot(bind_cols(education_science,outcome[,2]), by='HDI_rank', ncol = 3, nrow = 2 ,title = "Education and Science features by HDI_rank")
# qq plot
plot_qq(education_science, title = "Education and Science features Q-Q plots")#, ncol = 4, nrow = 3)

education_science.var <- data %>% select((starts_with('sci') & ends_with('var'))) #education and science variation values
#summary(education_science.var)
# plot pairs
pairs.panels(education_science.var, ellipses = F, smooth = F)
# plot correlation heatmap
plot_correlation(education_science.var, title = "Education and Science variation features correlations heatmap")
# box plot
plot_boxplot(bind_cols(education_science.var,outcome[,2]), by='HDI_rank', ncol = 3, nrow = 2 ,title = "Education and Science variation features by HDI_rank")
# qq plot
plot_qq(education_science.var, title = "Education and Science variation features Q-Q plots")#, ncol = 4, nrow = 3)

##### Geographic
geographic <- data %>% select((starts_with('geo') & -ends_with('var'))) #geographic instant values
#summary(geographic)
describe(geographic)
# plot pairs
pairs.panels(geographic, ellipses = F, smooth = F)

# correlation between Urban and Rural population is -1 as to be expected
# plot correlation heatmap
plot_correlation(geographic, title = "Geographic features correlations heatmap")
# box plot
plot_boxplot(bind_cols(geographic, outcome[,2]), by='HDI_rank', title = "Geographic features by HDI_rank", ncol = 3, nrow = 2)
# qq plot
plot_qq(geographic, title = "Geographic features Q-Q plots")#, ncol = 4, nrow = 3)

geographic.var <- data %>% select((starts_with('geo') & ends_with('var'))) #geographic variation values
#summary(geographic.var)
describe(geographic.var)
# plot pairs
pairs.panels(geographic.var, ellipses = F, smooth = F)
# correlation between Urban and Rural population variations is also -1 as to be expected
# plot correlation heatmap
plot_correlation(geographic.var, title = "Geographic variation features correlations heatmap")
# box plot
plot_boxplot(bind_cols(geographic.var, outcome[,2]), by='HDI_rank', title = "Geographic variation features by HDI_rank", ncol = 1, nrow = 3)
# qq plot
plot_qq(geographic.var, title = "Geographic variation features Q-Q plots")#, ncol = 4, nrow = 3)

##### Health and Sanitation
health_sanitation <-data %>% select((starts_with('hs') & -ends_with('var'))) #health and sanitation instant values
#summary(health_sanitation)
describe(health_sanitation)
# plot pairs
pairs.panels(health_sanitation, ellipses = F, smooth = F)
# plot correlation heatmap
plot_correlation(health_sanitation, title = "Health and sanitation features correlations heatmap")
# box plot
plot_boxplot(bind_cols(health_sanitation, outcome[,2]), by='HDI_rank', title = "Health and sanitation features by HDI_rank", ncol = 2, nrow = 2)
# qq plot
plot_qq(health_sanitation, title = "Health and sanitation features Q-Q plots", ncol = 2, nrow = 2)

health_sanitation.var <- data %>% select((starts_with('hs') & ends_with('var'))) #health and sanitation variation values
# summary(health_sanitation.var)
describe(health_sanitation.var)
# plot pairs
pairs.panels(health_sanitation.var, ellipses = F, smooth = F)
# plot correlation heatmap
plot_correlation(health_sanitation.var, title = "Health and sanitation variation features correlations heatmap")
# box plot
plot_boxplot(bind_cols(health_sanitation.var, outcome[,2]), by='HDI_rank', title = "Health and sanitation variation features by HDI_rank", ncol = 2, nrow = 2)
# qq plot
plot_qq(health_sanitation.var, title = "Health and sanitation variation features Q-Q plots", ncol = 2, nrow = 2)


# mRMR feature selection:
rank <- factor(as.character(outcome[[2]]), ordered = TRUE, levels = c("0", "1", "2", "3"))
mR_data <- bind_cols(rank, data[,-1])
# mR_data[,1] <- as.factor(mR_data[,1])
# mR_data[,1] <- ordered(mR_data[,1], levels = c(0, 1, 2, 3))

head(mR_data)
dd <- mRMR.data(data = mR_data[,-1], strata = rank)
mrmr_selector <- mRMR.classic(data = dd, target_indices = c(1), feature_count = 16)
feature_index <- mrmr_selector@filters$'1'
selected_features <- colnames(data[,-1])[feature_index]
selected_features

# ensemble would allow for union set of features. We'll stick to classic... 
# colnames(data[,-1])[feature_index]
# mrmr <- mRMR.ensemble(data = dd, target_indices = c(1), solution_count =1, feature_count = 16)
# feature_index <- mrmr@filters$'1'

data_selected <- data %>% select(all_of(selected_features))

describe(data_selected)
# plot pairs
pairs.panels(data_selected, ellipses = F, smooth = F)
cor(data_selected)

data_selected <- bind_cols(WDI[,1],data_selected)

data_out <- cbind(data_selected,outcome[,2])

describeBy(data_out[,-17] ~HDI_rank)
# box plots
boxplot(data_out$dem.MortalityInfant ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$dem.BirthRate.var ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$hs.DrinkingWater ~ data_out$HDI_rank, data = data_out)

#introduce(data_out)
#create_report(data_out)
# plot correlation heatmap
plot_correlation(data_out[,1:16], title = "mRMR 16 features correlation heatmap")
# box plot
plot_boxplot(data_out, by='HDI_rank', nrow = 4, ncol = 4)
# qq plot
plot_qq(data_out[,1:16], title = "mRMR 16 features QQ-plot", nrow = 4, ncol = 4)
# histogram
plot_histogram(data_out)
# density estimates
plot_density(data_out)
# plot PCA loadings with a 0.9 cap on the explained variance
plot_prcomp(data_out[,-17], variance_cap = 0.90)

create_report(data_out)

#pinpoint extreme values:
data_selected[data_selected$sci.EduExpense>20,]
data_selected[data_selected$dem.MortalityUnder5.var<(-50),]

#output to csv file
data_to_export <- data_out[, -which(names(data_out) %in% c("dem.Pop0to14","dem.BirthRate"))] 
write.csv(data_to_export, file = "mRMR_reduced_data.csv", row.names = FALSE)


#######################################
#the same with full data set

WDI_full <- read_csv(".//WDI_full.csv")
head(WDI_full)

outcome_full <- WDI_full[,c(369,370)]
colnames(outcome_full) <- c('HDI_var', 'HDI_rank')
outcome_full$`HDI_rank` <- as.factor(outcome_full$`HDI_rank`)

# WDI_full[,c(369,370)]
data_full <- WDI_full[,-c(369,370)]
rank_full <- factor(as.character(outcome_full[[2]]), ordered = TRUE, levels = c("0", "1", "2", "3"))
dd_full <- mRMR.data(data = data_full[,-1], strata = rank_full)
mrmr_full <- mRMR.classic(data = dd_full, target_indices = c(1), feature_count = 16)
feature_index <- mrmr_full@filters$'1'
selected_features <- colnames(data_full[,-1])[feature_index]
selected_features
data_selected <- data_full %>% select(selected_features)
describe(data_selected)
pairs.panels(data_selected, ellipses = F, smooth = F) # plot pairs
cor(data_selected)

data_out_full <- cbind(data_selected,outcome_full[,2])
describeBy(data_out_full[,-17]~HDI_rank)
boxplot(data_out_full$`Fixed telephone subscriptions - Delta`~data_out_full$HDI_rank, data = data_out_full)

# a lot of selected features are demographic and highly correlated, not really meaningful or interesting...