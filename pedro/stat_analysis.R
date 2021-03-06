library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(DataExplorer)

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
# correlation betwwen Urban and Rural population is -1 as to be expected
plot_correlation(geographic, title = "Geographic features correlations heatmap")
plot_boxplot(bind_cols(geographic, outcome[,2]), by='HDI_rank', title = "Geographic features by HDI_rank", ncol = 3, nrow = 2)
plot_qq(geographic, title = "Geographic features Q-Q plots")#, ncol = 4, nrow = 3)

geographic.var <- data %>% select((starts_with('geo') & ends_with('var'))) #geographic variation values
#summary(geographic.var)
describe(geographic)
pairs.panels(geographic, ellipses = F, smooth = F)
# correlation betwwen Urban and Rural population is -1 as to be expected
plot_correlation(geographic, title = "Geographic features correlations heatmap")
plot_boxplot(bind_cols(geographic, outcome[,2]), by='HDI_rank', title = "Geographic features by HDI_rank", ncol = 3, nrow = 2)
plot_qq(geographic, title = "Geographic features Q-Q plots")#, ncol = 4, nrow = 3)

##### Health and Sanitation
health_sanitation <-data %>% select((starts_with('hs') & -ends_with('var'))) #health and sanitation instant values
summary(health_sanitation)

health_sanitation.var <- data %>% select((starts_with('hs') & ends_with('var'))) #health and sanitation variation values
summary(health_sanitation.var)

# histograms and correlations per group

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

# mrmr <- mRMR.ensemble(data = dd, target_indices = c(1), solution_count = 5, feature_count = 16)
# feature_index <- mrmr@filters$'1'

selected_features <- colnames(data[,-1])[feature_index]
selected_features
data_selected <- data %>% select(selected_features)
describe(data_selected)
pairs.panels(data_selected, ellipses = F, smooth = F)
cor(data_selected)

data_out <- cbind(data_selected,outcome[,2])
describeBy(data_out[,-17]~HDI_rank)
boxplot(data_out$dem.MortalityInfant ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$dem.BirthRate.var ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$eco.CO2Emissions ~ data_out$HDI_rank, data = data_out)

introduce(data_out)
create_report(data_out)
plot_correlation(data_out[,1:16])
plot_boxplot(data_out, by='HDI_rank', nrow = 4, ncol = 4, )
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
