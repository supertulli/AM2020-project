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
png(filename = "figures/eco_pairs.png", width = 800, height = 600)
pairs.panels(economical, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/eco_cor_heatmap.png", width = 800, height = 600)
plot_correlation(economical, title = "Economical features correlations heatmap")
dev.off()
png(filename = "figures/eco_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(economical,outcome[,2]), by='HDI_rank', title = "Economical features by HDI_rank")
dev.off()
png(filename = "figures/eco_box_a.png", width = 800, height = 600)
plot_boxplot(bind_cols(economical[,-c(3:4,6:8)],outcome[,2]), by='HDI_rank')
dev.off()
png(filename = "figures/eco_box_b.png", width = 800, height = 600)
plot_boxplot(bind_cols(economical[,c(3:4,6:8)],outcome[,2]), by='HDI_rank')
dev.off()
png(filename = "figures/eco_qq.png", width = 800, height = 600)
plot_qq(economical, title = "Economical features Q-Q plots")
dev.off()

economical.var <- data %>% select((starts_with('eco') & ends_with('var'))) #economical variation values
#summary(economical.var)
describe(economical.var)
png(filename = "figures/eco_var_pairs.png", width = 800, height = 600)
pairs.panels(economical.var, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/eco_var_cor_heatmap.png", width = 800, height = 600)
plot_correlation(economical.var, title = "Economical variation features correlations heatmap")
dev.off()
png(filename = "figures/eco_var_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(economical.var,outcome[,2]), by='HDI_rank', title = "Economical variation features by HDI_rank")
dev.off()
png(filename = "figures/eco_var_qq.png", width = 800, height = 600)
plot_qq(economical.var, title = "Economical variation features Q-Q plots")
dev.off()

##### Demographic
demographic <- data %>% select((starts_with('dem') & -ends_with('var'))) #demographic instant values
#summary(demographic)
describe(demographic)
png(filename = "figures/dem_pairs.png", width = 800, height = 600)
pairs.panels(demographic, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/dem_cor_heatmap.png", width = 800, height = 600)
plot_correlation(demographic, title = "Demographic features correlations heatmap")
dev.off()
png(filename = "figures/dem_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(demographic,outcome[,2]), by='HDI_rank', title = "Demographic features by HDI_rank")
dev.off()
png(filename = "figures/dem_qq.png", width = 800, height = 600)
plot_qq(demographic, title = "Demographic features Q-Q plots")
dev.off()

demographic.var <- data %>% select((starts_with('dem') & ends_with('var'))) #demographic variation values
#summary(demographic.var)
describe(demographic.var)
png(filename = "figures/dem_var_pairs.png", width = 800, height = 600)
pairs.panels(demographic.var, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/dem_var_cor_heatmap.png", width = 800, height = 600)
plot_correlation(demographic.var, title = "Demographic variation features correlations heatmap")
dev.off()
png(filename = "figures/dem_var_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(demographic.var,outcome[,2]), by='HDI_rank', title = "Demographic variation features by HDI_rank")
dev.off()
png(filename = "figures/dem_var_qq.png", width = 800, height = 600)
plot_qq(demographic.var, title = "Demographic features Q-Q plots", ncol = 4, nrow = 3)
dev.off()
##### Education_Science
education_science <- data %>% select((starts_with('sci') & -ends_with('var'))) #education and science instant values
# summary(education_science)
describe(education_science)
png(filename = "figures/sci_pairs.png", width = 800, height = 600)
pairs.panels(education_science, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/sci_cor_heatmap.png", width = 800, height = 600)
plot_correlation(education_science, title = "Education and Science features correlations heatmap")
dev.off()
png(filename = "figures/sci_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(education_science,outcome[,2]), by='HDI_rank', ncol = 3, nrow = 2 ,title = "Education and Science features by HDI_rank")
dev.off()
png(filename = "figures/sci_qq.png", width = 800, height = 600)
plot_qq(education_science, title = "Education and Science features Q-Q plots")#, ncol = 4, nrow = 3)
dev.off()

education_science.var <- data %>% select((starts_with('sci') & ends_with('var'))) #education and science variation values
#summary(education_science.var)
png(filename = "figures/sci_var_pairs.png", width = 800, height = 600)
pairs.panels(education_science.var, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/sci_var_cor_heatmap.png", width = 800, height = 600)
plot_correlation(education_science.var, title = "Education and Science variation features correlations heatmap")
dev.off()
png(filename = "figures/sci_var_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(education_science.var,outcome[,2]), by='HDI_rank', ncol = 3, nrow = 2 ,title = "Education and Science variation features by HDI_rank")
dev.off()
png(filename = "figures/sci_var_qq.png", width = 800, height = 600)
plot_qq(education_science.var, title = "Education and Science variation features Q-Q plots")#, ncol = 4, nrow = 3)
dev.off()

##### Geographic
geographic <- data %>% select((starts_with('geo') & -ends_with('var'))) #geographic instant values
#summary(geographic)
describe(geographic)
png(filename = "figures/geo_pairs.png", width = 800, height = 600)
pairs.panels(geographic, ellipses = F, smooth = F)
dev.off()
# correlation between Urban and Rural population is -1 as to be expected
png(filename = "figures/geo_cor_heatmap.png", width = 800, height = 600)
plot_correlation(geographic, title = "Geographic features correlations heatmap")
dev.off()
png(filename = "figures/geo_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(geographic, outcome[,2]), by='HDI_rank', title = "Geographic features by HDI_rank", ncol = 3, nrow = 2)
dev.off()
png(filename = "figures/geo_qq.png", width = 800, height = 600)
plot_qq(geographic, title = "Geographic features Q-Q plots")#, ncol = 4, nrow = 3)
dev.off()

geographic.var <- data %>% select((starts_with('geo') & ends_with('var'))) #geographic variation values
#summary(geographic.var)
describe(geographic.var)
png(filename = "figures/geo_var_pairs.png", width = 800, height = 600)
pairs.panels(geographic.var, ellipses = F, smooth = F)
dev.off()
# correlation between Urban and Rural population variations is also -1 as to be expected
png(filename = "figures/geo_var_cor_heatmap.png", width = 800, height = 600)
plot_correlation(geographic.var, title = "Geographic variation features correlations heatmap")
dev.off()
png(filename = "figures/geo_var_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(geographic.var, outcome[,2]), by='HDI_rank', title = "Geographic variation features by HDI_rank", ncol = 1, nrow = 3)
dev.off()
png(filename = "figures/geo_var_qq.png", width = 800, height = 600)
plot_qq(geographic.var, title = "Geographic variation features Q-Q plots")#, ncol = 4, nrow = 3)
dev.off()

##### Health and Sanitation
health_sanitation <-data %>% select((starts_with('hs') & -ends_with('var'))) #health and sanitation instant values
#summary(health_sanitation)
describe(health_sanitation)
png(filename = "figures/hs_pairs.png", width = 800, height = 600)
pairs.panels(health_sanitation, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/hs_cor_heatmap.png", width = 800, height = 600)
plot_correlation(health_sanitation, title = "Health and sanitation features correlations heatmap")
dev.off()
png(filename = "figures/hs_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(health_sanitation, outcome[,2]), by='HDI_rank', title = "Health and sanitation features by HDI_rank", ncol = 2, nrow = 2)
dev.off()
png(filename = "figures/hs_qq.png", width = 800, height = 600)
plot_qq(health_sanitation, title = "Health and sanitation features Q-Q plots", ncol = 2, nrow = 2)
dev.off()

health_sanitation.var <- data %>% select((starts_with('hs') & ends_with('var'))) #health and sanitation variation values
# summary(health_sanitation.var)
describe(health_sanitation.var)
png(filename = "figures/hs_var_pairs.png", width = 800, height = 600)
pairs.panels(health_sanitation.var, ellipses = F, smooth = F)
dev.off()
png(filename = "figures/hs_var_cor_heatmap.png", width = 800, height = 600)
plot_correlation(health_sanitation.var, title = "Health and sanitation variation features correlations heatmap")
dev.off()
png(filename = "figures/hs_var_box.png", width = 800, height = 600)
plot_boxplot(bind_cols(health_sanitation.var, outcome[,2]), by='HDI_rank', title = "Health and sanitation variation features by HDI_rank", ncol = 2, nrow = 2)
dev.off()
png(filename = "figures/hs_var_qq.png", width = 800, height = 600)
plot_qq(health_sanitation.var, title = "Health and sanitation variation features Q-Q plots", ncol = 2, nrow = 2)
dev.off()

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
selected_features # remove a NA feature selection...

# ensemble would allow for union set of features. We'll stick to classic... 
# colnames(data[,-1])[feature_index]
# mrmr <- mRMR.ensemble(data = dd, target_indices = c(1), solution_count =1, feature_count = 16)
# feature_index <- mrmr@filters$'1'

data_selected <- data %>% select(all_of(selected_features))
describe(data_selected)
png(filename = "figures/mRMR_16_pairs.png", width = 800, height = 600)
pairs.panels(data_selected, ellipses = F, smooth = F)
dev.off()
cor(data_selected)

data_out <- cbind(data_selected,outcome[,2])

describeBy(data_out[,-17] ~HDI_rank)
boxplot(data_out$dem.MortalityInfant ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$dem.BirthRate.var ~ data_out$HDI_rank, data = data_out)
boxplot(data_out$hs.DrinkingWater ~ data_out$HDI_rank, data = data_out)

#introduce(data_out)
#create_report(data_out)
png(filename = "figures/mRMR_16_cor_heatmap.png", width = 800, height = 600)
plot_correlation(data_out[,1:16], title = "mRMR 16 features correlation heatmap")
dev.off()
png(filename = "figures/mRMR_16_box.png", width = 800, height = 600)
plot_boxplot(data_out, by='HDI_rank', nrow = 4, ncol = 4)
dev.off()
png(filename = "figures/mRMR_16_qq.png", width = 800, height = 600)
plot_qq(data_out[,1:16], title = "mRMR 16 features QQ-plot", nrow = 4, ncol = 4)
dev.off()
png(filename = "figures/mRMR_16_histogram.png", width = 800, height = 600)
plot_histogram(data_out)
dev.off()
png(filename = "figures/mRMR_16_density.png", width = 800, height = 600)
plot_density(data_out)
dev.off()
png(filename = "figures/mRMR_16_prcomp.png", width = 800, height = 600)
plot_prcomp(data_out[,-17], variance_cap = 0.90)
dev.off()

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

# a lot of selected features are demographic and highly correlated, not really meaningful or interesting...