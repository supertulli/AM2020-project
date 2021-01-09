library(ggplot2)
library(DataExplorer)
library(readr) # read_csv
# read and prepare data
WDI <- read_csv("../data/WDI_shortnames.csv")
#WDI_indicators<-WDI[,c(2:(length(WDI)-2))] # the last two columns are the HDI-delta and categorical HDI and the first is the country/year pair
HDI<-WDI[,c(length(WDI)-1, length(WDI))] # target variables
WDI_countryYear<-WDI[,1] # name of each sample (not relevant for the numerical analysis)

WDI_indicators<-data.frame(WDI$dem.BirthRate, 
                      WDI$dem.AdolescentFertRate.var, 
                      WDI$dem.MortalityUnder5, 
                      WDI$hs.GovHealthExpend,
                      WDI$hs.BasicSanitation, 
                      WDI$eco.AgeDependancyRate, 
                      WDI$dem.DeathRate.var, 
                      WDI$dem.LifeExpectancy, 
                      WDI$sci.EduExpense, 
                      WDI$eco.CO2Emissions, 
                      WDI$dem.MortalityUnder5.var, 
                      WDI$dem.PopGrowth,
                      WDI$hs.DrinkingWater,
                      WDI$dem.BirthRate.var)



#################################
# Principal Components Analysis #
#################################
#Classical PCA on the indicators centering and scaling
WDI_indicators.pca<-prcomp(WDI_indicators, center=TRUE, scale. = TRUE) 
summary(WDI_indicators.pca)

# # install.packages("devtools")
# library(devtools)
# # install_github("vqv/ggbiplot")
# library(ggbiplot)
# ggbiplot(WDI_indicators.pca)

# Proportion of variance explained by each principal component
variance_proportion <- WDI_indicators.pca$sdev^2/sum(WDI_indicators.pca$sdev^2) # compute the proportion of variance explained by each component
# plot
plot(variance_proportion, ylab = "Proportion of variance", xlab = "Principal Component Index", main = "Proportion of variance explained by each PC")
grid()

# Cumulative variance explained by each principal component
cumulative_variance <- cumsum(variance_proportion) # compute the cumulative variance explained by each component
# plot
plot(cumulative_variance, 
     ylab = "Cumulative Explained Variance", 
     xlab = "Principal Component Index", 
     main = "Cumulative variance explained by each PC");abline(0.8,0);grid()

# Choice of principal components
# 1.Choose *k_1* such that lambda_i >= lambda^bar (which is 1, since we are working with standardized variables) for i = 1:k_1
{k_1 = 0
for (value in WDI_indicators.pca$sdev){ if (value^2 >=  1) k_1 = k_1+1}
k_1} # show the value of k_1

# 2.Choose k_0.8 such that the first k_0.8 PC's explain 80% of the variance
{k_0.8 = 0
for (value in cumulative_variance){if (value < 0.8) k_0.8 = k_0.8+1}
if(cumulative_variance[k_0.8] < 0.8) k_0.8 = k_0.8+1 # if the threshold of the 80% has not yet been reached, add one more (PC_k_0.8 is the first component after reaching the 80% threshold)
k_0.8} # show the value of k_0.8

# 3. Find the elbow
screeplot(WDI_indicators.pca,
          type = "lines", 
          cex = 0.2, 
          npcs = length(WDI_indicators.pca$sdev));abline(h = mean(WDI_indicators.pca$sdev^2), col = 3)

k = min(k_1, k_0.8);k # choose the minimum value

# Transform data
loadings_pca<-WDI_indicators.pca$rotation[,1:k]
WDI_indicators_afterPCA <- WDI_indicators.pca$x%*% loadings_pca #transform data according to the chosen k

plot_prcomp(WDI_indicators.pca)


# understanding the loadings
count_magnitude<-rep(0, length(WDI_indicators.pca$rotation[,1]))
# each line's squares should sum 1, i.e. sum(WDI_indicators.pca$rotation[,j]^2 = 1 (||gamma_j|| = 1)
# if all variables would contribute the same for the PC, than their squared weight would be 1/#variables
for(i in 1:length(count_magnitude)){
  for(j in 1:k){
    if(WDI_indicators.pca$rotation[i,j]^2 > 1/length(WDI_indicators.pca$rotation[,j])) count_magnitude[i] = count_magnitude[i]+1
  }
}
barplot(count_magnitude, names.arg=rownames(WDI_indicators.pca$rotation), las=2, horiz = TRUE)



##############
# Robust PCA #
##############
library("rrcov") # Pca's
#WDI_indicators.pcaCov<-PcaCov(WDI_indicators,scale=TRUE,crit.pca.distances = 0.999) # singular covariance matrix

# PCA Grid - Projection Pursuit
WDI_indicators.pcaGrid<- PcaGrid(WDI_indicators,scale=TRUE,crit.pca.distances = 0.999)
summary(WDI_indicators.pcaGrid)

# Proportion of variance explained by each component
variance_proportion_pcaGrid <- WDI_indicators.pcaGrid$eigenvalues/sum(WDI_indicators.pcaGrid$eigenvalues) #vector of the proportion of variance explained by each component
# plot
plot(variance_proportion_pcaGrid, 
     ylab = "Proportion of variance", 
     xlab = "Principal Component Index", 
     main = "Proportion of variance explained by each PC");grid()

# Cumulative variance explained by each component
cumulative_variance_pcaGrid <- cumsum(variance_proportion_pcaGrid) #compute the cumulative variance
# plot
plot(cumulative_variance_pcaGrid, 
     ylab = "Cumulative Explained Variance", 
     xlab = "Principal Component Index", 
     main = "Cumulative variance explained by each PC");abline(0.8,0);grid()

# # Choice of principal components
# # 1.Choose *k_1* such that lambda_i >= lambda^bar (which is 1, since we are working with standardized variables) for i = 1:k_1
# k_1 = 0 
# for (value in WDI_indicators.pcaGrid$eigenvalues){ if (value >=  1) k_1 = k_1+1}
# k_1 # show the value of k_1
# 
# # 2.Choose k_0.8 such that the first k_0.8 PC's explain 80% of the variance
# k_0.8 = 0
# for (value in cumulative_variance_pcaGrid){if (value < 0.8) k_0.8 = k_0.8+1}
# if(cumulative_variance_pcaGrid[k_0.8] < 0.8) k_0.8 = k_0.8+1 # if the threshold of the 80% has not yet been reached, add one more (PC_k_0.8 is the first component after reaching the 80% threshold)
# k_0.8 # show the value of k_0.8
# 
# # choose the minimum value
# k = min(k_1, k_0.8) 
# 
# # Transform data
# WDI_indicators_afterPcaGrid <- WDI_indicators.pcaGrid$scores[,1:k]


# PCA Hubert
WDI_indicators.ROBPCA <- PcaHubert(WDI_indicators,scale=TRUE,crit.pca.distances = 0.999) #kmax = 10 in PcaHubert
summary(WDI_indicators.ROBPCA)

# Proportion of variance explained by each component
variance_proportion_ROBPCA <- WDI_indicators.ROBPCA$eigenvalues/sum(WDI_indicators.ROBPCA$eigenvalues) #vector of the proportion of variance explained by each component
# plot
plot(variance_proportion_ROBPCA, 
     ylab = "Proportion of variance", 
     xlab = "Principal Component Index", 
     main = "Proportion of variance explained by each PC");grid()

# Cumulative variance explained by each component
cumulative_variance_ROBPCA <- cumsum(variance_proportion_ROBPCA) #compute the cumulative variance
# plot
plot(cumulative_variance_ROBPCA, 
     ylab = "Cumulative Explained Variance", 
     xlab = "Principal Component Index", 
     main = "Cumulative variance explained by each PC");abline(0.8,0);grid()


screeplot(WDI_indicators.ROBPCA); abline(h=mean(WDI_indicators.ROBPCA$eigenvalues))

plot(WDI_indicators.ROBPCA,pch=20,lwd=2,col=(2-WDI_indicators.ROBPCA$flag)) # Mahalanobis Distance
# WDI_indicators.ROBPCA$flag = 0 or 1 depending on whether the observation 
# col =  2 -> plot in red (if 1 -> plot in black)

loadings_robpca<-matrix(c(WDI_indicators.ROBPCA$loadings[,1],WDI_indicators.ROBPCA$loadings[,2],WDI_indicators.ROBPCA$loadings[,3]), nrow=14, ncol=3)
WDI_indicators_afterROBPCA<-wDI_indicators.pca$x%*%loadingsrobpca

WDI_indicators.pcaROBPCA2 <- PcaHubert(WDI_indicators,scale=TRUE,k=2,crit.pca.distances = 0.999)
plot(WDI_indicators.pcaROBPCA2,pch=20,lwd=2,col=(2-WDI_indicators.pcaROBPCA$flag)) # Distance-Distance


# load.ROBPCA<-cbind(
#   round(getLoadings(WDI_indicators.pcaROBPCA),3),NA,
#   round(cor(WDI_indicators,getScores(WDI_indicators.pcaROBPCA)),3));load.ROBPCA

#diag(var(WDI_indicators))

# Test linear dependence between variables
#install.packages("plm")
library("plm")
detect.lindep(WDI_indicators)

library("rTensor")
# prepare data
WDI_eco <- WDI[grepl('eco', colnames(WDI))]; dim(WDI_eco)
WDI_sci<-WDI[grepl('sci', colnames(WDI))]; dim(WDI_sci)
WDI_geo<-WDI[grepl('geo', colnames(WDI))]; dim(WDI_geo)
WDI_dem<-WDI[grepl('dem', colnames(WDI))]; dim(WDI_dem)
WDI_hs<-WDI[grepl('hs', colnames(WDI))]; dim(WDI_hs)

WDI_multi = array(c(WDI_hs, WDI_dem), dim = c(dim(WDI_hs)[1],dim(WDI_hs)[2],dim(WDI_dem)[2] ))

###########################################
# Dataset with and without "delta" values #
###########################################

# find the indices of the columns that correspond to a "delta" value
delta_indx <- grepl('var', colnames(WDI_indicators))

# Considering the dataset without the Deltas
WDI_noDeltas <- WDI_indicators[!delta_indx] # consider only the columns that do not correspond to a "Delta" value
WDI_indicators_noDeltas <- WDI_noDeltas[,c(2:(length(WDI_noDeltas)-2))] # remove the country/year pair and the HDI

#PCA on the indicators with no deltas centering and scaling
WDI_indicators_noDeltas.pca<-prcomp(WDI_indicators_noDeltas, center=TRUE, scale. = TRUE)
summary(WDI_indicators_noDeltas.pca)

# Proportion of variance explained by each principal component
variance_proportion_noDeltas <- WDI_indicators_noDeltas.pca$sdev^2/sum(WDI_indicators_noDeltas.pca$sdev^2) # compute the proportion of variance explained by each component
# plot
plot(variance_proportion_noDeltas, 
     ylab = "Proportion of variance", 
     xlab = "Principal Component Index", 
     main = "Proportion of variance explained by each PC");grid()

# Cumulative variance explained by each principal component
cumulative_variance_noDeltas <- cumsum(variance_proportion_noDeltas) # compute the cumulative variance explained by each component
# plot
plot(cumulative_variance_noDeltas, 
     ylab = "Cumulative Explained Variance", 
     xlab = "Principal Component Index", main = "Cumulative variance explained by each PC");abline(0.8,0);grid()

# Choice of principal components
# 1.Choose *k_1* such that lambda_i >= lambda^bar (which is 1, since we are working with standardized variables) for i = 1:k_1
{k_1 = 0
  for (value in WDI_indicators_noDeltas.pca$sdev){ if (value^2 >=  1) k_1 = k_1+1}
  k_1 # show the value of k_1
  
  # 2.Choose k_0.8 such that the first k_0.8 PC's explain 80% of the variance
  k_0.8 = 0
  for (value in cumulative_variance_noDeltas){if (value < 0.8) k_0.8 = k_0.8+1}
  if(cumulative_variance_noDeltas[k_0.8] < 0.8) k_0.8 = k_0.8+1 # if the threshold of the 80% has not yet been reached, add one more (PC_k_0.8 is the first component after reaching the 80% threshold)
  k_0.8 # show the value of k_0.8
  
  k = min(k_1, k_0.8);k} # choose the minimum value

# Transform data
WDI_indicators_noDeltas_afterPCA <- WDI_indicators_noDeltas.pca$x%*% WDI_indicators_noDeltas.pca$rotation[,1:k] #transform data according to the chosen k

# understanding the loadings
{count_magnitude_noDeltas<-rep(0, length(WDI_indicators_noDeltas.pca$rotation[,1]))
  for(i in 1:length(count_magnitude_noDeltas)){
    for(j in 1:k){
      if(WDI_indicators_noDeltas.pca$rotation[i,j]^2 > sum(WDI_indicators_noDeltas.pca$rotation[,j]^2)/length(WDI_indicators_noDeltas.pca$rotation[,j])) count_magnitude_noDeltas[i] = count_magnitude_noDeltas[i]+1
    }
  }
  barplot(count_magnitude_noDeltas, names.arg=rownames(WDI_indicators_noDeltas.pca$rotation), las=2)}

###############################
# Considering only the Deltas #
###############################
WDI_indicators_Deltas<- WDI[delta_indx] # consider only the columns that correspond to a "Delta"

#PCA on the indicators with no deltas centering and scaling
WDI_indicators_Deltas.pca<-prcomp(WDI_indicators_Deltas, center=TRUE, scale. = TRUE)
summary(WDI_indicators_Deltas.pca)

# Proportion of variance explained by each component
variance_proportion_Deltas <- WDI_indicators_Deltas.pca$sdev^2/sum(WDI_indicators_Deltas.pca$sdev^2) #vector of the proportion of variance explained by each component
# plot
plot(variance_proportion_Deltas, 
     ylab = "Proportion of variance", 
     xlab = "Principal Component Index", 
     main = "Proportion of variance explained by each PC");grid()

# Cumulative variance explained by each component
cumulative_variance_Deltas <- cumsum(variance_proportion_Deltas) #compute the cumulative variance
# plot
plot(cumulative_variance_Deltas, 
     ylab = "Cumulative Explained Variance", 
     xlab = "Principal Component Index", 
     main = "Cumulative variance explained by each PC");abline(0.8,0);grid()

# Choice of principal components
# 1.Choose *k_1* such that lambda_i >= lambda^bar (which is 1, since we are working with standardized variables) for i = 1:k_1
{k_1 = 0; 
  for (value in WDI_indicators_Deltas.pca$sdev){ if (value^2 >=  1) k_1 = k_1+1}
  k_1 # show the value of k_1
  
  # 2.Choose k_0.8 such that the first k_0.8 PC's explain 80% of the variance
  k_0.8 = 0;
  for (value in cumulative_variance_Deltas){if (value < 0.8) k_0.8 = k_0.8+1}
  if(cumulative_variance_Deltas[k_0.8] < 0.8) k_0.8 = k_0.8+1 # if the threshold of the 80% has not yet been reached, add one more (PC_k_0.8 is the first component after reaching the 80% threshold)
  k_0.8 # show the value of k_0.8
  # choose the minimum value
  k = min(k_1, k_0.8); k}

# Transform data
WDI_indicators_Deltas_afterPCA <- WDI_indicators_Deltas.pca$x%*% WDI_indicators_Deltas.pca$rotation[,1:k] #transform data according to the chosen k

# understanding the loadings
{count_magnitude_Deltas<-rep(0, length(WDI_indicators_Deltas.pca$rotation[,1]))
  for(i in 1:length(count_magnitude_Deltas)){
    for(j in 1:k){
      if(WDI_indicators_Deltas.pca$rotation[i,j]^2 > sum(WDI_indicators_Deltas.pca$rotation[,j]^2)/length(WDI_indicators_Deltas.pca$rotation[,j])) count_magnitude_Deltas[i] = count_magnitude_Deltas[i]+1
    }
  }
  barplot(count_magnitude_Deltas, names.arg=rownames(WDI_indicators_Deltas.pca$rotation), las=2)}
