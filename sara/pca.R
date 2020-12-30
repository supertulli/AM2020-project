library(readr)
WDI <- read_csv("../data/WDI.csv")
WDI_indicators<-WDI[,c(2:(length(WDI)-2))] # the last two columns are the HDI and HDI-delta and the first is the country/year pair
HDI<-WDI[,c(length(WDI)-1, length(WDI))]
WDI_countryYear<-WDI[,1]
WDI_indicators.pca<-prcomp(WDI_indicators, center=TRUE, scale. = TRUE) #PCA on the indicators centering and scaling
summary(WDI_indicators.pca)
# # install.packages("devtools")
# library(devtools)
# # install_github("vqv/ggbiplot")
# library(ggbiplot)
# ggbiplot(WDI_indicators.pca)
cumulative_variance <- cumsum(WDI_indicators.pca$sdev^2/sum(WDI_indicators.pca$sdev^2)) #cumulative variance explained by each component
plot(cumulative_variance, ylab = "Cumulative Explained Variance", xlab = "Principal Component Index", main = "Cumulative variance explained by each PC")
grid()
variance_proportion <- WDI_indicators.pca$sdev^2/sum(WDI_indicators.pca$sdev^2) #this gives the proportion of variance explained by each component
plot(variance_proportion, ylab = "Proportion of variance", xlab = "Principal Component Index", main = "Proportion of variance explained by each PC")
grid()
k = 0 # choose *k* such that lambda_i >= lambda^bar (which is 1, since we are working with standardized variables)
for (var in WDI_indicators.pca$sdev){ if (var^2 >=  1) k = k+1}
k # show the value of k
WDI_indicators_afterPCA <- WDI_indicators.pca$x%*% WDI_indicators.pca$rotation[,1:k]

# find the indices of the columns that correspond to a "delta" value
delta_indx <- grepl('Delta', colnames(WDI)) 

# Considering the dataset without the Deltas
WDI_noDeltas <- WDI[!delta_indx] # consider only the columns that do not correspond to a "Delta" value
WDI_indicators_noDeltas <- WDI_noDeltas[,c(2:(length(WDI_noDeltas)))] # remove the country/year pair (the HDI has Delta on its name)
WDI_indicators_noDeltas.pca<-prcomp(WDI_indicators_noDeltas, center=TRUE, scale. = TRUE) #PCA on the indicators with no deltas centering and scaling
summary(WDI_indicators_noDeltas.pca)
cumulative_variance_noDeltas <- cumsum(WDI_indicators_noDeltas.pca$sdev^2/sum(WDI_indicators_noDeltas.pca$sdev^2)) #cumulative variance explained by each component
plot(cumulative_variance_noDeltas, ylab = "Cumulative Explained Variance", xlab = "Principal Component Index", main = "Cumulative variance explained by each PC")
grid()
variance_proportion_noDeltas <- WDI_indicators_noDeltas.pca$sdev^2/sum(WDI_indicators_noDeltas.pca$sdev^2) #this gives the proportion of variance explained by each component
plot(variance_proportion_noDeltas, ylab = "Proportion of variance", xlab = "Principal Component Index", main = "Proportion of variance explained by each PC")
grid()
k = 0 # choose *k* such that lambda_i >= lambda^bar (which is 1, since we are working with standardized variables)
for (var in WDI_indicators_noDeltas.pca$sdev){ if (var^2 >=  1) k = k+1}
k # show the value of k
WDI_indicators_noDeltas_afterPCA <- WDI_indicators_noDeltas.pca$x%*% WDI_indicators_noDeltas.pca$rotation[,1:k]


# Considering only the Deltas 
WDI_Deltas<- WDI[delta_indx] # consider only the columns that correspond to a "Delta"
WDI_indicators_Deltas <- WDI_Deltas[,c(1:(length(WDI_noDeltas)-1))] # remove the HDI
WDI_indicators_Deltas.pca<-prcomp(WDI_indicators_Deltas, center=TRUE, scale. = TRUE) #PCA on the indicators with no deltas centering and scaling
summary(WDI_indicators_Deltas.pca)
cumulative_variance_Deltas <- cumsum(WDI_indicators_Deltas.pca$sdev^2/sum(WDI_indicators_Deltas.pca$sdev^2)) #cumulative variance explained by each component
plot(cumulative_variance_Deltas, ylab = "Cumulative Explained Variance", xlab = "Principal Component Index", main = "Cumulative variance explained by each PC")
grid()
variance_proportion_Deltas <- WDI_indicators_Deltas.pca$sdev^2/sum(WDI_indicators_Deltas.pca$sdev^2) #this gives the proportion of variance explained by each component
plot(variance_proportion_Deltas, ylab = "Proportion of variance", xlab = "Principal Component Index", main = "Proportion of variance explained by each PC")
grid()
k = 0 # choose *k* such that lambda_i >= lambda^bar (which is 1, since we are working with standardized variables)
for (var in WDI_indicators_Deltas.pca$sdev){ if (var^2 >=  1) k = k+1}
k # show the value of k
WDI_indicators_Deltas_afterPCA <- WDI_indicators_Deltas.pca$x%*% WDI_indicators_Deltas.pca$rotation[,1:k]
