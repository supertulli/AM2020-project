# generic libraries
library(devtools) # close png

#-----------------------------------------------------------------
# import data
data<-read.csv("andre/WDI_afterMRMR.csv", header = TRUE, sep = ",") #TODO: replace when files are at the right place
row.names(data) = data[,1] # set row names
data = data[, -1] # remove row names

#----------Principal Component Analysis----------------------------
#libaries
library(rrcov)
library(DataExplorer)

# Classical PCA --------------------------------------------------
# apply classical pca
results_pcaClassic<-PcaClassic(data, scale = TRUE)
summary(results_pcaClassic)

# choice of k (number of principal components)
# 1.Find *k_1* such that lambda_i >= \bar{lambda} (which is 1, since we are working with standardized variables) for i = 1:k_1
k_1 = 0; for (value in results_pcaClassic$eigenvalues){ if (value >=  1) k_1 = k_1+1};k_1 # show the value of k_1
# 2.Find k_0.8 such that the first k_0.8 PC's explain 80% of the variance
{k_0.8 = 0;
  # compute the cumulative proportion of explained variance
cumVar_pcaClassic<-cumsum(results_pcaClassic$eigenvalues/sum(results_pcaClassic$eigenvalues));
for (value in cumVar_pcaClassic){if (value < 0.8) k_0.8 = k_0.8+1}
if(cumVar_pcaClassic[k_0.8] < 0.8) k_0.8 = k_0.8+1 # if the threshold of the 80% has not yet been reached, add one more (PC_k_0.8 is the first component after reaching the 80% threshold)
k_0.8} # show the value of k_0.8
# 3.Choose the minimum k
k = min(k_1,k_0.8);k

# confirm the value of k by plotting the eigenvalues and finding the elbow
png(filename = "figures/scree_pcaClassic.png", width = 800, height = 600)
screeplot(results_pcaClassic, 
          type="lines",
          cex = 0.2, 
          npcs = length(results_pcaClassic$eigenvalues),
          main = "Eigenvalues of each component"); abline(h = mean(results_pcaClassic$eigenvalues), col = 3)
dev.off()

# loadings visual analysis
bp_classic_names <- c(rep("PC1" , dim(results_pcaClassic$loadings)[1]) , rep("PC2" , dim(results_pcaClassic$loadings)[1]) , 
                rep("PC3" , dim(results_pcaClassic$loadings)[1]), rep("PC4" , dim(results_pcaClassic$loadings)[1]))
variables <- rep(rownames(results_pcaClassic$loadings),4)
bp_classic_loadings <- c(results_pcaClassic$loadings[,1], results_pcaClassic$loadings[,2], results_pcaClassic$loadings[,3], results_pcaClassic$loadings[,4])
bp_data_classic <- data.frame(bp_classic_names,variables,bp_classic_loadings)
# plot
png(filename = "figures/loadings_pcaClassic.png", width = 800, height = 600)
ggplot(bp_data_classic, aes(fill=bp_classic_loadings, y=variables, x=bp_classic_loadings, size=4)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~bp_classic_names, nrow = 2) +
  theme(legend.position="none") +
  xlab("Relative Importance")
dev.off()

# save the transformed data
data_afterPCA<-getScores(results_pcaClassic)[,1:k]

# Robus PCA --------------------------------------------------
# apply robust pca
results_pcaRobust<-PcaHubert(data,scale=TRUE,crit.pca.distances = 0.999)
summary(results_pcaRobust)

# plot the eigenvalues and find the elbow
png(filename = "figures/scree_pcaRobust.png", width = 800, height = 600)
screeplot(results_pcaRobust, 
          type="lines",
          cex = 0.2, 
          npcs = length(results_pcaRobust$eigenvalues),
          main = "Eigenvalues of each component"); abline(h = mean(results_pcaRobust$eigenvalues), col = 3)
dev.off()

#plot the outliers detected by the robust pca
png(filename = "figures/outliers_pcaRobust.png", width = 800, height = 600)
plot(results_pcaRobust,pch=20,lwd=2,col=(2-results_pcaRobust$flag))
dev.off()

# loadings visual analysis
bp_robust_names <- c(rep("PC1" , dim(results_pcaRobust$loadings)[1]) , rep("PC2" , dim(results_pcaRobust$loadings)[1]) , rep("PC3" , dim(results_pcaRobust$loadings)[1]))
variables <- rep(rownames(results_pcaRobust$loadings),3)
bp_robust_loadings <- c(results_pcaRobust$loadings[,1], results_pcaRobust$loadings[,2], results_pcaRobust$loadings[,3])
bp_robust_data <- data.frame(bp_robust_names,variables,bp_robust_loadings)
# plot
png(filename = "figures/loadings_pcaRobust.png", width = 800, height = 600)
ggplot(bp_robust_data, aes(fill=bp_robust_loadings, y=variables, x=bp_robust_loadings)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~bp_robust_names) +
  theme(legend.position="none") +
  xlab("")
dev.off()

# transform data (only 3 PC's so consider the whole set)
data_afterROBPCA<-getScores(results_pcaRobust)
#----------------------------------------------------------------



