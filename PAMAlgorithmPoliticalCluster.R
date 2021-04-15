
'''
This program focuses on implemetation of the Partition Around the Mediod unsupervised learning algorithm
to cluster social media and political behaviors for the deepfake study results

To be completed:
Writing and updating cluster column back into global data set

Jack R. Bressett
'''
#Data manipulation package
library(dplyr)
#Partition Around Mediod algorithm in clustering package
library(cluster)
#For visualizing the clusters
library(Rtsne)
library(ggplot2)
#For making indicator variables from column (one-hot encode)
library(fastDummies)
#Load in all the data
fileName <- 'RProcessedData.csv'
loadedData <- read.csv(fileName)



#Input columns for the clustering algorithm to use for social media profile
columnsUsed <- c('EconomicPolicies','SocialPolicies', 'NumTalkPoliticsPerDay', 'CodedNewsSources')

#Subset Data into specified columns, since I plan to use these groups for hypothesis testing with responses,
PoliticalDF <- subset(loadedData, select = columnsUsed)

#Remove NAs for calculating Gower Distance
CleanedPoliticalDF <- na.omit(PoliticalDF)


gower_df <- daisy(CleanedPoliticalDF,
                  metric = "gower" ,
                  type = list(logratio = 2))

summary(gower_df)


findBestCluster <- function(clusters,gower_df){
  
  #Create for vector silhouette scores
  silhouette <- c()
  
  silhouette = c(silhouette, NA)
  
  #Iterate through clusters
  for(i in 2:clusters){
    pam_clusters = pam(as.matrix(gower_df),
                       diss = TRUE,
                       k = i)
    silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
  }
  
  #Make simple plot showing silhouette width over different clusters
  plot(1:clusters, silhouette,
       xlab = "Clusters",
       ylab = "Silhouette Width")
  #Draw lines connecting data points
  lines(1:clusters, silhouette)
}

#Set hyperparameter limit
clusters <- 7

#Locate best cluster
findBestCluster(clusters,gower_df)

#Run model with optimized hyperparameter
PoliticalPAM = pam(as.matrix(gower_df), diss = TRUE, k = 3)

#Check out cluster information
CleanedPoliticalDF[PoliticalPAM$medoids,]

#Create tsne object which converts grower_df to lower dimensional space
tsne_object <- Rtsne(gower_df, is_distance = TRUE)

#Create dataframe with three columns: X,Y and cluster
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(PoliticalPAM$clustering))

#Plot clusters in a low dimensional space
ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))

#Add clusters to the end of the clean dataframe
CleanedPoliticalDF$PoliticalCluster <- as.numeric(tsne_df$cluster)
n <- nrow(CleanedPoliticalDF)
for (i in 1:n){
  if (CleanedPoliticalDF$PoliticalCluster[i] == 1){
    CleanedPoliticalDF$PoliticalCluster[i] <- "Conservative"
  }
  if (CleanedPoliticalDF$PoliticalCluster[i] == 2){
    CleanedPoliticalDF$PoliticalCluster[i] <- "Liberal"
  }  
  if (CleanedPoliticalDF$PoliticalCluster[i] == 3){
    CleanedPoliticalDF$PoliticalCluster[i] <- "Independent"
  }
}

loadedData$PoliticalCluster <- CleanedPoliticalDF$PoliticalCluster
write.csv(loadedData,'RProcessedData.csv')
