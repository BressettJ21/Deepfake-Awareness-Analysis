
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
columnsUsed <- c('MostUsedPlatform', 'NumPlatformsUsedPerWeek', 'NumHoursPerDaySocMedia','HowUseSocialMedia')

#Subset Data into specified columns, since I plan to use these groups for hypothesis testing with responses,
SocialDF <- subset(loadedData, select = columnsUsed)

#Remove NAs for calculating Gower Distance
CleanedSocialDF <- na.omit(SocialDF)

#Change HowUseSocialMedia to series of indicator variables for different use cases
colsToSeperate <- c('HowUseSocialMedia')
CleanedSocialDF <- dummy_cols(CleanedSocialDF, select_columns = colsToSeperate)

#Remove unwanted blank column from splitting
CleanedSocialDF <- subset(CleanedSocialDF, select = -c(HowUseSocialMedia_))


gower_df <- daisy(CleanedSocialDF,
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
clusters <- 10

#Locate best cluster
findBestCluster(clusters,gower_df)

#Run model with optimized hyperparameter
SocialPAM = pam(as.matrix(gower_df), diss = TRUE, k = 6)

#Check out cluster information
CleanedSocialDF[SocialPAM$medoids, ]

#Create tsne object which converts grower_df to lower dimensional space
tsne_object <- Rtsne(gower_df, is_distance = TRUE)

#Create dataframe with three columns: X,Y and cluster
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(SocialPAM$clustering))

#Plot clusters in a low dimensional space
ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))

#Add clusters to the end of the clean dataframe
CleanedSocialDF$SocialMediaCluster <- as.numeric(tsne_df$cluster)

n <- nrow(CleanedSocialDF)
for (i in 1:n){
  if (CleanedSocialDF$SocialMediaCluster[i] == 1){
    CleanedSocialDF$SocialMediaCluster[i] <- "Light-Variety-Facebook"
  }
  if (CleanedSocialDF$SocialMediaCluster[i] == 2){
    CleanedSocialDF$SocialMediaCluster[i] <- "Light-Public-Instagram"
  }  
  if (CleanedSocialDF$SocialMediaCluster[i] == 3){
    CleanedSocialDF$SocialMediaCluster[i] <- "Light-Friends-Snapchat"
  }
  if (CleanedSocialDF$SocialMediaCluster[i] == 4){
    CleanedSocialDF$SocialMediaCluster[i] <- "Heavy-Friends-Snapchat"
  }  
  if (CleanedSocialDF$SocialMediaCluster[i] == 5){
    CleanedSocialDF$SocialMediaCluster[i] <- "Heavy-Friends-Instagram"
  }
  if (CleanedSocialDF$SocialMediaCluster[i] == 6){
    CleanedSocialDF$SocialMediaCluster[i] <- "Heavy-Variety-TikTok"
  }
  
}


loadedData$SocialMediaCluster <- CleanedSocialDF$SocialMediaCluster
write.csv(loadedData,'RProcessedData.csv')
