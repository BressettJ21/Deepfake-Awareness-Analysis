
#Load in necessary libraries for visualization
library(ggplot2)



#Enter file name with most recent manually processed data
fileName <- "RProcessedData.csv"
data <- read.csv(fileName)


par(mfrow=c(1,2))
hist(as.numeric(data$MinutesToComplete[which(data$MinutesToComplete < 100)]), breaks = 40, main = "Minutes Taken for Survey (Under 100)", xlab = "")
mean(as.numeric(data$MinutesToComplete[which(data$MinutesToComplete < 30)]))
hist(data$ProbSharing, main = "Probability of Sharing with Friends",xlab = "")

#Create Viz for minutes taken to complete
temp <- subset(data, MinutesToComplete < 30)
p1 <- ggplot(temp, aes(x=MinutesToComplete)) + 
  geom_histogram(color="black", fill="white")
p1+ geom_vline(aes(xintercept=mean(MinutesToComplete)),
              color="blue", linetype="dashed", size=1)

#plot ProbSharing with density function for each group
ggplot(data, aes(x=ProbSharing, color = Group)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") 

#Plot a mix of several variables
ggplot(data, aes(x=IndicatedFake, y=Age, shape=Group, color=Gender)) +
  geom_point()


temp <- subset(data, secondsWatchingVideo < 120)
a <- mean(temp$secondsWatchingVideo[which(temp$Group == "Control")])
b <- mean(temp$secondsWatchingVideo[which(temp$Group == "Deepfake")])
c <- mean(temp$secondsWatchingVideo[which(temp$Group == "Parody")])

#plot ProbSharing with density function for each group
ggplot(temp, aes(x=secondsWatchingVideo, color = Group)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1)


#Run ANOVA model for IndicatedFake by Group
ATest <- aov(secondsWatchingVideo ~ Group, data=temp)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk<- TukeyHSD(ATest)
plot(tuk)




a <- mean(data$CodedNewsSources[which(data$Group == "Control")])
b <- mean(data$CodedNewsSources[which(data$Group == "Deepfake")])
c <- mean(data$CodedNewsSources[which(data$Group == "Parody")])

#plot ProbSharing with density function for each group
ggplot(data, aes(x=CodedNewsSources, color = Group)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1)

#Run ANOVA model for IndicatedFake by Group
ATest <- aov(CodedNewsSources ~ Group, data=data)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk<- TukeyHSD(ATest)
plot(tuk)



a <- mean(data$CodedNewsSources[which(data$PoliticalCluster == "Conservative")])
b <- mean(data$CodedNewsSources[which(data$PoliticalCluster == "Independent")])
c <- mean(data$CodedNewsSources[which(data$PoliticalCluster == "Liberal")])
#plot ProbSharing with density function for each group
ggplot(data, aes(x=CodedNewsSources, color = PoliticalCluster)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1)

#Run ANOVA model for IndicatedFake by Group
ATest <- aov(CodedNewsSources ~ PoliticalCluster, data=data)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk<- TukeyHSD(ATest)
plot(tuk)

#Quickplot all numerical data
par(mfrow=c(1,3))
hist(data$Age)
hist(data$NumHoursPerDaySocMedia)
hist(data$secondsWatchingVideo)

#Quickplot Demographic info (not age)
par(mfrow=c(1,2))
hist(data$Age)
hist(data$White)
ggplot(data, aes(x=Age)) + 
  geom_histogram(colour="black", fill="white", bins = 15)
ggplot(data, aes(x=White)) + 
  geom_histogram(colour="black", fill="white", bins = 2)


#Quickplot Demographic info (not age)
par(mfrow=c(1,2))
plot(data$Gender)
plot(data$Race)

#Quickplot Social Media info
par(mfrow=c(1,3))
plot(data$MostUsedPlatform)
plot(data$NumPlatformsUsedPerWeek)
plot(data$HowUseSocialMedia)

#Quickplot Political info
par(mfrow=c(1,2))
plot(data$WhoIsPaulRyan)
mean(data$WhoIsPaulRyan_Paul.Ryan)
mean(data$WhoIsNancyPelosi_Nancy.Pelosi)
#Determine if difference is significant
n <- nrow(data)
res <- prop.test(x = c(sum(data$WhoIsPaulRyan_Paul.Ryan), sum(data$WhoIsNancyPelosi_Nancy.Pelosi)), n = c(n, n))

plot(data$WhoIsNancyPelosi)
plot(data$WhereGetNews)
plot(data$EconomicPolicies)
plot(data$SocialPolicies)
plot(data$NumTalkPoliticsPerDay)

#Quickplot Speaking responses
par(mfrow=c(1,3))
plot(data$Presidential)
plot(data$Representative)
plot(data$Humourous)

par(mfrow=c(1,2))
hist(data$CodedNewsSources, breaks = 10)
hist(data$NumNewsSources, breaks = 10)


plot(data$PoliticalCluster)
plot(data$SocialMediaCluster)

plot(data$WhereGetNews)
hist(data$CodedNewsSources, breaks = 10, xlab = "Bipartisan Scale", main = "CodedNewsSources")






##############################################################################################

#Run ANOVA model for IndicatedFake by PoliticalCluster
ATest <- aov(IndicatedFake ~ PoliticalCluster, data=data)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk<- TukeyHSD(ATest)
plot(tuk)

#Run ANOVA model for IndicatedFake by SocialMediaCluster
ATest <- aov(IndicatedFake ~ SocialMediaCluster, data=data)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk<- TukeyHSD(ATest)
plot(tuk)

a <- mean(data$IndicatedFake[which(data$Group == "Control")])
b <- mean(data$IndicatedFake[which(data$Group == "Deepfake")])
c <- mean(data$IndicatedFake[which(data$Group == "Parody")])
d <- c(a,b,c)
d

#Create confidence interval for deepfake detect rate
prop.test(sum(data$IndicatedFake[which(data$Group == "Deepfake")]), length(data$IndicatedFake[which(data$Group == "Deepfake")]))

#Run ANOVA model for IndicatedFake by PoliticalCluster
ATest <- aov(QualityLoss_Video ~ Group, data=data)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk<- TukeyHSD(ATest)
plot(tuk)

