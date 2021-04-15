#For sentiment analysis
library(tm)
library(textstem)   #Used for lemmatize function
library(sentimentr) #Used for averaged sentiment scoring and word count
library(ggplot2)

#Enter file name with most recent manually processed data
fileName <- "RProcessedData.csv"
data <- read.csv(fileName)

#Begin character preprocessing

#Create vectors and df for quick iteration
data$Hashtag1 <- as.character(data$Hashtag1)
data$Hashtag2 <- as.character(data$Hashtag2)
data$Hashtag3 <- as.character(data$Hashtag3)
data$ShortAnswer <- as.character(data$ShortAnswer)
data$Group <- as.character(data$Group)


SentimentDF <- data.frame(data$Hashtag1,data$Hashtag2,data$Hashtag3,data$ShortAnswer,data$Group, data$IndicatedFake)

SentimentDF$data.Hashtag1 <- as.character(SentimentDF$data.Hashtag1)
SentimentDF$data.Hashtag2 <- as.character(SentimentDF$data.Hashtag2)
SentimentDF$data.Hashtag3 <- as.character(SentimentDF$data.Hashtag3)
SentimentDF$data.ShortAnswer <- as.character(SentimentDF$data.ShortAnswer)
SentimentDF$data.Group <- as.character(SentimentDF$data.Group)
SentimentDF$data.IndicatedFake <- as.numeric(SentimentDF$data.IndicatedFake)

N <- ncol(SentimentDF)

#Iterate including Short Answer, remove hashtags and corrupted characters and lowercase

for (i in 1:(N-1)) {       
  
  x <- SentimentDF[ , i]
  x <- sapply(x, function(x)  gsub("[^\x20-\x7E]", "", x))
  x <- sapply(x, function(x) gsub('([[:punct:]])', '',x))
  
  
  
  SentimentDF[ , i] <- x
}

#Iterate excluding Short Answer, perform cleaning
for (i in 1:(N-2)) {       
  x <- SentimentDF[ , i]
  
  x <- sapply(x, function(x) gsub('UN', 'united nations',x))
  
  
  #Make lowercase for easier trump conversion
  
  x <- sapply(x, tolower)
  
  #Reformat Capitalized entries to original state
  x <- sapply(x, function(x) gsub('maga', 'make america great again',x))
  x <- sapply(x, function(x) gsub('kag', 'keep america great',x))
  x <- sapply(x, function(x) gsub('trump', 'xyzabc',x)) #Note since trump has base sentiment -.1, change any reference of trump to dtrump for neutral 0 sentiment
  #x <- sapply(x, function(x) gsub('P O T U S', 'potus',x))
  #x <- sapply(x, function(x) gsub('U S A', 'usa',x))
  
  if (i == 1){ #Clean hashtag one specific entries
    x <- sapply(x, function(x) gsub('sl', 'snl',x))
    x <- sapply(x, function(x) gsub('drumpf', 'xyzabc',x))
    x <- sapply(x, function(x) gsub('trummmppp', 'xyzabc',x))
    x <- sapply(x, function(x) gsub('trumpy', 'xyzabc',x))
    x <- sapply(x, function(x) gsub('trumpet', 'xyzabc',x))
    
  }
  if (i == 2){ #Clean hashtag two specific entries
    x <- sapply(x, function(x) gsub('fdt', 'fuck xyzabc',x))
  }
  
  #Convert trump occurances to remove bias in sentiment
  x <- sapply(x, function(x) gsub('donald trump', 'xyzabc',x))
  x <- sapply(x, function(x) gsub('donaldtrump', 'xyzabc',x))
  x <- sapply(x, function(x) gsub('trump', 'xyzabc',x))
  x <- sapply(x, function(x) gsub('donald xyzabc', 'xyzabc',x))
  
  #Save the new vector in dataframe
  SentimentDF[ , i] <- x
}

#Iterate including Short Answer, remove hashtags and corrupted characters and lowercase
for (i in 1:(N-1)) {       
  x <- SentimentDF[ , i]
  x <- sapply(x, tolower)
  x <- sapply(x, function(x) removeWords(x,stopwords()))
  x <- sapply(x, function(x) lemmatize_words(x))
  
  #Save the new vector in dataframe
  SentimentDF[ , i] <- x
}

#Add variables to data frame from original data
SentimentDF$Gender <- as.character(data$Gender)
SentimentDF$SocialMediaCluster <- as.character(data$SocialMediaCluster)
SentimentDF$PoliticalCluster <- as.character(data$PoliticalCluster)

#Calculate sentiment per hashtag and create new columns for numWords and Sentiment

sentimentReportHash1 <- sentiment_by(SentimentDF$data.Hashtag1)

SentimentDF$Hash1NumWords <- sentimentReportHash1[,2]
SentimentDF$Hash1Score <- sentimentReportHash1[,4]

sentimentReportHash2 <- sentiment_by(SentimentDF$data.Hashtag2)

SentimentDF$Hash2NumWords <- sentimentReportHash2[,2]
SentimentDF$Hash2Score <- sentimentReportHash2[,4]

sentimentReportHash3 <- sentiment_by(SentimentDF$data.Hashtag3)

SentimentDF$Hash3NumWords <- sentimentReportHash3[,2]
SentimentDF$Hash3Score <- sentimentReportHash3[,4]


#Calculate linearly weighted average sentiment
n <- nrow(data)
SentimentDF$WeightedHashSentScore <- rep(0, n)
wt <- c((4/9),(1/3),(2/9))
for (i in 1:n){
  v <- c(as.numeric(SentimentDF$Hash1Score[i]),as.numeric(SentimentDF$Hash2Score[i]),as.numeric(SentimentDF$Hash3Score[i]))
  SentimentDF$WeightedHashSentScore[i] <- weighted.mean(v,wt)
}

#Create combined hashtags variable
SentimentDF$CombinedHashs <- rep(0, n)
for (i in 1:n){
  SentimentDF$CombinedHashs[i] <- paste(SentimentDF$data.Hashtag1[i],SentimentDF$data.Hashtag2[i],SentimentDF$data.Hashtag3[i], sep = " ")
  
}


#Calculate sentiment of combined hashtags
sentimentReportCombined <- sentiment_by(SentimentDF$CombinedHashs)

#Assigns list object
SentimentDF$CombinedHashNumWords <- sentimentReportCombined[,2]
#Isolate integer column only
SentimentDF$CombinedHashNumWords <- SentimentDF$CombinedHashNumWords[[1]]

#Assigns list object
SentimentDF$CombinedHashSentScore <- sentimentReportCombined[,4]
#Isolate numeric column only
SentimentDF$CombinedHashSentScore <- SentimentDF$CombinedHashSentScore[[1]]

#Visualize the two different sentiment scores (weighted v combined)
par(mfrow=c(2,2))
hist(SentimentDF$WeightedHashSentScore)
hist(SentimentDF$CombinedHashSentScore)
hist(SentimentDF$WeightedHashSentScore[which(SentimentDF$WeightedHashSentScore != 0)])
hist(SentimentDF$CombinedHashSentScore[which(SentimentDF$CombinedHashSentScore != 0)])


#Plot differences between averages to help decide how different the averages are
par(mfrow=c(1,2))
c <- SentimentDF$WeightedHashSentScore - SentimentDF$CombinedHashSentScore
c2 <- SentimentDF$WeightedHashSentScore[which(SentimentDF$WeightedHashSentScore != 0)] - SentimentDF$CombinedHashSentScore[which(SentimentDF$CombinedHashSentScore != 0)]
hist(c, main = "Difference between Weighted and Combined Hashtag Scores", xlab = "Difference in Hashtag Score")
mean(c)
hist(c2)
mean(c2)

#Run difference in means hypothesis test to determine difference in sentiment metrics
t.test(SentimentDF$WeightedHashSentScore, SentimentDF$CombinedHashSentScore, paired = TRUE, alternative = "two.sided")
#Outcome : fail to reject null that scoring methods are different
#Continue to use combined sent score

#Visualize and calc average number of words used in hashtag section
hist(SentimentDF$CombinedHashNumWords)
mean(SentimentDF$CombinedHashNumWords)


#Run sentiment on short answer
sentimentReportShortAnswer <- sentiment_by(SentimentDF$data.ShortAnswer)

#Assigns list object
SentimentDF$ShortAnswerNumWords <- sentimentReportShortAnswer[,2]
#Isolate integer column only
SentimentDF$ShortAnswerNumWords <- SentimentDF$ShortAnswerNumWords[[1]]

#Assigns list object
SentimentDF$ShortAnswerScore <- sentimentReportShortAnswer[,4]
#Isolate numeric column only
SentimentDF$ShortAnswerScore <- SentimentDF$ShortAnswerScore[[1]]


#Visualize short answer sentiment
par(mfrow=c(1,2))
hist(SentimentDF$ShortAnswerNumWords)
mean(SentimentDF$ShortAnswerNumWords)
hist(SentimentDF$ShortAnswerScore)
mean(SentimentDF$ShortAnswerScore)

#Plot Short answer sentiment by group with density functions
ggplot(SentimentDF, aes(x=ShortAnswerScore, color = data.Group)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") 

#Same plot without zeros
ggplot(subset(SentimentDF,SentimentDF$ShortAnswerScore != 0), aes(x=ShortAnswerScore, color = data.Group)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666")

a <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == "control")])
b <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == "deepfake")])
c <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == "parody")])
#plot ProbSharing with density function for each group
ggplot(SentimentDF, aes(x=ShortAnswerScore, color = data.Group)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1)


#Plot same plot over political media clusters
a <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$PoliticalCluster == "Conservative")])
b <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$PoliticalCluster == "Independent")])
c <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$PoliticalCluster == "Liberal")])
#plot ProbSharing with density function for each group
ggplot(SentimentDF, aes(x=ShortAnswerScore, color = PoliticalCluster)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1)


temp <- subset(SentimentDF, data.Group == "control")
#Plot same plot over political media clusters
a <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Conservative")])
b <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Independent")])
c <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Liberal")])
#plot ProbSharing with density function for each group
ggplot(temp, aes(x=ShortAnswerScore, color = PoliticalCluster)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1)



#Same plot grouped by gender
ggplot(SentimentDF, aes(x=ShortAnswerScore, color = Gender)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") 

# Randomized Block Design (B is the blocking factor)
fit <- aov(ShortAnswerScore ~ Gender + data.Group, data=SentimentDF) 
#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests 



#Get all words used in short response for by groups
allWords <- c("")
controlWords <- c("")
deepfakeWords <- c("")
parodyWords <- c("")
for (i in 1:n){
  a <- SentimentDF$data.ShortAnswer[i]
  w <- strsplit(a, "[[:space:]]+")[[1]]
  if (SentimentDF$data.Group[i] == 'control'){
    controlWords <- append(controlWords,w,after=length(controlWords))
  }
  if (SentimentDF$data.Group[i] == 'deepfake'){
    deepfakeWords <- append(deepfakeWords,w,after=length(deepfakeWords))
  }  
  if (SentimentDF$data.Group[i] == 'parody'){
    parodyWords <- append(parodyWords,w,after=length(parodyWords))
  }
  allWords <- append(allWords,w,after=length(allWords))
}

#Calculate proportion of trump mentions
a1 <- sum(str_count((SentimentDF$data.ShortAnswer[which(SentimentDF$data.Group == 'control')]), "xyzabc"))
b1 <- length(SentimentDF$data.ShortAnswer[which(SentimentDF$data.Group == 'control')])
PropControlTrumpReferences <- a1/b1

a2 <- sum(str_count((SentimentDF$data.ShortAnswer[which(SentimentDF$data.Group == 'deepfake')]), "xyzabc"))
b2 <- length(SentimentDF$data.ShortAnswer[which(SentimentDF$data.Group == 'deepfake')])
PropDeepfakeTrumpReferences <- a2/b2

a3 <- sum(str_count((SentimentDF$data.ShortAnswer[which(SentimentDF$data.Group == 'parody')]), "xyzabc"))
b3 <- length(SentimentDF$data.ShortAnswer[which(SentimentDF$data.Group == 'parody')])

PropParodyTrumpReferences <- a3/b3

#Show significantly more references by deepfake group
res <- prop.test(x = c(a2, sum(a1,a3)), n = c(b2, sum(b1,b3)),alternative = "greater")

#Shw ten most used words
tail(sort(table(allWords)), 10)


#Seperate hash sentiment scoeres by group
ControlShortAnswerScores <- as.numeric(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == 'control')])
DeepfakeShortAnswerScores <- as.numeric(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == 'deepfake')])
ParodyShortAnswerScores <- as.numeric(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == 'parody')])

#Compare measn and distributions of sentiment between treatment groups
par(mfrow=c(1,3))
hist(ControlShortAnswerScores, breaks = 15)
mean(ControlShortAnswerScores)
hist(DeepfakeShortAnswerScores, breaks = 15)
mean(DeepfakeShortAnswerScores)
hist(ParodyShortAnswerScores, breaks = 15)
mean(ParodyShortAnswerScores)


#Create a total sentiment score for each respondant
SentimentDF$totalSentScore <- SentimentDF$CombinedHashSentScore + SentimentDF$ShortAnswerScore

#Visualize total sentiment

#Plot same plot over treatments
a <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == "control")])
b <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == "deepfake")])
c <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$data.Group == "parody")])
#plot ProbSharing with density function for each group
ggplot(SentimentDF, aes(x=totalSentScore, color = data.Group)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1)



#Plot same plot over political media clusters
a <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$PoliticalCluster == "Conservative")])
b <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$PoliticalCluster == "Independent")])
c <- mean(SentimentDF$ShortAnswerScore[which(SentimentDF$PoliticalCluster == "Liberal")])
#plot ProbSharing with density function for each group
ggplot(SentimentDF, aes(x=totalSentScore, color = PoliticalCluster)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1) 


#############################################################################################
#Create plots for each video:

#Subset data for particular plot
temp <- subset(SentimentDF, data.Group == "control")
#Plot over political media clusters
a <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Conservative")])
b <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Independent")])
c <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Liberal")])
#plot ProbSharing with density function for each group
ggplot(temp, aes(x=totalSentScore, color = PoliticalCluster)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Control Video Sentiment Breakdown \nby Political Cluster") + 
  labs(y="Density", x = "Control Video Total Sentiment")

#Run ANOVA model for IndicatedFake by SocialMediaCluster
ATest <- aov(totalSentScore ~ PoliticalCluster, data=temp)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk <- TukeyHSD(ATest)
plot(tuk)




#Subset data for particular plot
temp <- subset(SentimentDF, data.Group == "deepfake")
#Plot over political media clusters
a <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Conservative")])
b <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Independent")])
c <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Liberal")])
#plot ProbSharing with density function for each group
ggplot(temp, aes(x=totalSentScore, color = PoliticalCluster)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Deepfake Video Sentiment Breakdown \nby Political Cluster") + 
  labs(y="Density", x = "Deepfake Video Total Sentiment")



#Subset data for particular plot
temp <- subset(SentimentDF, data.Group == "parody")
#Plot over political media clusters
a <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Conservative")])
b <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Independent")])
c <- mean(temp$ShortAnswerScore[which(temp$PoliticalCluster == "Liberal")])
#plot ProbSharing with density function for each group
ggplot(temp, aes(x=totalSentScore, color = PoliticalCluster)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") + 
  geom_vline(aes(xintercept=a),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=b),
             color="green", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=c),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Parody Video Sentiment Breakdown \nby Political Cluster") + 
  labs(y="Density", x = "Parody Video Total Sentiment")


#Run ANOVA model for IndicatedFake by SocialMediaCluster
ATest <- aov(totalSentScore ~ PoliticalCluster, data=temp)

#checks for heteroscedasticity, normality, and influential observerations.
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(ATest) # diagnostic plots
#Run Post Hoc analysis to show diffferences between groups
tuk <- TukeyHSD(ATest)
plot(tuk)

