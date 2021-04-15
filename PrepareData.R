#For dealing with datetime
library(lubridate)
#For making indicator variables from column (one-hot encode)
library(fastDummies)
#For counting occurences of characters in string
library(stringr)


#Enter file name with most recent manually processed data
fileName <- "PostHashTreatmentManualPoliticalMediaConsumptionReview_February 6, 2021_18.4.csv"
loadedData <- read.csv(fileName)


#function to quickly generate contact file and remove identifying info
ProcessEmailInfo <- function(loadedData){
  #Make email list, assumes format either: No or email
  emails <-loadedData$IncludeEmail[which(loadedData$IncludeEmail != 'No')]
  write.csv(emails,'EmailContacts.csv')
  
  #Drop emails from dataset
  loadedData <- subset(loadedData, select = -c(IncludeEmail))
}

#Shuffle data function to decrease bias in analysis
shuffleData <- function(data){
  #Set seed for reproducable results
  set.seed(42)
  #create vector of random numbers that is length of data
  rows <- sample(nrow(data))
  #Reorder with random vector
  data <- data[rows,]
}



#Call function to process emails then remove them from data for confidential and unbias analysis
loadedData <- ProcessEmailInfo(loadedData)


#Subset complete and usable data for proper analysis and remove columns due to repitition and lack of substance. 
#Note: Removing consent vars is fine since UseVideoResponse ensures respondents agreed
data <- subset(loadedData, select = -c(EndDate, Progress, Finished, ResponseID, 
                                       Language, ConfirmConsent, Duration.seconds.))

#Reformat dates and calculate minutes spent on survey
data$RecordedDate <- mdy_hm(data$RecordedDate)
data$StartDate <- mdy_hm(data$StartDate)
data$MinutesToComplete <- as.numeric(difftime(data$RecordedDate,data$StartDate))

#Scale column
data$ProbSharing <- data$ProbSharing/10 #Divide since scale was out of ten

#Call function to Shuffle data
data <- shuffleData(data)

#Due to imbalance, create White indicator variable
n <- nrow(data)
data$White <- rep(0, n)
White <- which(data$Race == 'White')
for (i in White){
  data$White[i] <- 1
}


data$secondsWatchingVideo <- rep(0, n)
for (i in 1:n){
  data$secondsWatchingVideo[i] <- sum(data$SecondsWatchingVideoControl[i],data$SecondsWatchingVideoDeepfake[i],data$SecondsWatchingVideoParody[i], na.rm = TRUE)
}
data <- subset(data, select = -c(SecondsWatchingVideoControl, SecondsWatchingVideoDeepfake, SecondsWatchingVideoParody))

#Change WhoIs to series of indicator variables for different answer cases (celebrity, fictional, politician, correct)
#Change WhereGetNews to isolate different media groups
#Change QualityLoss for specific type of quality loss
#Change Economic/Social Policies to help identify bipartisan groups
colsToSeperate <- c('WhoIsPaulRyan', 'WhoIsNancyPelosi', 'QualityLoss', 'EconomicPolicies', 'SocialPolicies')
data <- dummy_cols(data, select_columns = colsToSeperate)

#^^^This gets messy with 52 vars so maybe not encode all these or explore different option or keep it


#Count the number of character sequences in each element of string
NumLeft <- str_count(as.character(data$WhereGetNews), "Left")
NumRight <- str_count(as.character(data$WhereGetNews), "Right")
NumNeutral <- str_count(as.character(data$WhereGetNews), "Neutral")
data$NumNewsSources <- NumLeft + NumRight + NumNeutral
#Make a coded average by assigning negative values to Left score and zero to neutral score, written in an efficent algebraic form 
data$CodedNewsSources <- (NumRight-NumLeft)/data$NumNewsSources

#Save data
write.csv(data,'RProcessedData.csv')
