clean-data-project
==================

The purpose of this R project is to provide a tidy data set for later analysis from the source data of this smartphone trial set: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


##Setup
Download and unzip the raw data to your working directory. The unzipped directory of raw data should be titled: "UCI HAR Dataset"

Download the run_analysis.R to your working directory

##Running the script
The run_analysis.R contains a run_analysis function that when called returns the tidy data set

To run the script just type the following into the console

source("run_analysis.R")
df <- run_analysis()

*Note the example drops the tidy data into a new data.frame df.


##Code
run_analysis <- function() {
    
    ##Loads training data and adds the subjects and activity labels to the data frame
    trainingData <- read.table("./UCI HAR Dataset/train/X_train.txt")
    trainingLabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
    trainingData$Activity = c(t(trainingLabels))
    trainingSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    trainingData$Subject = c(t(trainingSubjects))
    
    ##Loads test data and adds the subjects and activity labels to the data frame
    testData <- read.table("./UCI HAR Dataset/test/X_test.txt")    
    testLabels <- read.table("./UCI HAR Dataset/test/y_test.txt")    
    testData$Activity = c(t(testLabels))   
    testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt") 
    testData$Subject = c(t(testSubjects))
      
    ##Merges the training and the test sets to create one data set.
    mergedData <- rbind(trainingData, testData)
    
    
    ##Loads the variable labels
    varLabels <- read.table("./UCI HAR Dataset/features.txt")
    
    ##Sets the raw variable names on the merged data set
    names(mergedData) <- c(as.character(varLabels[,2]), "Activity", "Subject")
       
    
    ##Extracts only the measurements on the mean and standard deviation for each measurement.
    trimmedData <- mergedData[,c(names(mergedData)[grep("ean", names(mergedData))] ,names(mergedData)[grep("std", names(mergedData))], "Activity", "Subject")]
    
    
    ##Loads the activity keys for using human readable activity descriptions
    activityKeys <- read.table("./UCI HAR Dataset/activity_labels.txt")
    
    ##Creates a list split by subjects and activities
    bySubjectByActivity <- split(trimmedData, list(trimmedData$Subject, trimmedData$Activity))
    
    ##calculates the mean for each variable by user nad by activity
    meansBySByA <- lapply(bySubjectByActivity, function(x) colMeans(x[,names(trimmedData[, c(-87, -88)])]))
    
    
    ##Builds the tidy data frame
    for(i in 1:30) {  
        for(j in 1:6) {
            
            if(i == 1 & j == 1)
            {
                tidyData <- data.frame(matrix(unlist(meansBySByA[["1.1"]][1:86]), nrow=1, byrow=T))
                names(tidyData) = names(meansBySByA[["1.1"]][])
                
                tidyData$subject = 1
                ##Uses descriptive activity names to name the activities in the data set
                tidyData$activity = as.character(activityKeys[activityKeys$V1 == j, 2])
                
            } else {
                listName <- paste(i, j, sep=".")
                
                tempDf <- data.frame(matrix(unlist(meansBySByA[[listName]][1:86]), nrow=1, byrow=T))
                names(tempDf) = names(meansBySByA[[listName]][])
                tempDf$subject = i
                ##Uses descriptive activity names to name the activities in the data set
                tempDf$activity = as.character(activityKeys[activityKeys$V1 == j, 2])
                
                tidyData <- rbind(tidyData, tempDf)
            }
            
        } 
    }
    
    ##Appropriately labels the data set with descriptive activity names. 
    
    ##Removes the ()
    names(tidyData) = gsub("\\(\\)", "", names(tidyData))
    
    ##Removes the  -
    names(tidyData) = gsub("-", ".", names(tidyData))
    
    
    ##Returns the tidy data set
    tidyData
     
    
}
