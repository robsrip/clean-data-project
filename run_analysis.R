run_analysis <- function() {
    
    ##Merges the training and the test sets to create one data set.
    trainingData <- read.table("./UCI HAR Dataset/train/X_train.txt")
    trainingLabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
    trainingData$Activity = c(t(trainingLabels))
    trainingSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    trainingData$Subject = c(t(trainingSubjects))
    
    
    testData <- read.table("./UCI HAR Dataset/test/X_test.txt")    
    testLabels <- read.table("./UCI HAR Dataset/test/y_test.txt")    
    testData$Activity = c(t(testLabels))   
    testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt") 
    testData$Subject = c(t(testSubjects))
      
    mergedData <- rbind(trainingData, testData)
    
    ##Appropriately labels the data set with descriptive activity names. 
    varLabels <- read.table("./UCI HAR Dataset/features.txt")
    
    names(mergedData) <- c(as.character(varLabels[,2]), "Activity", "Subject")
       
    
    ##Extracts only the measurements on the mean and standard deviation for each measurement.
    trimmedData <- mergedData[,c(names(mergedData)[grep("ean", names(mergedData))] ,names(mergedData)[grep("std", names(mergedData))], "Activity", "Subject")]
    
       
    ##Uses descriptive activity names to name the activities in the data set
    activityKeys <- read.table("./UCI HAR Dataset/activity_labels.txt")
    
    bySubjectByActivity <- split(trimmedData, list(trimmedData$Subject, trimmedData$Activity))
    
    meansBySByA <- lapply(bySubjectByActivity, function(x) colMeans(x[,names(trimmedData[, c(-87, -88)])]))
    
    for(i in 1:30) {  
        for(j in 1:6) {
            
            if(i == 1 & j == 1)
            {
                tidyData <- data.frame(matrix(unlist(meansBySByA[["1.1"]][1:86]), nrow=1, byrow=T))
                names(tidyData) = names(meansBySByA[["1.1"]][])
                
                tidyData$subject = 1
                tidyData$activity = as.character(activityKeys[activityKeys$V1 == j, 2])
                
            } else {
                listName <- paste(i, j, sep=".")
                
                tempDf <- data.frame(matrix(unlist(meansBySByA[[listName]][1:86]), nrow=1, byrow=T))
                names(tempDf) = names(meansBySByA[[listName]][])
                tempDf$subject = i
                tempDf$activity = as.character(activityKeys[activityKeys$V1 == j, 2])
                
                tidyData <- rbind(tidyData, tempDf)
            }
            
        } 
    }
    
    names(tidyData) = gsub("\\(\\)", "", names(tidyData))
    
    names(tidyData) = gsub("-", ".", names(tidyData))
    
    tidyData
     
    
}
