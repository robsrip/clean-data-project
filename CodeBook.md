#Variables
The original variables included in the data set are as follows with the original variable name on the left followed by the R friendlier variable name in the tidy data output. All variables that included "std" for Standard Deviation measurements or "mean" and "Mean" were inluded in the data set

* tBodyAcc-mean()-X	tBodyAcc.mean.X
* tBodyAcc-mean()-Y	tBodyAcc.mean.Y
* tBodyAcc-mean()-Z	tBodyAcc.mean.Z
* tGravityAcc-mean()-X	tGravityAcc.mean.X
* tGravityAcc-mean()-Y	tGravityAcc.mean.Y
* tGravityAcc-mean()-Z	tGravityAcc.mean.Z
* tBodyAccJerk-mean()-X	tBodyAccJerk.mean.X
* tBodyAccJerk-mean()-Y	tBodyAccJerk.mean.Y
* tBodyAccJerk-mean()-Z	tBodyAccJerk.mean.Z
* tBodyGyro-mean()-X	tBodyGyro.mean.X
* tBodyGyro-mean()-Y	tBodyGyro.mean.Y
* tBodyGyro-mean()-Z	tBodyGyro.mean.Z
* tBodyGyroJerk-mean()-X	tBodyGyroJerk.mean.X
* tBodyGyroJerk-mean()-Y	tBodyGyroJerk.mean.Y
* tBodyGyroJerk-mean()-Z	tBodyGyroJerk.mean.Z
* tBodyAccMag-mean()	tBodyAccMag.mean
* tGravityAccMag-mean()	tGravityAccMag.mean
* tBodyAccJerkMag-mean()	tBodyAccJerkMag.mean
* tBodyGyroMag-mean()	tBodyGyroMag.mean
* tBodyGyroJerkMag-mean()	tBodyGyroJerkMag.mean
* fBodyAcc-mean()-X	fBodyAcc.mean.X
* fBodyAcc-mean()-Y	fBodyAcc.mean.Y
* fBodyAcc-mean()-Z	fBodyAcc.mean.Z
* fBodyAcc-meanFreq()-X	fBodyAcc.meanFreq.X
* fBodyAcc-meanFreq()-Y	fBodyAcc.meanFreq.Y
* fBodyAcc-meanFreq()-Z	fBodyAcc.meanFreq.Z
* fBodyAccJerk-mean()-X	fBodyAccJerk.mean.X
* fBodyAccJerk-mean()-Y	fBodyAccJerk.mean.Y
* fBodyAccJerk-mean()-Z	fBodyAccJerk.mean.Z
* fBodyAccJerk-meanFreq()-X	fBodyAccJerk.meanFreq.X
* fBodyAccJerk-meanFreq()-Y	fBodyAccJerk.meanFreq.Y
* fBodyAccJerk-meanFreq()-Z	fBodyAccJerk.meanFreq.Z
* fBodyGyro-mean()-X	fBodyGyro.mean.X
* fBodyGyro-mean()-Y	fBodyGyro.mean.Y
* fBodyGyro-mean()-Z	fBodyGyro.mean.Z
* fBodyGyro-meanFreq()-X	fBodyGyro.meanFreq.X
* fBodyGyro-meanFreq()-Y	fBodyGyro.meanFreq.Y
* fBodyGyro-meanFreq()-Z	fBodyGyro.meanFreq.Z
* fBodyAccMag-mean()	fBodyAccMag.mean
* fBodyAccMag-meanFreq()	fBodyAccMag.meanFreq
* fBodyBodyAccJerkMag-mean()	fBodyBodyAccJerkMag.mean
* fBodyBodyAccJerkMag-meanFreq()	fBodyBodyAccJerkMag.meanFreq
* fBodyBodyGyroMag-mean()	fBodyBodyGyroMag.mean
* fBodyBodyGyroMag-meanFreq()	fBodyBodyGyroMag.meanFreq
* fBodyBodyGyroJerkMag-mean()	fBodyBodyGyroJerkMag.mean
* fBodyBodyGyroJerkMag-meanFreq()	fBodyBodyGyroJerkMag.meanFreq
* angle(tBodyAccMean,gravity)	angle(tBodyAccMean,gravity)
* angle(tBodyAccJerkMean),gravityMean)	angle(tBodyAccJerkMean),gravityMean)
* angle(tBodyGyroMean,gravityMean)	angle(tBodyGyroMean,gravityMean)
* angle(tBodyGyroJerkMean,gravityMean)	angle(tBodyGyroJerkMean,gravityMean)
* angle(X,gravityMean)	angle(X,gravityMean)
* angle(Y,gravityMean)	angle(Y,gravityMean)
* angle(Z,gravityMean)	angle(Z,gravityMean)
* tBodyAcc-std()-X	tBodyAcc.std.X
* tBodyAcc-std()-Y	tBodyAcc.std.Y
* tBodyAcc-std()-Z	tBodyAcc.std.Z
* tGravityAcc-std()-X	tGravityAcc.std.X
* tGravityAcc-std()-Y	tGravityAcc.std.Y
* tGravityAcc-std()-Z	tGravityAcc.std.Z
* tBodyAccJerk-std()-X	tBodyAccJerk.std.X
* tBodyAccJerk-std()-Y	tBodyAccJerk.std.Y
* tBodyAccJerk-std()-Z	tBodyAccJerk.std.Z
* tBodyGyro-std()-X	tBodyGyro.std.X
* tBodyGyro-std()-Y	tBodyGyro.std.Y
* tBodyGyro-std()-Z	tBodyGyro.std.Z
* tBodyGyroJerk-std()-X	tBodyGyroJerk.std.X
* tBodyGyroJerk-std()-Y	tBodyGyroJerk.std.Y
* tBodyGyroJerk-std()-Z	tBodyGyroJerk.std.Z
* tBodyAccMag-std()	tBodyAccMag.std
* tGravityAccMag-std()	tGravityAccMag.std
* tBodyAccJerkMag-std()	tBodyAccJerkMag.std
* tBodyGyroMag-std()	tBodyGyroMag.std
* tBodyGyroJerkMag-std()	tBodyGyroJerkMag.std
* fBodyAcc-std()-X	fBodyAcc.std.X
* fBodyAcc-std()-Y	fBodyAcc.std.Y
* fBodyAcc-std()-Z	fBodyAcc.std.Z
* fBodyAccJerk-std()-X	fBodyAccJerk.std.X
* fBodyAccJerk-std()-Y	fBodyAccJerk.std.Y
* fBodyAccJerk-std()-Z	fBodyAccJerk.std.Z
* fBodyGyro-std()-X	fBodyGyro.std.X
* fBodyGyro-std()-Y	fBodyGyro.std.Y
* fBodyGyro-std()-Z	fBodyGyro.std.Z
* fBodyAccMag-std()	fBodyAccMag.std
* fBodyBodyAccJerkMag-std()	fBodyBodyAccJerkMag.std
* fBodyBodyGyroMag-std()	fBodyBodyGyroMag.std
* fBodyBodyGyroJerkMag-std()	fBodyBodyGyroJerkMag.std

##Additional variables
The subject id and the activity label were included in the tidy data output
* subject
* activity

#The Data
For each variable listed above their is a value for the mean across all of the observations of that mesurement per user per activity in the tidy output.

