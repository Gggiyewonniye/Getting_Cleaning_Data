

library(dplyr)

## download zip file containing data
zipDS3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFileData <- "UCI HAR Dataset.zip"

if (!file.exists(zipFileData)) {
  download.file(zipDS3, zipFileData, mode = "wb")
}

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFileData)
}


## Get training data using read and read test data
tSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
tValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
tActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

tstSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
tstValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
tstActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

## read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")
##===========================================================================
## Merges the training and the test sets to create one data set.

## concatenate individual data tables to make single data table
humanActivity <- rbind(
  cbind(tSubjects, tValues, tActivity),
  cbind(tstSubjects, tstValues, tstActivity)
)

## to save memory remove individual data tables 
rm(tSubjects, tValues, tActivity, 
   tstSubjects, tstValues, tstActivity)

## then need assigning column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")


##==============================================================================
## Extract only the measurements on the mean and standard deviation for each measurement


columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]


##=============================================================================
## Use descriptive activity names to name the activities in the data set

humanActivity$activity <- factor(humanActivity$activity, 
                              levels = activities[, 1], labels = activities[, 2])


##============================================================================
## Appropriately label the data set with descriptive variable names

humanActivityCols <- colnames(humanActivity)
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

## expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
colnames(humanActivity) <- humanActivityCols


###========================================================================
## Create a second, independent tidy set with the average of each variable for each activity and each subject

humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

## output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)