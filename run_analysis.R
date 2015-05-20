if (!getwd() == "./temp_temp") {
  dir.create("./temp_temp")
}

library(plyr) 
library(data.table)
library(dplyr) 

temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
unzip(temp, list = TRUE) #This provides the list of variables and I choose the ones that are applicable for this data set
YTest <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))
XTest <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
SubjectTest <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
YTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
XTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))
SubjectTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))
Features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))
unlink(temp)


#Concatenate the data tables by rows
#Give the name to the column of XTrain and test
colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])

## Add the colum activities (whici is YTrain or YTest) 
## and participants (which is SubjectTrain or SubjectTest) to X Train & Test from  
XTrain$activities <- YTrain[, 1]
XTrain$participants <- SubjectTrain[, 1]
XTest$activities <- YTest[, 1]
XTest$participants <- SubjectTest[, 1]

#Q1 Merges the training and the test sets to create one data set.
Merged <- rbind(XTrain, XTest)

#Q2 Extracts only the measurements on the mean and standard deviation for each measurement. 
pick_mean_std_Features<-Features$V2[grep("mean\\(\\)|std\\(\\)", Features$V2)]

as.char_pick_mean_std_Features <- as.character(pick_mean_std_Features)
as.char_pick_mean_std_Features <- c(as.char_pick_mean_std_Features,"activities","participants")
Sub_Merged_MeanStd <- subset(Merged, select =as.char_pick_mean_std_Features)
head(Sub_Merged_MeanStd,1)

##Another way
Pick_Mean <- grep("mean()", names(Merged), value = FALSE, fixed = TRUE)
Pick_Std <- grep("std()", names(Merged), value = FALSE, fixed = TRUE)

Sub_Merged_Mean2 <- Merged[Pick_Mean]
Sub_Merged_Std2z <-Merged[Pick_Std]

#Q3 Uses descriptive activity names to name the activities in the data set
Merged$activities <- as.character(Merged$activities)  #change the class to replace strings
Merged$activities[Merged$activities == 1] <- "Walking dead"
Merged$activities[Merged$activities == 2] <- "Walking dead Upstairs"
Merged$activities[Merged$activities == 3] <- "Walking dead Downstairs"
Merged$activities[Merged$activities == 4] <- "Sitting doing nothing"
Merged$activities[Merged$activities == 5] <- "Standing like a stupid"
Merged$activities[Merged$activities == 6] <- "Laying like a lazyboy"

Sub_Merged_MeanStd$activities<- as.character(Sub_Merged_MeanStd$activities)  #change the class to replace strings
Sub_Merged_MeanStd$activities[Sub_Merged_MeanStd$activities == 1] <- "Walking dead"
Sub_Merged_MeanStd$activities[Sub_Merged_MeanStd$activities == 2] <- "Walking dead Upstairs"
Sub_Merged_MeanStd$activities[Sub_Merged_MeanStd$activities == 3] <- "Walking dead Downstairs"
Sub_Merged_MeanStd$activities[Sub_Merged_MeanStd$activities == 4] <- "Sitting doing nothing"
Sub_Merged_MeanStd$activities[Sub_Merged_MeanStd$activities == 5] <- "Standing like a stupid"
Sub_Merged_MeanStd$activities[Sub_Merged_MeanStd$activities == 6] <- "Laying like a lazyboy"

#Q4 Appropriately labels the data set with descriptive variable names. 
# let's do just: Accelerator, magnitude Gyroscope time and frequenzy
names(Merged) <- gsub("Acc", "Accelerator", names(Merged))
names(Merged) <- gsub("Mag", "Magnitude", names(Merged))
names(Merged) <- gsub("Gyro", "Gyroscope", names(Merged))
names(Merged) <- gsub("^t", "time", names(Merged))
names(Merged) <- gsub("^f", "frequency", names(Merged))

names(Sub_Merged_MeanStd) <- gsub("Acc", "Accelerator", names(Sub_Merged_MeanStd))
names(Sub_Merged_MeanStd) <- gsub("Mag", "Magnitude", names(Sub_Merged_MeanStd))
names(Sub_Merged_MeanStd) <- gsub("Gyro", "Gyroscope", names(Sub_Merged_MeanStd))
names(Sub_Merged_MeanStd) <- gsub("^t", "time", names(Sub_Merged_MeanStd))
names(Sub_Merged_MeanStd) <- gsub("^f", "frequency", names(Sub_Merged_MeanStd))

#Q5 From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject


duplicated(colnames(Merged))
Merged <- Merged[, !duplicated(colnames(Merged))]

duplicated(colnames(Sub_Merged_MeanStd))
Sub_Merged_MeanStd <- Sub_Merged_MeanStd[, !duplicated(colnames(Sub_Merged_MeanStd))]

final<-aggregate(. ~participants + activities, Sub_Merged_MeanStd, mean)
final<-final[order(final$participants,final$activities),]

write.table(final, "c:\\Users\\Andrea\\Desktop\\coursera\\Get_cleaning_data\\Project_2\\final_with_Arg.txt", row.name=FALSE)
