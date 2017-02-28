library(plyr) # load plyr first, then dplyr 
library(data.table) # a prockage that handles dataframe better
library(dplyr) # for fancy data table manipulations and organization

#READ DATA

Xtest <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt")
Ytest <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
Xtrain <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
Ytrain <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
Features <- read.table(".\\UCI HAR Dataset\\features.txt")
SubjectTrain <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
SubjectTest <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")


#DATA CLEANING
colnames(Xtrain) <- t(features[2])
colnames(Xtest) <- t(features[2])

Xtrain$activities <- ytrain[, 1]
Xtrain$participants <- SubjectTrain[, 1]
Xtest$activities <- Ytest[, 1]
Xtest$participants <- SubjectTest[, 1]

#1. Merges the training and the test sets to create one data set.

Merged<- rbind(Xtrain, Xtest)
duplicated(colnames(Merged))
Merged2 <- Merged[, !duplicated(colnames(Merged))]

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
Mean <- grep("mean()", names(Merged2), fixed = TRUE)
  #Include 555:559 as they have means and are associated with the gravity terms
Mean <- append(Mean, 471:477)
MeanMatrix <- Merged2[Mean]
  #To std
STD <- grep("std()", names(Merged2))
STDMatrix <- Merged2[STD]

#3. Uses descriptive activity names to name the activities in the data set
Merged2$activities <- as.character(Merged2$activities)
Merged2$activities[Merged2$activities == 1] <- "Walking"
Merged2$activities[Merged2$activities == 2] <- "Walking Upstairs"
Merged2$activities[Merged2$activities == 3] <- "Walking Downstairs"
Merged2$activities[Merged2$activities == 4] <- "Sitting"
Merged2$activities[Merged2$activities == 5] <- "Standing"
Merged2$activities[Merged2$activities == 6] <- "Laying"
Merged2$activities <- as.factor(Merged2$activities)


#4. Appropriately labels the data set with descriptive variable names. 
names(Merged2)  # survey the data
names(Merged2) <- gsub("Acc", "Accelerator", names(Merged2))
names(Merged2) <- gsub("Mag", "Magnitude", names(Merged2))
names(Merged2) <- gsub("Gyro", "Gyroscope", names(Merged2))
names(Merged2) <- gsub("^t", "time", names(Merged2))
names(Merged2) <- gsub("^f", "frequency", names(Merged2))

Merged2$participants <- as.character(Master$participants)
Merged2$participants[Merged2$participants == 1] <- "Participant 1"
Merged2$participants[Merged2$participants == 2] <- "Participant 2"
Merged2$participants[Merged2$participants == 3] <- "Participant 3"
Merged2$participants[Merged2$participants == 4] <- "Participant 4"
Merged2$participants[Merged2$participants == 5] <- "Participant 5"
Merged2$participants[Merged2$participants == 6] <- "Participant 6"
Merged2$participants[Merged2$participants == 7] <- "Participant 7"
Merged2$participants[Merged2$participants == 8] <- "Participant 8"
Merged2$participants[Merged2$participants == 9] <- "Participant 9"
Merged2$participants[Merged2$participants == 10] <- "Participant 10"
Merged2$participants[Merged2$participants == 11] <- "Participant 11"
Merged2$participants[Merged2$participants == 12] <- "Participant 12"
Merged2$participants[Merged2$participants == 13] <- "Participant 13"
Merged2$participants[Merged2$participants == 14] <- "Participant 14"
Merged2$participants[Merged2$participants == 15] <- "Participant 15"
Merged2$participants[Merged2$participants == 16] <- "Participant 16"
Merged2$participants[Merged2$participants == 17] <- "Participant 17"
Merged2$participants[Merged2$participants == 18] <- "Participant 18"
Merged2$participants[Merged2$participants == 19] <- "Participant 19"
Merged2$participants[Merged2$participants == 20] <- "Participant 20"
Merged2$participants[Merged2$participants == 21] <- "Participant 21"
Merged2$participants[Merged2$participants == 22] <- "Participant 22"
Merged2$participants[Merged2$participants == 23] <- "Participant 23"
Merged2$participants[Merged2$participants == 24] <- "Participant 24"
Merged2$participants[Merged2$participants == 25] <- "Participant 25"
Merged2$participants[Merged2$participants == 26] <- "Participant 26"
Merged2$participants[Merged2$participants == 27] <- "Participant 27"
Merged2$participants[Merged2$participants == 28] <- "Participant 28"
Merged2$participants[Merged2$participants == 29] <- "Participant 29"
Merged2$participants[Merged2$participants == 30] <- "Participant 30"


Merged2$participants <- as.factor(Merged2$participants)



#5. From the data set in step 4, creates a second, 
#  independent tidy data set with the average of each variable for each activity and each subject.
Merged2.dt <- data.table(Merged2)
   #Mean of every column broken down by participants and activities
TidyData <- Merged2.dt[, lapply(.SD, mean), by = 'participants,activities']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)