#import libraries

library(dplyr)

#set working directory

setwd("Z:/Pop Health Analytics Team/Other/SAS to Open Source Resources/Coursera - Getting & Cleaning Data/")

#read in all data

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

var_names <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

####1. Merge the training and the test sets to create one data set.

x_total <- rbind(x_train, x_test)
y_total <- rbind(y_train, y_test)
sub_total <- rbind(sub_train, sub_test)

####2. Extract only the measurements on the mean and standard deviation for each measurement.

measurement <- var_names[grep("mean\\(\\)|std\\(\\)",var_names[,2]),]
x_total <- x_total[,measurement[,1]]

####3. Use descriptive activity names to name the activities in the data set

colnames(y_total) <- "activity"
y_total$activitylabel <- factor(y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- y_total[,-1]

####4. Appropriately label the data set with descriptive variable names.

colnames(x_total) <- var_names[measurement[,1],2]

####5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for
#each activity and each subject.

colnames(sub_total) <- "subject"
total <- cbind(x_total, activitylabel, sub_total)
total_mean <- total %>% 
  group_by(activitylabel, subject) %>% 
    summarize_each(funs(mean))

#export
write.table(total_mean, file = "./UCI HAR Dataset/final_data.txt", row.names = FALSE, col.names = TRUE)
