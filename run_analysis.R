############################################################################################
# 0. Download data-set into working directory and unzip
############################################################################################


# setwd("C:/Users/dsanz001/Desktop/Clients/Data Science/Coursera/Getting and Cleaning Data/Project")
# if(!file.exists("./data")){
#   dir.create("./data")
# }
# 
# file.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# file.dest <- "data/data_set.zip"
# 
# download.file(file.url,file.dest,method="auto")
# 
# unzip(file.dest,exdir="data")

unzipped.files.path <- file.path("data" , "UCI HAR Dataset")

############################################################################################
# 1. Merges the training and the test sets to create one data set
############################################################################################

# Read test files
features.test <- read.table(file.path(unzipped.files.path, "test", "X_test.txt"), header = FALSE)
activity.test <- read.table(file.path(unzipped.files.path, "test", "y_test.txt"), header = FALSE)
subject.test <- read.table(file.path(unzipped.files.path, "test", "subject_test.txt"), header = FALSE)

# Read train files
features.train <- read.table(file.path(unzipped.files.path, "train", "X_train.txt"), header = FALSE)
activity.train <- read.table(file.path(unzipped.files.path, "train", "y_train.txt"), header = FALSE)
subject.train <- read.table(file.path(unzipped.files.path, "train", "subject_train.txt"), header = FALSE)

#Read feature names file
feature.names <- read.table(file.path(unzipped.files.path, "features.txt"), header = FALSE)

# Merge data
subject.merged <- rbind(subject.test,subject.train)
activity.merged <- rbind(activity.test, activity.train)
features.merged <- rbind(features.test, features.train)

names(subject.merged) <- "subject"
names(activity.merged) <- "activity"
names(features.merged) <- feature.names$V2

merged.subject.activity <- cbind(subject.merged, activity.merged)
all.data.merged <- cbind(features.merged, merged.subject.activity)

############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement
############################################################################################

mean.std.feature.names <- feature.names$V2[grep("-(mean|std)\\(\\)", feature.names$V2)]
filter.std.mean.subject.activity <- c(as.character(mean.std.feature.names), "subject","activity")
all.data.filtered <- subset(all.data.merged, select=filter.std.mean.subject.activity)

############################################################################################
# 3. Uses descriptive activity names to name the activities in the data set
############################################################################################

activity.labels <- read.table(file.path(unzipped.files.path, "activity_labels.txt"), header = FALSE)

activity.number = 1
for (activity.label in activity.labels$V2) {
  all.data.filtered$activity <- gsub(activity.number, activity.label, all.data.filtered$activity)
  activity.number <- activity.number + 1
}

############################################################################################
# 4. Appropriately labels the data set with descriptive variable names 
############################################################################################

names(all.data.filtered)<-gsub("Acc", "Accelerometer", names(all.data.filtered))
names(all.data.filtered)<-gsub("BodyBody", "Body", names(all.data.filtered))
names(all.data.filtered)<-gsub("Gyro", "Gyroscope", names(all.data.filtered))
names(all.data.filtered)<-gsub("Mag", "Magnitude", names(all.data.filtered))
names(all.data.filtered)<-gsub("std", "stdev", names(all.data.filtered))
names(all.data.filtered)<-gsub("^t", "time", names(all.data.filtered))
names(all.data.filtered)<-gsub("^f", "frequency", names(all.data.filtered))

############################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.
############################################################################################

library(plyr)
all.data.tidy <- aggregate(. ~ subject + activity, all.data.filtered, mean)
write.table(all.data.tidy, file = "tidydata.txt", row.name=FALSE)





