library(dplyr)

# Data preparation 1: Get data

Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
File <- "UCI HAR Dataset.zip"

if (!file.exists(File)) {
        download.file(Url, File, mode = "wb")
}

# Unzip the zip file containing data
Path <- "UCI HAR Dataset"
if (!file.exists(Path)) {
        unzip(File)
}

# Data preparation 2: Read the data

# Training data
TrSubjects <- read.table(file.path(Path, "train", "subject_train.txt"))
TrValues <- read.table(file.path(Path, "train", "X_train.txt"))
TrActivity <- read.table(file.path(Path, "train", "y_train.txt"))

# Test data
TestSubjects <- read.table(file.path(Path, "test", "subject_test.txt"))
TestValues <- read.table(file.path(Path, "test", "X_test.txt"))
TestActivity <- read.table(file.path(Path, "test", "y_test.txt"))

# read features and activity labels
features <- read.table(file.path(Path, "features.txt"), as.is = TRUE)

Activities <- read.table(file.path(Path, "Activity_labels.txt"))
colnames(Activities) <- c("ActivityId", "ActivityLabel")

##############################################################################
# First Step:- Merge the training and the test sets to create one data set


# 1: making a single data table

HumanAct <- rbind(
        cbind(TrSubjects, TrValues, TrActivity),
        cbind(TestSubjects, TestValues, TestActivity)
)

# remove individual data tables to save memory
rm(TrSubjects,TrValues, TrActivity, 
   TestSubjects, TestValues, TestActivity)

# assign column names
colnames(HumanAct) <- c("Subject", features[, 2], "activity")


##############################################################################
# Second Step:  Extract only the measurements on the mean and standard deviation
# for each measurement


KeptColumns <- grepl("Subject|activity|mean|std", colnames(HumanAct))

# ... and keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]


##############################################################################
# Third Step: - Use descriptive activity names to name the activities in the data
#          set

# replace activity values with named factor levels
HumanAct$Activity <- factor(HumanAct$Activity, 
                                 levels = activities[, 1], labels = activities[, 2])


##############################################################################
# Fourth Step: - Appropriately label the data set with descriptive variable names


# get column names
HumanActivityCols <- colnames(HumanAct)

# remove special characters
HumanActivityCols <- gsub("[\\(\\)-]", "", HumanActivityCols)

# expand abbreviations and clean up names
HumanActivityCols <- gsub("^f", "frequencyDomain", HumanActivityCols)
HumanActivityCols <- gsub("^t", "timeDomain", HumanActivityCols)
HumanActivityCols <- gsub("Acc", "Accelerometer", HumanActivityCols)
HumanActivityCols <- gsub("Gyro", "Gyroscope", HumanActivityCols)
HumanActivityCols <- gsub("Mag", "Magnitude", HumanActivityCols)
HumanActivityCols <- gsub("Freq", "Frequency", HumanActivityCols)
HumanActivityCols <- gsub("mean", "Mean", HumanActivityCols)
HumanActivityCols <- gsub("std", "StandardDeviation", HumanActivityCols)
HumanActivityCols <- gsub("BodyBody", "Body", HumanActivityCols)

# use new labels as column names
colnames(HumanAct) <- HumanActivityCols

##############################################################################
# Fifth Step: - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject

# group by subject and activity and summarise using mean
HumanActMeans <- HumanAct %>% 
        group_by(Subject, Activity) %>%
        summarise_each(list(mean))

# output to file "tidy_data.txt"
write.table(HumanActMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
