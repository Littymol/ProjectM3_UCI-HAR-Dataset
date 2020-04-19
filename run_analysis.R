#Getting and Cleaning Data Course Project by Littymol Chacko

# If required packages are not present then install them.
# code from https://class.coursera.org/getdata-008/forum/thread?thread_id=247#post-1074
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")};library(tidyr)

# If the dataset is not present in the current working directory then download it
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("UCI HAR Dataset")) {
  if (!file.exists("my_data")) {
    dir.create("my_data")
  }
  download.file(fileUrl, destfile="my_data/raw.zip")
  unzip("my_data/raw.zip", exdir=".")
}

# Step 1 - Merges the training and the test sets to create one data set.

# read training data
x_train <- read.table("UCI HAR Dataset//train/X_train.txt")
sub_train <- read.table("UCI HAR Dataset//train/subject_train.txt", col.names=c("subject"))
y_train <- read.table("UCI HAR Dataset/train//y_train.txt", col.names=c("activity"))
final_train<- cbind(x_train, sub_train, y_train)

# Read test data
x_test <- read.table("UCI HAR Dataset//test/X_test.txt")
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
y_test <- read.table("UCI HAR Dataset/test//y_test.txt", col.names=c("activity"))
final_test<- cbind(x_test, sub_test,y_test)

# merge both train and test data
merged_data <- rbind(final_train, final_test)


# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

# Read features
feature_list <- read.table("UCI HAR Dataset//features.txt", col.names = c("id", "name"))
features <- c(as.vector(feature_list[, "name"]), "subject", "activity")

# filter only features that has mean or std in the name
filtered_feature_ids <- grepl("mean|std|subject|activity", features) & !grepl("meanFreq", features)
filtered_data = merged_data[, filtered_feature_ids]


# Step 3 - Uses descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset//activity_labels.txt", col.names=c("id", "name"))
for (i in 1:nrow(activities)) {
  filtered_data$activity[filtered_data$activity == activities[i, "id"]] <- as.character(activities[i, "name"])
}


# step 4 - Appropriately labels the data set with descriptive variable names.

# make feature names more human readble
filtered_feature_names <- features[filtered_feature_ids]
filtered_feature_names <- gsub("\\(\\)", "", filtered_feature_names)
filtered_feature_names <- gsub("Acc", "-acceleration", filtered_feature_names)
filtered_feature_names <- gsub("Mag", "-Magnitude", filtered_feature_names)
filtered_feature_names <- gsub("^t(.*)$", "\\1-time", filtered_feature_names)
filtered_feature_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feature_names)
filtered_feature_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feature_names)
filtered_feature_names <- gsub("BodyBody", "Body", filtered_feature_names)
filtered_feature_names <- tolower(filtered_feature_names)

# assign names to features
names(filtered_data) <- filtered_feature_names


# step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- tbl_df(filtered_data) %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean)) 
  


# Save the data into the file
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)
