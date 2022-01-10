#Load tidyverse libraries
library(tidyverse)

#Get Data and Cleanup Environment
url <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip <- "UCI HAR Dataset.zip"
dir <- "UCI HAR Dataset"
if (!file.exists(zip)) {
        download.file(url, zip)
}
if (!file.exists(dir)) {
        unzip(zip)
}
rm(dir, url, zip)

#read label data
features <-
        read_table("./UCI HAR Dataset/features.txt", col_names = F)
activitylabel <-
        read_table("./UCI HAR Dataset/activity_labels.txt", col_names = F)
activitylabel[, 2] <-
        c("walk", "upstairs", "downstairs", "sit", "stand", "lie")

#read training data
s_train <-
        read_table("./UCI HAR Dataset/train/subject_train.txt", col_names = F)
X_train <-
        read_table("./UCI HAR Dataset/train/X_train.txt", col_names = F)
y_train <-
        read_table("./UCI HAR Dataset/train/y_train.txt", col_names = F)

#read testing data
s_test <-
        read_table("./UCI HAR Dataset/test/subject_test.txt", col_names = F)
X_test <-
        read_table("./UCI HAR Dataset/test/X_test.txt", col_names = F)
y_test <-
        read_table("./UCI HAR Dataset/test/y_test.txt", col_names = F)

#apply column names to training data
names(X_train) <- as.character(features$X2)
names(y_train) <- "activity"
names(s_train) <- "subject"

#apply column names to testing data
names(X_test) <- as.character(features$X2)
names(y_test) <- "activity"
names(s_test) <- "subject"

#combine  training data into a dataframe
train <- cbind(s_train, X_train, y_train)
test <- cbind(s_test, X_test, y_test)
full <- rbind(train, test)
rm(s_train,
   X_train,
   y_train,
   s_test,
   X_test,
   y_test,
   train,
   test)

#define columns wanted
filter <- grep("-mean..$|-std..$", features$X2, value = TRUE)
full <- full[, c("subject", filter, "activity")] %>%
        mutate(activity = factor(activity, levels = activitylabel$X1, labels = activitylabel$X2))
rm(activitylabel, features, filter)

##reformat variable names
names(full)[2:19] <- names(full)[2:19] %>%
        str_replace("^(\\d)*\\s", "") %>%
        str_replace(".{2}$", "") %>%
        str_replace("^t", "Time") %>%
        str_replace("^f", "Frequency") %>%
        str_replace("AccJerk", "LinearJerk") %>%
        str_replace("GyroJerk", "AngularJerk") %>%
        str_replace("Acc", "LinearAcceleration") %>%
        str_replace("Gyro", "AngularVelocity") %>%
        str_replace("Mag", "") %>%
        str_replace("BodyBody", "Body") %>%
        str_replace("-(m)", "M") %>%
        str_replace("-(s)", "S")
        

##Summarise Full
means <- group_by(full, activity,subject) %>%
        summarise_all(mean)

#Output data as text file
write.table(means, file = "tidydata.txt",row.names = F,col.names = T)

#Optionally remove any extraneous data not in the tidy dataset.
unlink("UCI HAR Dataset",recursive = T)
unlink("UCI HAR Dataset.zip")
