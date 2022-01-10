library("dplyr")

wd <- getwd()

# X : Features
# y : Activity

activityLabels <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", sep =""))
activityLabels <- as.character(activityLabels[,2])

featureLabels <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", sep =""))
featuresW <- grep("(mean|std)\\(\\)", featureLabels[, 2])

features <- featureLabels[featuresW,2]
measures <- str_replace_all(features, "[()]","")

#load train dataset
trainx <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", sep =""))
trainy <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", sep =""))
subject_train <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", sep =""))
trainx <- trainx[,featuresW]

names(subject_train) <- c("Subject")
names(trainx) <- measures
names(trainy) <- c("Activity")

train <- cbind(subject_train, trainx, trainy)


#load test dataset
testx <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", sep =""))
testy <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", sep =""))
subject_test <- read.table(paste(wd, "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", sep =""))
names(subject_test) <- c("Subject")

testx <- testx[,featuresW]

names(subject_test) <- c("Subject")
names(testx) <- measures
names(testy) <- c("Activity")

test <- cbind(subject_test, testx, testy)


#Merge Datasets
mds <- rbind(train, test)

mds$ActivityName <- activityLabels[mds$Activity]

mds <- mds[complete.cases(mds),]

tidyds <- mds %>% group_by(Subject, ActivityName) %>% 
  summarise_at(vars(1:ncol(mds)-1), list(name = mean))


write.table(tidyds,"tidy.txt",sep="\t",row.names
