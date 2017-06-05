##Loading packages for the project
library(data.table)
library(reshape2)

##Set your working directory/folder to start the project
setwd("C:/Users/tiago/Desktop/Coursera - Data Science/ProgrammingAssignment4/Project")
path<-getwd()

## Retrieve the data for the project
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

f <- "Dataset.zip"
if (!file.exists(path)) 
        {dir.create(path)
        }
download.file(url, file.path(path, f))

## After downloading the file, go to the folder where the download occur and unzip the files in the same location. 
## For the purposes of this project, the files in the Inertial Signals folders are not going to be used.

## Read the files
insidepath <- file.path(path, "UCI HAR Dataset")
data_train<-fread(file.path(insidepath, "train", "subject_train.txt"))
data_test<-fread(file.path(insidepath, "test", "subject_test.txt"))
data_ytrain <- fread(file.path(insidepath, "train", "Y_train.txt"))
data_ytest  <- fread(file.path(insidepath, "test" , "Y_test.txt" ))

## Read x_train.txt and X_test.txt and convert from a dataframe into a datatable

dfTodt <- function (f) {
        df <- read.table(f)
        all_data <- data.table(df)
}
dtTrain <- dfTodt(file.path(insidepath, "train", "X_train.txt"))
dtTest  <- dfTodt(file.path(insidepath, "test" , "X_test.txt" ))

## Question 1: Merges the training and the test data sets to create one data set.

subject_data <- rbind(data_train, data_test)
setnames(subject_data, "V1", "subject")
activity_data <- rbind(data_ytrain, data_ytest)
setnames(activity_data, "V1", "num_activity")
all_data<- rbind(dtTrain, dtTest)

## Creates one data set named all_data
subject_activity <- cbind(subject_data, activity_data)
all_data <- cbind(subject_activity, all_data)

setkey(all_data,subject,num_activity)

## Question 2: Extract only the measurements on the mean and standard deviation for each measurement.

## To solve this question, first we will need to read the features.txt file. This file will tells us the variables in dt that are measurements for the mean and std.

data_features<-fread(file.path(insidepath,"features.txt"))
setnames(data_features,names(data_features), c("id_feature","name_feature"))

## Subset only measurements for the mean and standard deviation.         
data_features <- data_features[grepl("mean\\(\\)|std\\(\\)", name_feature)]         

## Convert the ID feature column to a vector of variable names matching columns in the data set "data_features"

        data_features$featurecode <- data_features[, paste0("V", id_feature)]
        head(data_features)
        data_features$featurecode

## Subset these variables using variable names.
        
select <- c(key(all_data), data_features$featurecode)
all_data <- all_data[, select, with=FALSE]

## Question 3: Use descriptive activity names based on the "activity_labels.txt" file. 
## This will be used to add descriptive names to the activities.
        
ActivityNames <- fread(file.path(insidepath, "activity_labels.txt"))
setnames(ActivityNames, names(ActivityNames), c("num_activity", "NameActivity"))
        
## Label with descriptive activity names
        
## Merge activity labels.
        
all_data <- merge(all_data, ActivityNames, by = "num_activity", all.x=TRUE)

## Add NameActivity variable as a key.
        
setkey(all_data, subject, num_activity, NameActivity)
        
## Melt the data table to reshape it from a short and wide format to a tall and narrow format.
        
all_data <- data.table(melt(all_data, key(all_data), variable.name="featurecode"))

## Merge NameActivity.
        
all_data <- merge(all_data, data_features[, list(id_feature, featurecode, name_feature)], by="featurecode", all.x=TRUE)

## Create a new variable called activity in the all_data set that is equivalent to activityName as a factor. 
## Create another new variable called feature that is equivalent to name_feature and also as a factor.
        
        all_data$activity <- factor(all_data$NameActivity)
        all_data$feature <- factor(all_data$name_feature)
        
        
## Question 4:Appropriately labels the data set with descriptive variable names.
## Separate features from featureName using a function called grepthis
        
grepthis <- function (regex) {
        grepl(regex, all_data$feature)
        }
## Features with 1 category

all_data$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
all_data$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
all_data$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
all_data$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
all_data$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
all_data$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

## Show unique 2 values in featVariable - SD and Mean
unique(all_data$featVariable)


## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
all_data$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))


## Question 5: Create a tidy data set with the average of each variable for each activity and each subject.

setkey(all_data, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
tidy_data <- all_data[, list(count = .N, average = mean(value)), by=key(all_data)]

## And finally to save data table objects to a tab-delimited text file called DatasetHumanActivityRecognitionUsingSmartphones.txt.

f <- file.path(insidepath, "DatasetHumanActivityRecognitionUsingSmartphones.txt")
write.table(tidy_data, f, quote=FALSE, sep="\t", row.names=FALSE)
