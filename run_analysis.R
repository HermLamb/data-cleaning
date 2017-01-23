
################################################################################
#
## aim: completing the peer-graded assignment from the Getting and Cleaning 
##    Data Course Project
## date: 2017-01-23
## name: Herm Lamberink
#
################################################################################

## load required libraries
library(plyr);library(dplyr)
library(tidyr)

## create "data" directory
if( !file.exists("data")) {dir.create("data")}


# --------------------------------------------------------------------------
# download data
# --------------------------------------------------------------------------

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file( fileURL, "./data/accelerometers.zip")
unzip("data/accelerometers.zip", exdir = "./data")
unlink("data/accelerometers.zip")

# --------------------------------------------------------------------------
# read data
# --------------------------------------------------------------------------

test <- read.table("data/UCI HAR Dataset/test/X_test.txt", sep = "")
testlabels <- read.table("data/UCI HAR Dataset/test/Y_test.txt", sep = "")
train <- read.table("data/UCI HAR Dataset/train/X_train.txt", sep = "")
trainlabels <- read.table("data/UCI HAR Dataset/train/Y_train.txt", sep = "")
subjecttest <- read.table("data/UCI HAR Dataset/test/subject_test.txt", sep = "")
subjecttrain <- read.table("data/UCI HAR Dataset/train/subject_train.txt", sep = "")
features <- read.table("data/UCI HAR Dataset/features.txt", sep = "")

# --------------------------------------------------------------------------
# tidying and merging
# --------------------------------------------------------------------------

# rename "[test|train]labels" from V1 into 'activity', rename [test|train]subjects
# from V1 into 'subject' 
testlabels <- testlabels %>% rename(activity = V1) 
trainlabels <- trainlabels %>% rename(activity = V1)
subjecttest <- subjecttest %>% rename(subject = V1)
subjecttrain <- subjecttrain %>% rename(subject = V1)

# merge test+testlabels+subjects, merge train+trainlabels+subjects
test <- cbind(subjecttest, testlabels, test)
train <- cbind(subjecttrain, trainlabels, train)

# merge test and train sets
test <- mutate(test, type = "test")
train <- mutate(train, type = "train")
data <- bind_rows(train, test)

# attach variable names
first <- c("subject", "activity")
middle <- as.character(features$V2)
last <- c("type")
names(data) <- c(first, middle, last)

# name the activities
data$activity <- as.character(data$activity)
data$activity <- revalue(data$activity, c("1" = "walking",
                                                        "2" = "walkingupstairs", 
                                                        "3" = "walkingdownstairs", 
                                                        "4" = "sitting", 
                                                        "5" = "standing", 
                                                        "6" = "laying")) 

# create valid (non-duplicated) variable names
valid_column_names <- make.names(names=names(data), unique = TRUE, allow_ = TRUE)
names(data) <- valid_column_names

# select only the columns with either mean or sd for each measurement
# whilst maintaining the colums 'subject', 'activity' and 'type'. First,
# reorder the variables with select()
data <- select(data, type, activity, subject, everything())
data <- data[,grepl("activity|subject|type|mean|std", names(data))]
data <- select(data, -contains("meanFreq"))

# rewrite variable names to descriptive variable names
#read: features_info.txt
names(data) <- sub("t", "time", names(data))
names(data) <- sub("timeype", "type", names(data))
names(data) <- sub("actimeivity", "activity", names(data))
names(data) <- sub("subjectime", "subject", names(data))
names(data) <- sub("stimed", "std", names(data))
names(data) <- sub("f", "freq", names(data))
names(data) <- sub("\\.\\.\\.", "_", names(data))
names(data) <- sub("\\.\\.", "_", names(data))
names(data) <- sub("Acc", ".accelleration", names(data))
names(data) <- sub("Gyro", ".orientation", names(data))
names(data) <- sub("Jerk", ".jerksignal", names(data))
names(data) <- sub("Mag", ".magnitude", names(data))


## create new dataset with average for each activity and each subject

# create vehicle
newdata <- data.frame()

# compute averages per activity per subject
for(a in c("laying", "sitting", "standing", "walking", "walkingdownstairs",
           "walkingupstairs")) {
  df <- filter(data, activity == a)
  
   for(s in c(1:30)){
    datasubject <- filter(df, subject == s)
    df2 <- as.data.frame(t(colMeans(datasubject[,c(4:69)])))
    df2 <- cbind(datasubject[1,c(1:3)], df2)
    newdata <- rbind(newdata, df2)
  }
}

# change names newdata
newnames <- paste("av_", names(data), sep = '')
names(newdata) <- newnames

# --------------------------------------------------------------------------
# write output
# --------------------------------------------------------------------------

## create output directory
outdir <- 'tidy.data'
dir.create( outdir, showWarnings = FALSE )

## write tidy data to files
write.csv( data, "tidy.data/accellerometers_tidy.csv", row.names = FALSE)
write.csv( newdata, "tidy.data/accellerometers_averages.csv", row.names = FALSE)

