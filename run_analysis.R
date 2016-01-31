#
#
#Getting And Cleaning Data Course Project
#
#file performs steps required for the course project
#
#1. Download the data set from 
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#2. unzip and merge the test and training datasets
#3. extract only the mean and standard Deviation measures
#4. label set with descriptive names
#5. create a tidy data set with activity and and average of each variable
#
############################################


if(!dir.exists('./data')){dir.create('./data')}
fileURL1 <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if(!file.exists('./data/projectFiles.zip')){download.file(fileURL1, destfile = './data/projectFiles.zip', mode="wb", quiet = F)}
parentdir <- getwd()
setwd('./data')
unzip("projectFiles.zip")
setwd(parentdir)
# clean up workspacerm(list=ls())
rm(list=ls())

# step 1 Merging of training and test sets
features <- read.table('./data/UCI HAR Dataset/features.txt', header = F)
activities <- read.table('./data/UCI HAR Dataset/activity_labels.txt', header = F)
subjectTrain <- read.table('./data/UCI HAR Dataset/train/subject_train.txt', header = F)
xTrain <- read.table('./data/UCI HAR Dataset/train/X_train.txt', header = F)
yTrain <- read.table('./data/UCI HAR Dataset/train/y_train.txt', header = F)

# give name to the columns in the tables
colnames(activities)=c('activityId', 'activityType')
colnames(subjectTrain)="subjectId"
colnames(xTrain)=features[,2]
colnames(yTrain)="activityId"

# create training set
trainingData <- cbind(yTrain, subjectTrain, xTrain)

# read Test data
subjectTest <- read.table('./data/UCI HAR Dataset/test/subject_test.txt', header = F)
xTest <- read.table('./data/UCI HAR Dataset/test/X_test.txt', header = F)
yTest <- read.table('./data/UCI HAR Dataset/test/y_test.txt', header = F)

# give name to the columns in the tables
colnames(subjectTest)="subjectId"
colnames(xTest)=features[,2]
colnames(yTest)="activityId"

# create testing set
testingData <- cbind(yTest, subjectTest, xTest)

# combine training and testing
combinedData <- rbind(trainingData, testingData)

# get ony the mean and standard deviation columns
colName <-colnames(combinedData)

# find only activity, subject, mean, and standard deviation columns in data set.
logicalVector <- (grepl("activity..",colName) | grepl("subject..",colName) | grepl(".*mean.*|.*STD.*",colName))
finalData = combinedData[logicalVector==TRUE]
finalData <- merge(finalData, activities, by='activityId', all.x = TRUE)

# sub the name to make readable using regex
colNames <- colnames(finalData)

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([G]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[G]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(finalData) <- colNames

# Create a new table, finalData2 without activity column
finalData2  <- finalData[,names(finalData) != 'activityType']

tidyData    = aggregate(finalData2[,names(finalData2) != c('activityId','subjectId')],by=list(activityId=finalData2$activityId,subjectId = finalData2$subjectId),mean)
# add activity descriptionwrite.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')

tidyData    = merge(tidyData,activities,by='activityId',all.x=TRUE)

# Write out  the tidyData set
write.table(tidyData, './data/tidyData.txt',row.names=TRUE,sep='\t')

