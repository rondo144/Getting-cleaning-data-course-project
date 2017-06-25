
## Coursera Getting and Cleaning Data Course Project
## Roi Maman
## 24/6/2017

# 1. Merge the training and the test sets to create one data set

rm(list=ls()) #clean environment

setwd("C:/data science coursera/Getting and cleaning data/course proect") #set workin directory

#import data from txt files
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

#Givs columuns lables to data that been imported
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"

#merge the training data 
trainingData = cbind(yTrain,subjectTrain,xTrain)

# Read  the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE)
xTest       = read.table('./test/x_test.txt',header=FALSE)
yTest       = read.table('./test/y_test.txt',header=FALSE)

# label columuns names to thetest datat files
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"

# Merge all training data
testData = cbind(yTest,subjectTest,xTest)

#merge the test data and the training data and create final data set
finalData = rbind(trainingData,testData)

#Creat a vector for colmns names in the final data
colNames  = colnames(finalData)

#creat logical vector in order to extract only mean and std dev colums

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

#keep just the desired columns in final data
finalData = finalData[logicalVector==TRUE]

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData)


# Appropriately label the data set with descriptive activity names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
colnames(finalData) = colNames;

#Create a second, independent tidy data set with the average of each variable for each activity and each subject
#creat another data file witout activitytype
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

#summarize the noactivity datafile to get the mean
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

#merge the dataset
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE)
#creat tidydata file
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')

