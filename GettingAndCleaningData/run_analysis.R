##This R program is part of Getting and Cleaning Data - Course Project
##Get the Data for the project from the following location
##https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
library(RCurl)
library(data.table)

##Download and unzip the dataset
if(!file.exists("UCI HAR Dataset")){
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl,dest="getdata_projectfiles_UCIDataset.zip",method="curl")
  unzip("getdata_projectfiles_UCI HAR Dataset.zip")
}

##Read Train Data
X.train <- read.table("UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")

##Read Test Data
X.test <- read.table("UCI HAR Dataset/test/X_test.txt")
y.test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")

#Read Data from Features.txt then label the data
features <- read.table("UCI HAR Dataset/features.txt")

##Label the Features data
names(features)<-c("FeaturesLabel","FeaturesName")

##Read Activity Data then Label the columns
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors=F)
names(activityLabels)<-c("ActivityLabel", "ActivityName")

##Label Train Data
names(y.train) <- "Label"
names(X.train) <- features$FeaturesName
names(subject.train) <- "Subject"

##Label Test Data
names(y.test) <- "Label"
names(X.test) <- features$FeaturesName
names(subject.test) <- "Subject"

##Bind all Train data with subject and appropreate Label
Final_TrainingSet <- cbind(y.train,X.train)
Final_TrainingSet <- cbind(subject.train,Final_TrainingSet)

#Bind Test Data with Subject and Appropreate Label
Final_TestSet <- cbind(y.test,X.test)
Final_TestSet <- cbind(subject.test,Final_TestSet)

##Q1.Merges the training and the test sets to create one data set
traingandtest_dataset<-rbind(Final_TrainingSet,Final_TestSet)

##Q2.Extracts only the measurements on the mean and standard deviation for each measurement. 
stdandmean_dataset <- traingandtest_dataset[,grepl('Subject|Label|mean\\(\\)|std\\(\\)',colnames(traingandtest_dataset))]

##Q3.Uses descriptive activity names to name the activities in the data set
#Replace the code with corresponding Description
stdandmean_dataset$Label <- factor(stdandmean_dataset$Label,labels=activityLabels$V2)
#Rename the Column Name Label to ActivityName
setnames(stdandmean_dataset, "Label", "ActivityName")

##Q4.Appropriately labels the data set with descriptive variable names.
#Remove all special characters from the header attributes. Make more descriptive
names(stdandmean_dataset)<-gsub("-","",names(stdandmean_dataset))
names(stdandmean_dataset)<-gsub("\\(","",names(stdandmean_dataset))
names(stdandmean_dataset)<-gsub("\\)","",names(stdandmean_dataset))
names(stdandmean_dataset)<-gsub("^t","Time",names(stdandmean_dataset))
names(stdandmean_dataset)<-gsub("^f","Freqncy",names(stdandmean_dataset))
names(stdandmean_dataset)<-gsub("std","STD",names(stdandmean_dataset))

##5.From the data set in step 4, creates a second, independent tidy data 
##set with the average of each variable for each activity and each subject.
attach(stdandmean_dataset)
aggdata <-aggregate(stdandmean_dataset, by=list(ActivityNameGroup=ActivityName,SubjectGroup=Subject),FUN=mean, na.rm=FALSE)
aggdata$Activity=NULL
aggdata$ActivityName=NULL
aggdata$Subject=NULL
detach(stdandmean_dataset)

##Write the output tidy data to csv files 
write.csv(aggdata, file = "UCI_HAR_Tidy_Dataset.csv", row.names = FALSE)
