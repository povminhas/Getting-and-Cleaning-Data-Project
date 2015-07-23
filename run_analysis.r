##You should create one R script called run_analysis.R that does the following. 
##Merges the training and the test sets to create one data set.
##Extracts only the measurements on the mean and standard deviation for each measurement. 
##Uses descriptive activity names to name the activities in the data set
##Appropriately labels the data set with descriptive variable names. 
##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#get data

	fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(fileURL,destfile="./Assignment/Assignment.zip")
	unzip(zipfile = "./Assignment/Assignment.zip",exdir="./Assignment")
	path_rf <- file.path("./Assignment" , "UCI HAR Dataset")

#load data
	test <- read.table(file.path(path_rf, "test","X_test.txt"))
	train <- read.table(file.path(path_rf, "train","X_train.txt"))
	features <- read.table(file.path(path_rf,"features.txt."))
	
#load label
	testactivity <- read.table(file.path(path_rf, "test","y_test.txt"))
	trainactivity <- read.table(file.path(path_rf, "train","y_train.txt"))

#load subject data	
	testsubject <- read.table(file.path(path_rf, "test","subject_test.txt"))
	trainsubject <- read.table(file.path(path_rf, "train","subject_train.txt"))
	
#merge datatables by rows
	dataset <- rbind(test,train)
	activity <- rbind(testactivity, trainactivity)
	subject <- rbind(testsubject,trainsubject)

#name variables
	names(subject) <- "subject"
	names(activity) <- "activity"
	names(dataset) <- features$V2

#merge all data
	alldata <- cbind(subject,activity)
	alldata <- cbind(dataset,alldata)

#extract only mean and std
	extractfeatures <- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
	selectedNames<-c(as.character(extractfeatures), "subject", "activity" )
	alldata <- alldata[,selectedNames]

#add activity labels
	activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"))
	alldata <- merge(alldata,activityLabels,by.x = "activity",by.y = "V1")
	library(dplyr)
	alldata <- rename(alldata, activityname = V2, activitynum = activity)

#label dataset with descriptive variable names
	names(alldata)<-gsub("std()", "SD", names(alldata))
	names(alldata)<-gsub("mean()", "MEAN", names(alldata))
	names(alldata)<-gsub("^t", "time", names(alldata))
	names(alldata)<-gsub("^f", "frequency", names(alldata))
	names(alldata)<-gsub("Acc", "Accelerometer", names(alldata))
	names(alldata)<-gsub("Gyro", "Gyroscope", names(alldata))
	names(alldata)<-gsub("Mag", "Magnitude", names(alldata))
	names(alldata)<-gsub("BodyBody", "Body", names(alldata))

#create second independent tidy data set
	alldata2<-aggregate(. ~subject + activityname, alldata, mean)
	alldata2<-alldata2[order(alldata2$subject,alldata2$activityname),]
	write.table(alldata2, file = "tidydata.txt",row.name=FALSE)