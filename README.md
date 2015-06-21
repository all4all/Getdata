# Getdata
Course Project for Coursera

##Data Getting and Cleaning Course Project##

###run_analysis.R###

Here's an easy way to check it works:

data <- read.table("https://s3.amazonaws.com/coursera-uploads/user-797870df688c825cf3eff33b/973502/asst-3/90bfe1e017b211e58f8bf149448c0d67.txt", header = TRUE)

The goal of the Course Project was to prepare tidy data that can be used for later analysis. 
The R script called run_analysis.R does the following: 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Here goes the script with the comments.

library(dplyr)
library(tidyr)

features <- read.table("./UCI HAR Dataset/features.txt") ## reads in the
##names of our columns
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", colClasses = "numeric")  ## reads
##in the X_train data
colnames(X_train) <- features[,2] ## asigns names to columns of the X_train
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt") ## reads in Y_train data
colnames(Y_train) <- "activity" ## assigns names to the only column
sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt") ## reads in subject 
##column
colnames(sub_train) <- "subject" ## assigns names to the only column
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", colClasses = "numeric")  ## reads in
##the X_test data
colnames(X_test) <- features[,2] ## asigns names to columns of the X_test
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt") ## reads in Y_test data
colnames(Y_test) <- "activity" ## assigns names to the only column
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt") ## reads in subject column
colnames(sub_test) <- "subject" ## assigns names to the only column

dat_train <- cbind.data.frame(sub_train, Y_train, X_train) ##gathers all 'train' data 
##together
dat_test <- cbind.data.frame(sub_test, Y_test, X_test) ##puts all 'test' data together
DS1 <-  tbl_df(rbind(dat_train, dat_test)) ##makes the first raw dataset

ADS <-DS1[grep("mean()|std()", names(DS1),fixed = F)] %>%
  select(-contains("Freq")) ##gets rid of unnecessary columns
  
ADS <- cbind(DS1[,1:2], ADS) ## gives back the subject and activity variables
## to substitute digits with proper lables we read in a list of activity 'names'
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
## then bring them to proper state, i.e. to lower case
activity_labels[,2] <- tolower(activity_labels[,2])
## put labels to their place (the "activity" column)
ADS[ ,2] <- as.character(factor(ADS[ ,2], labels = activity_labels[,2]))
## adjust variables to requirements (omit some improper signs and bring to lower case)
colnames(ADS) <- make.names(colnames(ADS))
## forming the new independent tidy data set with the average of each variable for each 
## activity and each subjec
TidyData <- ADS %>% group_by(subject, activity) %>% summarise_each(funs(mean))
## saving the result to a txt file
write.table(TidyData, file="GetdataCourseProject.txt", row.names = FALSE)
## tidying the room
rm(activity_labels, ADS, dat_test, dat_train, DS1, features, sub_train, sub_test, 
   X_test, X_train, Y_test, Y_train)
## telling the user where to look for the result
print("GetdataCourseProject.txt file is ready in your workind directory")


###Raw Data###

The Course Project assignment is based on the real dataset of Human Activity Recognition Using Smartphones. More details can be obtained from the READMe.txt file in the Dataset: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, the researchers captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The features, except the first two, selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ given in the Course Project assignment under the link:  These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-xyz and tBodyGyroJerk-xyz). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation

The full list of variables is available in the CodeBook.md


========
For more information about this dataset contact: activityrecognition@smartlab.ws
========
License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
