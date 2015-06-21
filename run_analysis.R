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
