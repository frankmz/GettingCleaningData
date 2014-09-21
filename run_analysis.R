
# TASK: You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
#    measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

        
# read in test files
XTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
subjTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# read in training files
XTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
subjTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# read in features and activity labels
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# extract feature names as character vector for column labeling of data frame
featureNames <- as.character(features$V2)

# extract only mean() and std() features
mean_logic <- grepl("mean\\(\\)", featureNames, perl=TRUE)
std_logic <- grepl("std\\(\\)", featureNames, perl=TRUE)
mean_std_indices <- which(mean_logic | std_logic)

XTestSel <- XTest[,mean_std_indices]
XTrainSel <- XTrain[,mean_std_indices]
featureNamesSel <- featureNames[mean_std_indices]

# form data frames for test and training; 
# include column for differentiating test and training in combined data frame;
# include NA column to be later filled with activity labels
TestData <- data.frame(subjTest, TRUE, yTest, NA, XTestSel)
TrainData <- data.frame(subjTrain, FALSE, yTrain, NA, XTrainSel)

# set column names
colnames(TestData) <- c("Subject", "Test", "Label", "Activity", featureNamesSel)
colnames(TrainData) <- c("Subject", "Test", "Label", "Activity", featureNamesSel)

# combine data frames
Data <- rbind(TrainData, TestData)

# add activity names
Data$Activity <- activity_labels$V2[Data$Label]


### create second data set with the average of each variable for each activity and each subject

# split Data into Subject and Activity
Data_split <- split(Data, list(Data$Subject,Data$Activity))

# claculate average of measurement variables
Data2 <- t(sapply(Data_split, function(x) {colMeans(x[,c(5:70)], na.rm = TRUE)}))

# prepare Subject and Activity information for tidy data set
Data2pre <- data.frame(Subject=rep(1:30, times=length(levels(Data$Activity))), 
                       Activity=rep(levels(Data$Activity), each=30))

# combine to final data set
Data_tidy = cbind(Data2pre, Data2)

# write data set to file
write.table(Data_tidy, "./Data_tidy.txt", row.name=FALSE)


