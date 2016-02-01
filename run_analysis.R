#############################################################################
#  GCD Project 1 - Tidy Data Preparation                     Greg Lawrence
#
#  The "UCI HAR Dataset" library is assumed to exist (already unzipped)
#  in the directory containing this run_analysis.R source code file.
#############################################################################

# 1. Merges the training and the test sets to create one data set.
# 2. Reads measurements from the test and train data sets, and merges them together.
# 3. Reads selected source data into vectors and data tables for later code use.

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
X_test  <- read.table("./UCI HAR Dataset/test/X_test.txt")
X_all <- rbind(X_train, X_test)

S_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
S_test  <- read.table("./UCI HAR Dataset/test/subject_test.txt")
S_all <- rbind(S_train, S_test)

Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
Y_test  <- read.table("./UCI HAR Dataset/test/y_test.txt")
Y_all <- rbind(Y_train, Y_test)

# 4. Updates variable names so they are more readable.
# 5. Extracts mean and standard deviation measurements.

features <- read.table("./UCI HAR Dataset/features.txt")
selected_features <- grep("-mean\\(\\)|-std\\(\\)", features[,2])
X_sel <- X_all[, selected_features]
names(X_sel) <- features[selected_features,2]
names(X_sel) <- gsub("\\(|\\)", "", names(X_sel))
names(X_sel) <- tolower(names(X_sel))

# 6. Set descriptive activity names to the measurements in the data set.

activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activities)=c("code","desc")
activities$desc = gsub("_", "", tolower(as.character(activities$desc)))
Y_all[,1] = activities$desc[Y_all[,1]]
names(Y_all) <- "activity"

# 7. Appropriately labels the data set with descriptive variable names. 

names(S_all) <- "subject"
CD <- cbind(S_all, Y_all, X_sel)   ## the raw clean data
write.table(CD, "./CleanData1.txt", row.names=FALSE, sep=",")

# 8. Summarise data by subject and activity and get average for each combination.

distinctSubjects = unique(S_all)[,1]
cntSubjects = length(distinctSubjects)
cntActivities = nrow(activities)
cntCols = dim(CD)[2]
outData = CD[1:(cntSubjects*cntActivities), ]

thisRow = 1
for (s in 1:cntSubjects) {
    for (a in 1:cntActivities) {
        outData[thisRow,1] = distinctSubjects[s]
        outData[thisRow,2] = activities$desc[a]
        work1 <- CD[CD$subject==s & CD$activity==activities$desc[a], ]
        outData[thisRow,3:cntCols] <- colMeans(work1[,3:cntCols])
        thisRow = thisRow + 1
    }
}

# 9. Outputs the summarised data set (sorted by subject) to *HAR_TidyData.txt*.

outSort <- outData[order(outData$subject),]
write.table(outSort, "./HAR_TidyData.txt", row.names=FALSE, sep=",")

### THE END