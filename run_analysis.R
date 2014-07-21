run_analysis <- function(){

  library(reshape2)
  
  ## load data from files
  data_dir <- "./data/UCI HAR Dataset/"
  activity_labels <- read.table(paste0(data_dir, "activity_labels.txt"))
  features <- read.table(paste0(data_dir, "features.txt"))
  x_train <- read.table(paste0(data_dir, "train/X_train.txt"))
  y_train <- read.table(paste0(data_dir, "train/y_train.txt"))
  subject_train <- read.table(paste0(data_dir, "train/subject_train.txt"))
  x_test <- read.table(paste0(data_dir, "test/X_test.txt"))
  y_test <- read.table(paste0(data_dir, "test/y_test.txt"))
  subject_test <- read.table(paste0(data_dir, "test/subject_test.txt"))
  
  
  
  ## 1. Merges the training and the test sets to create one data set.
  x <- rbind(x_train, x_test)
  y <- rbind(y_train, y_test)
  subject <- rbind(subject_train, subject_test)
  
  ## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  names(x) <- features$V2
  colnamestokeep <- grep(".*mean\\(\\)|.*std\\(\\)", features$V2, ignore.case = TRUE, value = TRUE)
  #note that grep("mean|std"..) also extract other measure beyond mean and std, for instance angle(X,gravityMean) and that's wrong  given the requiremewnts
  k <- x[colnamestokeep]
  
  ## 3. Uses descriptive activity names to name the activities in the data set
  activity <- merge(y, activity_labels)
  k <- cbind(activity = activity$V2, k)
  k <- cbind(subject = subject$V1, k)
  
  ## 4. Appropriately labels the data set with descriptive variable names. 
  ## names(k) <- sub("\\(\\)", "", names(k))
  ## already done
  
  ## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.   
  k <- melt(k, id=c("subject", "activity"), measure.vars=colnamestokeep)
  tidy <-dcast(k, subject + activity ~ variable, mean) 
  write.table(tidy, paste0(data_dir, "tidy.txt"))
}