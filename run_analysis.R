run_analysis<-function(){
#1-Merges the training and the test sets to create one data set.        
        #Read X_train.txt and X_test.txt, both datasets for train and test experiences.
        #Merge training and test data into single data frame
        Data_train <- read.table("./Dataset/train/X_train.txt")
        Data_test <- read.table("./Dataset/test/X_test.txt")
        dataset <- rbind(Data_train,Data_test)

#2-Extracts only the measurements on the mean and standard deviation for each measurement.         
        #Read features.txt, this list of features is the labels names of the dataset columns
        list_features <- read.table("./Dataset/features.txt", col.names=c("index_features", "labels_features"))
        #vector character containing labels from list_features data frame
        labels_features <- list_features$labels_features
                 ##logical vector for select features have mean or std in their name
                meansd_features <- grepl('mean\\(\\)|std\\(\\)',labels_features)
                 #Create a character vector of only features with mean and standard deviation in their name
                 list_feature_meansd <- as.character(labels_features[meansd_features])
                        #Rename columns in dataset, names match row list_features/col dataset 
                         colnames(dataset) <- labels_features
                        ##select datasubstet with features content mean and std, from dataset with the logical vector subset_features.
                        datasubset <- dataset[,meansd_features]
        
#3- Uses descriptive activity names to name the activities in the data set
        #Read y_train.txt and y_test.txt, both datasets for train and test index activities.
        #Merge training and test data into single data frame (in order first train activities, second test activities)
        #rename column to "IndexActivity"
        train_activities <- read.table("./Dataset/train/Y_train.txt", col.names=c("IndexActivity"))
        test_activities <- read.table("./Dataset/test/Y_test.txt", col.names=c("IndexActivity"))
        activities <- rbind(train_activities,test_activities)
                ## activity values with descriptive names using the activity_labels.txt file 
                activity_labels<-read.table("./Dataset/activity_labels.txt",col.names=c("IndexActivity","Activity"))
                library(dplyr)
                activities<-left_join(activities,activity_labels,by="IndexActivity")
                #Drop activity index
                activities$IndexActivity <- NULL
        
#4- Appropriately labels the data set with descriptive variable names. 
        #Read subjects ids for train and test Dataset
        #and merge into a single col (in order first train subjects, second test subjects)
        subject_train <- read.table("./Dataset/train/subject_train.txt", col.names=c("Subject"))
        subject_test <- read.table("./Dataset/test/subject_test.txt", col.names=c("Subject"))
        subject <- rbind(subject_train, subject_test)
        
        # Also we have Activity and all features which content mean or sd
        
        
#5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        #Merge activities, Subjects, Features of measures, and data, all into one data frame
        TidyDataSet <- cbind(subject, activities, datasubset)
        library(reshape2)
        #Melt data frame for reshaping
        TidyDataSet <- melt(TidyDataSet, id=c("Subject", "Activity"), measure.vars=list_feature_meansd)
        #tidy data frame by mean using the reshape2 package
        TidyDataSet <- dcast(TidyDataSet, Subject + Activity ~ variable, mean)
        #Order by Subject And Activity
        TidyDataSet <- TidyDataSet[order(TidyDataSet$Subject, TidyDataSet$Activity),]
        #Output file create a txt file with write.table()
        write.table(TidyDataSet,file="tidy_dataset.txt",row.name = FALSE) 
}