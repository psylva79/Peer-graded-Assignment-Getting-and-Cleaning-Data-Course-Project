# Import the tidyverse

library(tidyverse)

# import data: X_train and X_test (contains the explanatory variables), Y_train and Y_test (contains activity), subject_train and subject_test (contains subject), activity_labels, and features.
# read_table() from the readr package is used to import datasets 
X_train <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", 
                      col_names = FALSE)

Y_train <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt", 
                      col_names = "activity")

subject_train <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", 
                            col_names = "subject")


features <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",                        col_names = FALSE)

activity_labels <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", 
                              col_names = FALSE)



X_test <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", 
                     col_names = FALSE)

Y_test <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt", 
                     col_names = "activity")

subject_test <- read_table("C:/Users/pmps/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", 
                           col_names = "subject")

# Change column names of X_train and X_test with features vector
colnames(X_train) <- features$X2
names(X_test) <- features$X2

##########################################################################################################
# concatenate train and test subsets
X <- X_train %>% bind_rows(X_test, .id = "id") 
Y <- Y_train %>% bind_rows(Y_test)
subject <- subject_train %>% bind_rows(subject_test) 

# select columns with "mean()" & "std()" in colnames. Here I use REGEX expression because "()" needs special syntax
# anyof() inside select to filter columns
relevant_cols <- grep(pattern = "(mean|std)\\(\\)", features$X2, value = TRUE)
X <- X %>% select(any_of(relevant_cols))

# merge activity, subject and X. I use bind_cols() from dplyr, but rbind() or add_columns() would also work
YX <- bind_cols(activity = Y$activity, subject = subject$subject) %>% 
  bind_cols(X)

# change activity in the dataframe. Here, I use inner_join to obtain activity labels. One alternative would be to pass the labels inside the factor() function, with labels equal to activity_labels$X2: factor(activity_labels, levels = activity_labels$X1, labels = activity_labels$X2)
YX <- YX  %>% 
  inner_join(activity_labels, by = c("activity" = "X1")) %>% 
  relocate(X2, .before = 1) %>%
  select(-activity) %>% 
  rename(activity = X2)  %>% 
  mutate_at(activity:subject, as.factor)

# transform to long format. In my opinion, it is preferable to work with long format with this data. One observation per row
YX_long <-  YX %>% 
  pivot_longer(3:68, names_to = c("type", "stat", "xyz"), names_sep = "-", names_repair = "minimal")

# Here I change the type variable (previously in column names)
YX_long <- YX_long  %>% 
  mutate(stat = gsub("\\(\\)", "", stat),         
         type = gsub("Acc", " Accelerometer ", type),
         type = gsub("Gyro", " Gyroscope ", type),
         type = gsub("BodyBody", "Body ", type),
         type = gsub("Mag", "Magnitude ", type),
         type = gsub("^t", "Time ", type),
         type = gsub("^f", "Frequency ", type),
         type = gsub("angle", "Angle", type),
         type = gsub("gravity", "Gravity", type)
  )


write.table(YX_long, "YX_long.txt", row.name=FALSE)

# Lastly I use summarize to obtain grouped statistics
finaldf <- YX_long %>% 
group_by(subject, activity, type, stat, xyz) %>%
  summarise(mean =mean(value, na.rm =TRUE))
write.table(finaldf, "FinalTidyData.txt", row.name=FALSE)

