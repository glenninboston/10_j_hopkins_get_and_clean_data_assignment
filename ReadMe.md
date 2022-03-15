---
title: "Read Me"
author: "Glenn A"
date: "3/9/2022"
output: html_document
---

This dataset was provided courtesy of:
[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on 
Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted 
Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012


# Getting & Cleaning Data, Week 4 Assignment
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set following tidy principles. For this purpose, we will leverage learnings from our lectures related to importing, cleaning and manipulating data. The tidyverse library will provide the bulk of our functions.

Remember from our Week 1 lecture their are four things you should have at the end "tidying" your data: (1) raw data; (2) a tidy dataset; (3) a code book describing each variable and its values in the tidy data set (metadata); and (4) an explicit and exact recipe that you used

>**Tidy Data:**
>
>1. Each variable you measure should be in one column
>2. Each different observation of that variable should be in a different row
>3. There should be one table for each kind of variable
>4. If you have multiple tables, they should include a column in the table that allows them to be linked (a key)


>**Review criteria:**
>
>1. The submitted data set is tidy. 
>2. The Github repo contains the required scripts.
>3. GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, >and any other relevant information.
>4. The README that explains the analysis files is clear and understandable.
>5.  The work submitted for this project is the work of the student who submitted it.



The following steps were provided as guidance from the assignment.

>**Create one R script called run_analysis.R that does the following:** 
>
>1. Merges the training and the test sets to create one data set.
>2. Extracts only the measurements on the mean and standard deviation for each measurement. 
>3. Uses descriptive activity names to name the activities in the data set
>4. Appropriately labels the data set with descriptive variable names. 
>5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


This work was aligned to these steps. Though our first step was to review the assignment and the associated datasets.





## STEP 0: Project background, import data, explore datasets 
Load the tidyverse set of packages for our tidying exercise

``` {r, message = FALSE}
# load the tidyverse
library(tidyverse)
```



#### [0] BACKGROUND
Data is from UCI.edu "Human Activity Recognition Using Smartphone Data Set".

Goals of Work:

1. Create a tidy dataset with summary means by Subject and Activity
2. Provide a link to the Github repository with the script for this analysis
3. Provide a corresponding README file and Codebook



#### [1] IMPORT DATA

Specify path to files you want to import
                                      
```{r}
# create a list of all the available files in UCI HAR Dataset (including subfolders)
files_list_full <- list.files("01 Data/UCI HAR Dataset", recursive = TRUE, full.names = TRUE)  
```


Create a list of cleaned-up names (we will use the "r" prefix to denote raw data, p# for processed data)

```{r}
files_list_names <- paste0("r_",gsub("^01_data_uci_har_dataset_|_test_txt|_train_txt|_txt", "", stringr::str_to_lower(gsub("[.]|[/]|[ ]", "_", files_list_full)))) 
files_list_names
```


Now we can import all the files in the directory to a list of dataframes, then unnest them into the environment

```{r, message = FALSE, warning = FALSE}
# import all files in the directory to a list of dataframes
my_list_dfs <- files_list_full %>%
  purrr::map(read_table, col_names = FALSE) %>%
  purrr::set_names(files_list_names)

# unnest dataframes from list of dataframes creating each one in the environment
base::invisible(base::list2env(my_list_dfs, .GlobalEnv))
```


To make our analysis easier, and do some memory hygiene, we will remove all unnecessary files from the environment

```{r, message = FALSE}
# remove unneeded files (based on manual review of .txt files)
rm(my_list_dfs, r_readme, r_features_info)


## Given mentor comments to see David Hood's overview, the intertial data is not used per that presentation
# Clean-Up Environment  rm inertial data
rm(list = ls(pattern = "inertial"))
rm(list = ls(pattern = "files_list_"))
```



#### [2] EXPLORE DATA
Evaluate the structure (str/skim) of the imported tables; check dim size between files; bring in file descriptions from README.txt; use unique() to explore unique elements of each variable in each table

``` {r, eval = FALSE}
# ADMIN FILES
str(r_activity_labels); skimr::skim(r_activity_labels)    # [6x2]      Links the class labels with their activity names (These are the six activities), e.g. 1(WALKING)
str(r_features); skimr::skim(r_features)                  # [561x2]    List of all features (These appear the column id and associated names) e.g. 1=tBodyAcc-mean()-X

# TEST SET FILES
str(r_test_x); skimr::skim(r_test_x)                      # [2947x561] Training set (This is your measurement data)
str(r_test_y); skimr::skim(r_test_y)                      # [2947x1]   Training labels (These appear the activity being measured (see Activity "Class" Labels))
str(r_test_subject); skimr::skim(r_test_subject)          # [2947x1]   Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. (These appear the subject ID)

# TRAINING SET FILES
str(r_train_x); skimr::skim(r_train_x)                    # [7352x561] Test set (This is your measurement data)
str(r_train_y); skimr::skim(r_train_y)                    # [7352x1]   Testing labels (These appear the activity being measured (see Activity "Class" Labels))
str(r_train_subject); skimr::skim(r_train_subject)        # [7352x1]   Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. (These appear the subject ID)
```


From here we can conclude several things that will help us combine this dataset:

- activity_labels - six activity labels that correspond to test_y (1-6)
- features - 561 features, correspond to columns in test_x, train_x
- test_subject and train_subject is the subject ID (1-30)
- test_y and train_y are the activity (1-6)
- test_x and train_x are your measurements





## STEP 1. Merges the training and the test sets to create one data set 


#### [1] Prepare raw (r) tables for combination (need to update the column names to avoid duplicates, e.g. X1, X2...)

``` {r}
# create an vector of name values from the features table
r_names <- unlist(as.list(r_features[2]))

# udpate column names
colnames(r_test_x) <- r_names
colnames(r_train_x) <- r_names

colnames(r_test_y) <- "activity_id"
colnames(r_train_y) <- "activity_id"

colnames(r_test_subject) <- "subject_id"
colnames(r_train_subject) <- "subject_id"
```


#### [2] Combine files into complete test and training sets
```{r}
# create combined test and train tables
r_test_cmb <- cbind(r_test_subject, 
                    r_test_y,
                    r_test_x)

r_train_cmb <- cbind(r_train_subject, 
                     r_train_y,
                     r_train_x)
```


#### [3] Combine test and training datasets, convert subject ID to factor, ordered 1-30
```{r}
# now we can combine the test and training sets together to re-form the complete dataset
r_fit_monitor_data_cmb <- rbind(r_test_cmb, r_train_cmb)
```


#### [4] Save file as rds (this is your complete raw dataset)
Though a tidy set of the raw data is not requested of the assignment, it is consistent with the principles of tidying data taught in the class
```{r}
# save the "tidy" raw data 
write_rds(r_fit_monitor_data_cmb, "01 Data/r_fit_monitor_data_cmb.rds")
```


#### [5] Create map of activity names with factor for later
```{r}
# this "activity map" will allow us to create the requested labels for our activity field
m_activity_labels <- r_activity_labels %>%
  mutate(activity = factor(X1, labels = stringr::str_to_title(gsub("[_]", " ", X2)))) %>%
  select(activity_id = X1, activity)

# check factor (maintains order)
unclass(m_activity_labels$activity)
```



```{r}
# Clean-Up Environment
rm(list = ls(pattern = "r_"))
```





## STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement 

#### [1] Select only fields that capture either mean or standard deviation
```{r}
p1_fitness_cmd_v1 <- read_rds("01 Data/r_fit_monitor_data_cmb.rds") %>%
  select(subject_id, activity_id, contains(c("mean", "std")), -contains("meanFreq"), -contains("angle")) 
```




## STEP 3. Uses descriptive activity names to name the activities in the data set 

#### [1] use a join to attach our activity map & also convert non-numeric fields to factors 
```{r}
p1_fitness_cmd_v1 <- p1_fitness_cmd_v1 %>%
  # map your activity vector to factorize activity_id, giving descriptive activity names (labels)
  left_join(m_activity_labels, by = c("activity_id")) %>%
  # factorize subject_id
  mutate(subject_id = as.factor(subject_id)) %>%
  # select and order df
  select(subject_id, activity, contains(c("mean", "std")))
```


```{r}
# Clean-Up Environment
rm(list = ls(pattern = "m_"))
```





## STEP 4. Appropriately labels the data set with descriptive variable names 

#### [1] Rename Variables to be more decriptive
- t = time
- f = frequency (of domain signals) Fast Fournier Transform (FFT) was applied

- Body = body acceleration signals
- Gravity = gravity acceleration signals

- Acc = accelerometer
- Gyro =  gyroscope

- Jerk = signals (derived from body linear acceleration and angular velocity)
- Mag = magnitude (of these 3d signals were calculated using the Euclidean norm)

- X, Y, Z = 3 axial raw signals in the X, Y and Z directions (constant 50Hz)

- mean = mean 
- std = standard deviation

NOTE on column names, while the course recommends not using underscores in column names, this is not generally accepted practice. I've been a heavy tidyverse user for a long time. Hadley Wickham himself uses underscores. See examples on the tidyverse.org website: https://dplyr.tidyverse.org/

```{r}
p1_fitness_cmd <- p1_fitness_cmd_v1 %>%
  # clean-up the variable names
  # pivot_longer(cols = where(is.numeric), names_to = "temp", values_to = "value") %>%
  # separate(col = "temp", into = c("metric", "measure", "number"), sep = "-", extra = "warn", remove = FALSE)%>%
  rename_with(stringr::str_replace_all, pattern = "^t", replacement = "time_") %>%
  rename_with(stringr::str_replace_all, pattern = "^f", replacement = "freq_") %>%
  rename_with(stringr::str_replace_all, pattern = "Acc", replacement = "Acceleration") %>%
  rename_with(stringr::str_replace_all, pattern = "Gyro", replacement = "Velocity") %>%
  rename_with(stringr::str_replace_all, pattern = "-mean[(][)]", replacement = "_Mean") %>%
  rename_with(stringr::str_replace_all, pattern = "-std[(][)]", replacement = "_Stddev") %>%
  rename_with(stringr::str_replace_all, pattern = "-", replacement = "_Angle") %>%
  rename_with(stringr::str_replace_all, pattern = "Mag", replacement = "Magnitude")

write_rds(p1_fitness_cmd, "01 Data/p1_fit_monitor_data_cmb.rds")
```



```{r}
# Clean-Up Environment
rm(list = ls(pattern = "p1_"))
```





## STEP 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

As a note, there was some ambiguity on what was a unique activity x subject, for the purposes of this exercise, we assumed that every measurement by angles X, Y and Z were distinct, thus the mean was taken for each of these (rather than aggregating them into a singular mean).
```{r}
# take the mean of each variable by activity and subject
p1_fitness_sum <- read_rds("01 Data/p1_fit_monitor_data_cmb.rds") %>%
  pivot_longer(cols = where(is.numeric), names_to = "measure", values_to = "value") %>%
  # summarise and take the mean by subject x activity
  group_by(subject_id, activity, measure) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  # make sure variable name captures that this is a mean of a mean or mean of a std dv
  mutate(measure = paste0("MeanOf_",measure)) 

# save file as rds
write_rds(p1_fitness_sum, "01 Data/p1_fitness_summary.rds")

# create a wide version of the data
p1_fitness_sum_wide <- p1_fitness_sum %>%
  pivot_wider(names_from = "measure", values_from = "value") %>%
  # use select to provide a reasonable order to the columns that aggregate similar measures
  select(subject_id, activity,
         contains("_time_"),
         contains("_Mean_"),
         contains("_Stddev"))

# save as rds for later use
write_rds(p1_fitness_sum_wide, "01 Data/p1_fitness_sum_wide.rds")


# Clean-Up Environment
rm(list = ls())
```

