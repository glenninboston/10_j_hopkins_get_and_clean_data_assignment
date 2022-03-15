---
title: "Codebook"
author: "Glenn A"
date: "3/9/2022"
output: html_document
---


### Codebook for "Getting & Cleaning Data", Week 4 Assignment
_Note: All measures represent a mean for each subject x variable, so are the mean of the mean or mean of the standard deviation (e.g. as labelled, "MeanOf")_  

```{r, echo = FALSE, message = FALSE}
# load tidyverse
library(tidyverse)

# load file into environment
p1_fitness_sum_wide <- readr::read_rds("01 Data/p1_fitness_sum_wide.rds")


# Create a table for Codebook
code_book <- as_tibble(names(p1_fitness_sum_wide)) %>%
  rename_with(.cols = 1, ~"variable") %>%
  mutate(n = if_else(grepl("subject", variable), as.numeric(n_distinct(p1_fitness_sum_wide$subject_id)),
             if_else(grepl("activity", variable), as.numeric(n_distinct(p1_fitness_sum_wide$activity)),
                           NA_real_)),
         measurement = if_else(grepl("subject", variable), NA_character_,
                       if_else(grepl("activity", variable), NA_character_,
                       if_else(grepl("Accelerometer", variable), "Acceleration [estimated body]",
                       if_else(grepl("Gyroscope", variable), "Velocity [angular]", 
                               NA_character_)))),
         measure_instrument = if_else(grepl("subject", variable), NA_character_,
                              if_else(grepl("activity", variable), NA_character_,
                              if_else(grepl("Accelerometer", variable), "Accelerometer",
                              if_else(grepl("Gyroscope", variable), "Gyroscope", 
                                      NA_character_)))),
         type_of_measure = if_else(grepl("subject", variable), NA_character_,
                           if_else(grepl("activity", variable), NA_character_,
                           if_else(grepl("time", variable), "Actual (in time)",
                           if_else(grepl("freq", variable), "Frequency", 
                                   NA_character_)))),
         unit_of_measure = if_else(grepl("subject", variable), NA_character_,
                           if_else(grepl("activity", variable), NA_character_,
                           if_else(grepl("Accelerometer", variable), "g's (gravity of earth -> 9.80665 meters per second squared)",
                            if_else(grepl("Gyroscope", variable), "radians per second", 
                                    NA_character_)))),
         summary_measure = if_else(grepl("subject", variable), NA_character_,
                           if_else(grepl("activity", variable), NA_character_,
                           if_else(grepl("Stddev", variable), "Standard Deviation", 
                           if_else(grepl("Mean", variable) & grepl("MeanOf", variable), "Mean",
                                   NA_character_)))),
         value_range = if_else(grepl("subject", variable), "1-30", 
                        if_else(grepl("activity", variable), "1 - Walking, 2 - Walking Upstairs, 3 - Walking Downstairs, 4 - Standing, 5 - Sitting, 6 - Laying Down",
                                "Normalized between -1 - 1")),
         notes = "All measures represent a mean for each subject x variable, so are the mean of the mean, or mean of the standard deviation") 


# save rds file for table
write_rds(code_book, "01 Data/code_book.rds")


# read code book into the environment
my_code_book <- code_book %>% select(-notes)

# print as table
knitr::kable(my_code_book)
```  








