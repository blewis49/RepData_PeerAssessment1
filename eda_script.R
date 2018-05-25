install.packages("xtable")
library(ggplot2)
library(dplyr)
library(xtable)

activity <- read.csv("activity.csv")
dup_activity <- activity
str(activity)
head(activity)
# change the date from a factor to a date format 
activity$date <- as.Date(activity$date, "%Y-%m-%d")

# a histogram of the total steps taken each day
ggplot(data = activity) +
      geom_histogram(mapping = aes(x = date, y = steps), stat = "identity", fill = "blue")

# calculation of mean and median steps taken each day
activity %>% 
      group_by(date) %>% 
      summarise(avg_steps = mean(steps, na.rm = T),
                med_steps = median(steps, na.rm = T)) %>% 
      ungroup()

activity %>% 
      group_by(date) %>% 
      summarise(numdays = n()) %>% 
      ungroup()

# 288 5-minute intervals each day for 61 days
# 12 5-minute intervals each hour for 24 hours = 288 intervals / day

# Time-Series plot of the 5-min intervals on average number of steps taken averaged across all days
activity %>% 
      group_by(interval) %>% 
      summarise(avg_steps = mean(steps, na.rm = T)) %>% 
      ungroup() %>% 
      ggplot() +
      geom_line(aes(x=interval, y = avg_steps))

# calculate the total number of rows with missing data (i.e. NAs)
sum(is.na(activity)) # 2304 rows with missing days (8 x 288 intervals)

# identify which days or intervals have missing values
activity %>% 
      group_by(date) %>% 
      summarise(blank_ints = sum(is.na(steps))) %>% 
      filter(blank_ints > 0) %>% 
      ungroup()

activity[!complete.cases(activity),]
activity[is.na(activity$steps), "interval"] 

# average and max number of steps across each interval
int_sum <- activity %>% 
      group_by(interval) %>% 
      summarise(avg_int = mean(steps, na.rm = T),
                max_int = max(steps, na.rm = T)) %>% 
      ungroup()

tapply(activity$steps, activity$interval, mean, na.rm = T)

impute_values <- rep(int_sum$avg_int,8)
activity[is.na(activity$steps), "steps"] <- impute_values

# look at data with imputed values
ggplot(data = activity) +
      geom_histogram(mapping = aes(x = date, y = steps), stat = "identity", fill = "blue")
# calculation of mean and median steps taken each day
activity %>% 
      group_by(date) %>% 
      summarise(avg_steps = mean(steps, na.rm = T),
                med_steps = median(steps, na.rm = T)) %>% 
      ungroup()

# create a variable "day" to be either weekday or weekend
activity$day <- "weekday"
activity[weekdays(activity$date) %in% c("Saturday","Sunday"), "day"] <- "weekend"

# Time-Series plot of the 5-min intervals on average number of steps taken averaged across all weekdays and weekends
#set up the 2x1 frame for plots
activity %>% 
      group_by(interval, day) %>% 
      summarise(avg_steps = mean(steps, na.rm = T)) %>% 
      ungroup() %>% 
      ggplot() +
      geom_line(mapping = aes(x = interval, y = avg_steps), color = "blue") + 
      facet_wrap(~ day, nrow = 2) +
      ylab("Average Number of Steps")

