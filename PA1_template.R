
# loading required libraries
library(dplyr)
library(ggplot2)


## Loading and preprocessing the data

if (!file.exists("activity.csv")) {
  unzip(zipfile="activity.zip")
}

# read csv file
act_org <- read.csv("activity.csv")
dim(act_org)
head(act_org)

# omit records with NA
act_clean <- na.omit(act_org)
dim(act_clean)


## What is mean total number of steps taken per day?

#1. Calculate the total number of steps taken per day

# records grouped on date and total of steps calculated
act_date_summ <- act_clean %>% select(date, steps) %>% group_by(date) %>% summarize(tot_steps=sum(steps))
dim(act_date_summ)
summary(act_date_summ)
act_date_summ
    

#2. Make a histogram of the total number of steps taken each day

ggplot(act_date_summ, aes(x = tot_steps)) +
  geom_histogram(color = "white", binwidth = 2000) +
  labs(title = "Daily Steps Histogram", x = "Total Daily Steps", y = "Frequency")


#3. Calculate and report the mean and median of the total number of steps taken per day

mean(act_date_summ$tot_steps)

median(act_date_summ$tot_steps)


## What is the average daily activity pattern?

#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


act_interval_mean <- act_clean %>% select(interval, steps) %>% group_by(interval) %>% summarize(avg_steps=mean(steps))
ggplot(act_interval_mean, aes(x = interval, y = avg_steps)) + 
  geom_line(color = "blue") + 
  labs(title = "Average Daily Actvity Pattern", x = "Interval (5 mins)", y = "Average Steps")


#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


act_interval_mean[which.max(act_interval_mean$avg_steps),]



## Imputing missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


sum(is.na(act_org))


#2. Strategy for fillin gin all f the missing values in the dataset: 
#  Missing values will be replaced with mean for that 5-minute interval


repl_w_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))



#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

act_new <- act_org%>% group_by(interval) %>% mutate(steps= repl_w_mean(steps))
head(act_new)
# check for any missing value
sum(is.na(act_new))


#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


# records grouped on date and total of steps calculated
act_new_date_summ <- act_new %>% select(date, steps) %>% group_by(date) %>% summarize(tot_steps=sum(steps))
ggplot(act_new_date_summ, aes(x = tot_steps)) +
  geom_histogram(color = "white", binwidth = 2000) +
  labs(title = "Daily Steps Histogram", x = "Total Daily Steps", y = "Frequency")

# mean total number of steps taken per day.
mean(act_new_date_summ$tot_steps)

# median total number of steps taken per day.
median(act_new_date_summ$tot_steps)




#4.1 Do these values differ from the estimates from the first part of the assignment? 

#The mean and median calculated in the first part of assignment after excluding missing values differ from the mean and median calculated now after replacing missing values with mean for that 5-minute interval.

#The mean value remained same whereas median is shifted upwards by 1.19 steps.

#Earlier Values (after removing missing values)

mean(act_date_summ$tot_steps)

median(act_date_summ$tot_steps)


# New values (after imputing missing values with mean for that 5-minute interval)
  
mean(act_new_date_summ$tot_steps)

median(act_new_date_summ$tot_steps)



#4.2 What is the impact of imputing missing data on the estimates of the total daily number of steps?

# The effect of using mean data per interval as a data imputing method for missing values has pushed the median towards the mean.


## Are there differences in activity patterns between weekdays and weekends?
#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


# type cast the date column as date so we can use weekdays() function on that column to determine day
act_new$date <- as.Date(act_new$date)
act_new$weekday <- weekdays(act_new$date)
act_new$weekday_weekend <- ifelse(act_new$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday" )
head(act_new)



#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


act_day_summ <- (act_new %>% group_by(interval, weekday_weekend) %>% summarise(mean_steps = mean(steps) ) )
head(act_day_summ)

grid_labels <- c(Weekday = "Weekday", Weekend = "Weekend")

ggplot(act_day_summ, aes(x = interval, y = mean_steps)) + 
  geom_line(color = "blue") +
  facet_grid(weekday_weekend ~.) +
  labs(title = "Comparison of average steps across weekdays and weekends", x = "Interval (5 mins)", y = "Mean of Steps")




