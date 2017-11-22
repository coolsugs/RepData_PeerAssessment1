# Coursera Course 5 - Reproducible Research: Week 2 - Peer Assessment 1

```r
# loading required libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

## Loading and preprocessing the data

```r
# setting working directory
setwd("D:/Coursera/Course5_ReproducibleResearch/Week2/Project")
# read csv file
act_org <- read.csv("activity.csv")
dim(act_org)
```

```
## [1] 17568     3
```

```r
head(act_org)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
# omit records with NA
act_clean <- na.omit(act_org)
dim(act_clean)
```

```
## [1] 15264     3
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
# records grouped on date and total of steps calculated
act_date_summ <- act_clean %>% select(date, steps) %>% group_by(date) %>% summarize(tot_steps=sum(steps))
dim(act_date_summ)
```

```
## [1] 53  2
```

```r
summary(act_date_summ)
```

```
##          date      tot_steps    
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
act_date_summ
```

```
## # A tibble: 53 x 2
##          date tot_steps
##        <fctr>     <int>
##  1 2012-10-02       126
##  2 2012-10-03     11352
##  3 2012-10-04     12116
##  4 2012-10-05     13294
##  5 2012-10-06     15420
##  6 2012-10-07     11015
##  7 2012-10-09     12811
##  8 2012-10-10      9900
##  9 2012-10-11     10304
## 10 2012-10-12     17382
## # ... with 43 more rows
```

2. Make a histogram of the total number of steps taken each day

```r
ggplot(act_date_summ, aes(x = tot_steps)) +
       geom_histogram(color = "white", binwidth = 2000) +
  labs(title = "Daily Steps Histogram", x = "Total Daily Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(act_date_summ$tot_steps)
```

```
## [1] 10766.19
```

```r
median(act_date_summ$tot_steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
act_interval_mean <- act_clean %>% select(interval, steps) %>% group_by(interval) %>% summarize(avg_steps=mean(steps))
ggplot(act_interval_mean, aes(x = interval, y = avg_steps)) + 
       geom_line(color = "blue") + 
       labs(title = "Average Daily Actvity Pattern", x = "Interval (5 mins)", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
act_interval_mean[which.max(act_interval_mean$avg_steps),]
```

```
## # A tibble: 1 x 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835  206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(act_org))
```

```
## [1] 2304
```

2. Strategy for fillin gin all f the missing values in the dataset: 
   Missing values will be replaced with mean for that 5-minute interval


```r
repl_w_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
act_new <- act_org%>% group_by(interval) %>% mutate(steps= repl_w_mean(steps))
head(act_new)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##       steps       date interval
##       <dbl>     <fctr>    <int>
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
# check for any missing value
sum(is.na(act_new))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
# records grouped on date and total of steps calculated
act_new_date_summ <- act_new %>% select(date, steps) %>% group_by(date) %>% summarize(tot_steps=sum(steps))
```

```
## Adding missing grouping variables: `interval`
```

```r
ggplot(act_new_date_summ, aes(x = tot_steps)) +
       geom_histogram(color = "white", binwidth = 2000) +
  labs(title = "Daily Steps Histogram", x = "Total Daily Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
# mean total number of steps taken per day.
mean(act_new_date_summ$tot_steps)
```

```
## [1] 10766.19
```

```r
# median total number of steps taken per day.
median(act_new_date_summ$tot_steps)
```

```
## [1] 10766.19
```


  4.1 Do these values differ from the estimates from the first part of the assignment? 
  
The mean and median calculated in the first part of assignment after excluding missing values differ from the mean and median calculated now after replacing missing values with mean for that 5-minute interval.

The mean value remained same whereas median is shifted upwards by 1.19 steps.

**
*Earlier Values (after removing missing values)*
**

```r
mean(act_date_summ$tot_steps)
```

```
## [1] 10766.19
```

```r
median(act_date_summ$tot_steps)
```

```
## [1] 10765
```

**
*New values (after imputing missing values with mean for that 5-minute interval)*
**

```r
mean(act_new_date_summ$tot_steps)
```

```
## [1] 10766.19
```

```r
median(act_new_date_summ$tot_steps)
```

```
## [1] 10766.19
```


  4.2 What is the impact of imputing missing data on the estimates of the total daily number of steps?

The effect of using mean data per interval as a data imputing method for missing values has pushed the median towards the mean.


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# type cast the date column as date so we can use weekdays() function on that column to determine day
act_new$date <- as.Date(act_new$date)
act_new$weekday <- weekdays(act_new$date)
act_new$weekday_weekend <- ifelse(act_new$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday" )
head(act_new)
```

```
## # A tibble: 6 x 5
## # Groups:   interval [6]
##       steps       date interval weekday weekday_weekend
##       <dbl>     <date>    <int>   <chr>           <chr>
## 1 1.7169811 2012-10-01        0  Monday         Weekday
## 2 0.3396226 2012-10-01        5  Monday         Weekday
## 3 0.1320755 2012-10-01       10  Monday         Weekday
## 4 0.1509434 2012-10-01       15  Monday         Weekday
## 5 0.0754717 2012-10-01       20  Monday         Weekday
## 6 2.0943396 2012-10-01       25  Monday         Weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
act_day_summ <- (act_new %>% group_by(interval, weekday_weekend) %>% summarise(mean_steps = mean(steps) ) )
head(act_day_summ)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval weekday_weekend mean_steps
##      <int>           <chr>      <dbl>
## 1        0         Weekday 2.25115304
## 2        0         Weekend 0.21462264
## 3        5         Weekday 0.44528302
## 4        5         Weekend 0.04245283
## 5       10         Weekday 0.17316562
## 6       10         Weekend 0.01650943
```

```r
grid_labels <- c(Weekday = "Weekday", Weekend = "Weekend")

ggplot(act_day_summ, aes(x = interval, y = mean_steps)) + 
       geom_line(color = "blue") +
       facet_grid(weekday_weekend ~.) +
       labs(title = "Comparison of average steps across weekdays and weekends", x = "Interval (5 mins)", y = "Mean of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
