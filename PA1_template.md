---
output: 
  html_document: 
    keep_md: yes
---
output:
html_document:
keep_md: TRUE

#================================
#Reproducible Research Project 1
###Dorothea L. Ugi
###November 11, 2019
#================================

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

* steps:    Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) </br>
* date:     The date on which the measurement was taken in YYYY-MM-DD format </br>
* interval: Identifier for the 5-minute interval in which measurement was taken </br>

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 


```r
library("data.table")
library(ggplot2)
library(dplyr)
```

## Loading and preprocessing the data


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
activity <- data.table::fread(input = "data/activity.csv")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
totalSteps <- activity[, c(lapply(.SD, sum, na.rm=FALSE)), .SDcols=c("steps"), by=.(date)] 
head(totalSteps)
```

```
##          date steps
## 1: 2012-10-01    NA
## 2: 2012-10-02   126
## 3: 2012-10-03 11352
## 4: 2012-10-04 12116
## 5: 2012-10-05 13294
## 6: 2012-10-06 15420
```

2. Make a histogram of the total number of steps taken each day. 


```r
ggplot(totalSteps, aes(x=steps)) +
    geom_histogram(fill="blue", binwidth = 1000) +
    labs(title="Steps Taken Each Day", x="# of Steps", y="# of Days")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanSteps <- mean(totalSteps$steps, na.rm=TRUE)
meanSteps <- format(meanSteps, scientific=FALSE)
medianSteps <- median(totalSteps$steps, na.rm=TRUE)
```

The MEAN is 10766.19. and the MEDIAN is 10765.

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalSteps <- aggregate(steps ~ interval, activity, FUN=sum)

ggplot(intervalSteps, aes(x = interval , y = steps)) +
  geom_line(color="blue", size=1) + 
  labs(title = "Average Daily Steps", x = "5-minute Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxIntervalSteps <- filter(intervalSteps, steps==max(steps))
maxInterval <- maxIntervalSteps$interval
maxSteps <- maxIntervalSteps$steps
```

The 5-minute interval with the maximum number of steps is 835, with a maximum number of steps of 10927.

## Imputing missing values
#### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalNAs <- sum(is.na(activity$steps))
```

There are 2304 records with steps=NA.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Filling in missing values with median of dataset. 
activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.table::fwrite(x = activity, file = "ImputedActivity.csv", quote = FALSE)
imputedActivity <- data.table(activity)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of steps taken per day
imputedTotalSteps <- imputedActivity[, c(lapply(.SD, sum)), .SDcols=c("steps"), by=.(date)] 

## Calculate mean of imputed data
imputedMeanSteps <- mean(imputedTotalSteps$steps)
##Imputed Mean steps taken per day
imputedMeanSteps
##Original Mean steps taken per day
meanSteps

## Calculate median of imputed data
imputedMedianSteps <- median(imputedTotalSteps$steps)
##Imputed Median steps taken per day
imputedMedianSteps
## Original Median steps taken per day
medianSteps
```

With NAs ===============>  Mean = 10766.19, Median = 10765.
With Median Imputed ====>  Mean = 9354.2295082, Median = 10395.


```r
## Histogram
ggplot(imputedTotalSteps, aes(x=steps)) +
    geom_histogram(fill="blue", binwidth = 1000) +
    labs(title="Steps Taken Each Day", x="# of Steps", y="# of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
1.  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity[, date := as.POSIXct(date, format="%Y-%m-%d")]
activity[, `Day of Week`:= weekdays(x=date)]
activity[grepl(pattern="Monday|Tuesday|Wednesday|Thursday|Friday", x=`Day of Week`), "weekday or weekend"] <- "weekday"
activity[grepl(pattern="Saturday|Sunday", x=`Day of Week`), "weekday or weekend"] <- "weekend"
activity[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activity)
```

```
##    steps       date interval Day of Week weekday or weekend
## 1:     0 2012-10-01        0      Monday            weekday
## 2:     0 2012-10-01        5      Monday            weekday
## 3:     0 2012-10-01       10      Monday            weekday
## 4:     0 2012-10-01       15      Monday            weekday
## 5:     0 2012-10-01       20      Monday            weekday
## 6:     0 2012-10-01       25      Monday            weekday
```

```r
tail(activity)
```

```
##    steps       date interval Day of Week weekday or weekend
## 1:     0 2012-11-30     2330      Friday            weekday
## 2:     0 2012-11-30     2335      Friday            weekday
## 3:     0 2012-11-30     2340      Friday            weekday
## 4:     0 2012-11-30     2345      Friday            weekday
## 5:     0 2012-11-30     2350      Friday            weekday
## 6:     0 2012-11-30     2355      Friday            weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, median, na.rm=TRUE)), .SDcols=c("steps")]
intervalActivity <- activity[, c(lapply(.SD, mean, na.rm=TRUE)), .SDcols=c("steps"), by=.(interval, `weekday or weekend`)] 

ggplot(intervalActivity , aes(x=interval , y=steps, color=`weekday or weekend`)) + 
  geom_line() + 
  labs(title="Average Daily Steps by Type of Day (weekday vs. weekend day)", x="Interval", y="Number of Steps") + 
  facet_wrap(~`weekday or weekend` , ncol=1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



















