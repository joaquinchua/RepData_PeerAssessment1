---
title: "Reproducible Research: Peer Assessment 1"
author: Joaquin Chua
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Load libraries, unzip and read file

```r
library(data.table)
library(ggplot2)

unzip("activity.zip")
data <- fread("activity.csv")
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
totalsteps <- data[ , sum(steps), by=.(date)]
names(totalsteps)[2] <- "steps"
totalsteps
```

```
##           date steps
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
## 11: 2012-10-11 10304
## 12: 2012-10-12 17382
## 13: 2012-10-13 12426
## 14: 2012-10-14 15098
## 15: 2012-10-15 10139
## 16: 2012-10-16 15084
## 17: 2012-10-17 13452
## 18: 2012-10-18 10056
## 19: 2012-10-19 11829
## 20: 2012-10-20 10395
## 21: 2012-10-21  8821
## 22: 2012-10-22 13460
## 23: 2012-10-23  8918
## 24: 2012-10-24  8355
## 25: 2012-10-25  2492
## 26: 2012-10-26  6778
## 27: 2012-10-27 10119
## 28: 2012-10-28 11458
## 29: 2012-10-29  5018
## 30: 2012-10-30  9819
## 31: 2012-10-31 15414
## 32: 2012-11-01    NA
## 33: 2012-11-02 10600
## 34: 2012-11-03 10571
## 35: 2012-11-04    NA
## 36: 2012-11-05 10439
## 37: 2012-11-06  8334
## 38: 2012-11-07 12883
## 39: 2012-11-08  3219
## 40: 2012-11-09    NA
## 41: 2012-11-10    NA
## 42: 2012-11-11 12608
## 43: 2012-11-12 10765
## 44: 2012-11-13  7336
## 45: 2012-11-14    NA
## 46: 2012-11-15    41
## 47: 2012-11-16  5441
## 48: 2012-11-17 14339
## 49: 2012-11-18 15110
## 50: 2012-11-19  8841
## 51: 2012-11-20  4472
## 52: 2012-11-21 12787
## 53: 2012-11-22 20427
## 54: 2012-11-23 21194
## 55: 2012-11-24 14478
## 56: 2012-11-25 11834
## 57: 2012-11-26 11162
## 58: 2012-11-27 13646
## 59: 2012-11-28 10183
## 60: 2012-11-29  7047
## 61: 2012-11-30    NA
##           date steps
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
ggplot(totalsteps, aes(x = steps)) +
    geom_histogram(binwidth = 500) +
    labs(title = "Total Steps Per Day", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalsteps$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(totalsteps$steps, na.rm = T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalsteps <- data[ , mean(steps, na.rm = T), by=.(interval)]
names(intervalsteps)[2] <- "meansteps"

ggplot(intervalsteps, aes(x = interval, y = meansteps)) +
  geom_line() +
  labs(title = "Average Daily Steps", x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalsteps[meansteps==max(meansteps), .(interval)]
```

```
##    interval
## 1:      835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
data[is.na(steps), .N]
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# Impute using mean of each interval
data2 <- copy(data)
forimput <- data[ , round(mean(steps, na.rm = T)), by=.(interval)]
names(forimput)[2] <- "meansteps"

for (i in 1:data2[,.N]) {
  if (is.na(data2[i,steps])) {
    data2[i,"steps"] <- forimput[interval == data2[i,interval], meansteps]
  }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fwrite(data2,"activity_filled.csv")
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalsteps2 <- data2[ , sum(steps), by=.(date)]
names(totalsteps2)[2] <- "steps"

ggplot(totalsteps2, aes(x = steps)) +
    geom_histogram(binwidth = 500) +
    labs(title = "Total Steps Per Day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
mean(totalsteps2$steps, na.rm = T)
```

```
## [1] 10765.64
```

```r
median(totalsteps2$steps, na.rm = T)
```

```
## [1] 10762
```

There are differences for the mean and median between the raw dataset and imputed dataset. The imputed dataset results in a lower mean and median steps per day. 


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data2[, date := as.Date(date)]
data2[, day := weekdays(date)]

data2[, daytype := as.factor(ifelse(day %in% c("Saturday","Sunday"), "weekend", "weekday"))]
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
intervalsteps2 <- data2[ , mean(steps, na.rm = T), by=.(interval,daytype)]
names(intervalsteps2)[3] <- "meansteps"

ggplot(intervalsteps2, aes(x = interval, y = meansteps, colour = daytype)) +
  geom_line() +
  facet_wrap(.~daytype, nrow = 2) +
  labs(title = "Average Daily Steps", x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


