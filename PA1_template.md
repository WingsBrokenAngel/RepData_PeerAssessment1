---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Unzip and read the csv file named "activity.csv". 
Remove the NA steps in the data.

```r
raw_data <- read.csv(unz("activity.zip", "activity.csv"))
data <- raw_data[!is.na(raw_data$steps),]
```

## What is mean total number of steps taken per day?


```r
steps.by.day <- tapply(data$steps, data$date, sum)
mean.steps <- as.integer(round(mean(steps.by.day)))
median.steps <- median(steps.by.day)
```

The mean total number of steps taken per day is 10766 and the median is 10765.

## What is the average daily activity pattern?


```r
library(ggplot2)
steps.by.min <- tapply(data$steps, data$interval, mean)
qplot(steps.by.min, xlab = "Interval", ylab = "Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/pattern-1.png)<!-- -->

```r
max.interval <- steps.by.min[which.max(steps.by.min)]
max.name <- names(max.interval)
```

The 835 5-minute interval contains the maximum number of steps: 206.1698113.

## Imputing missing values

```r
num.missing <- sum(is.na(raw_data))
filled.data <- raw_data

for(interval in names(steps.by.min)) {
    cond <- which(is.na(filled.data$steps) & (filled.data$interval==interval))
    if(sum(cond) > 0) {
        filled.data[cond, 1] = steps.by.min[interval]
    }
}

steps.by.day2 <- tapply(filled.data$steps, filled.data$date, sum)

qplot(steps.by.day2, xlab = "Steps", ylab = "Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/missing-1.png)<!-- -->

```r
mean.steps2 <- as.integer(round(mean(steps.by.day2)))
median.steps2 <- as.integer(round(median(steps.by.day2)))
```


- The total number of missing values in the dataset is 2304.
- After filling the missing steps, the mean total number of steps taken per day is 10766, 
and the median is 10766.
- The mean and median values, before and after filling the missing steps, are almost the same.

## Are there differences in activity patterns between weekdays and weekends?


```r
library(chron)
filled.data$weekday <- as.factor(is.weekend(filled.data$date))
levels(filled.data$weekday) <- c("weekday", "weekend") 

weekday <- filled.data[filled.data$weekday == "weekday",]
weekend <- filled.data[filled.data$weekday == "weekend",]
weekday.by.interval <- tapply(weekday$steps, weekday$interval, mean)
weekend.by.interval <- tapply(weekend$steps, weekend$interval, mean)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(names(weekday.by.interval), weekday.by.interval, 
     type = "l", xlab = "", ylab = "Steps", main = "Steps by Interval in Weekday")

plot(names(weekend.by.interval), weekend.by.interval, 
     type = "l", xlab = "Interval", ylab = "Steps", main = "Steps by Interval in Weekend")
```

![](PA1_template_files/figure-html/differences-1.png)<!-- -->

The plot has been made as shown in above.
