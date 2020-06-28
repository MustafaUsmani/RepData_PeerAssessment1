---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
If zip file is not present

```r
if(!file.exists("activity.csv"))
  unzip("repdata_data_activity.zip")
```
Read the file into a variable called `data`

```r
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(total.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total.steps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
library(ggplot2)
avg <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=avg, aes(x=interval, y=steps)) +geom_line() +xlab("5-minute interval") +ylab("average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

5-minute interval contains that contain maximum number of steps

```r
avg[which.max(avg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

There are days/intervals with missing values (`NA`). This can cause baised calculations or summaries and evaluation.


```r
Missing <- is.na(data$steps)
table(Missing)
```

```
## Missing
## FALSE  TRUE 
## 15264  2304
```

Missing values are replaced with mean value for that 5-minute
interval.


```r
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    changeable <- NA
    if (!is.na(steps))
        changeable <- c(steps)
    else
        changeable <- (avg[avg$interval==interval, "steps"])
    return(changeable)
}
changeable.data <- data
changeable.data$steps <- mapply(fill.value, changeable.data$steps, changeable.data$interval)
```
Making histogram of the total steps taken each day and calculating the mean and median total number of steps with the corrected values.


```r
total.steps <- tapply(changeable.data$steps, changeable.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10766.19
```

 The impact of imputing the data appears to be a no change in the mean value, but a slight increase in the (now non-integer) median value.

## Are there differences in activity patterns between weekdays and weekends?



```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
changeable.data$date <- as.Date(changeable.data$date)
changeable.data$day <- sapply(changeable.data$date, FUN=weekday.or.weekend)
```



```r
avg <- aggregate(steps ~ interval + day, data=changeable.data, mean)
ggplot(avg, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
