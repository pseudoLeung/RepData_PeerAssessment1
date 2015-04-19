---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

### Loading and preprocessing the data
* Load Data

```r
  activity <- read.csv('activity.csv')
```
* Process/transform the data (if necessary) into a format suitable for your analysis

```r
  stepsDate <- aggregate(steps~date, data=activity, FUN=sum, na.rm = T)
```

### What is mean total number of steps taken per day?
* Make a histogram of the total number of steps taken each day

```r
  barplot(stepsDate$steps,names.arg=stepsDate$date, xlab='date',ylab='steps')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

* Calculate and report the <b>mean</b> and <b>median</b> total number of steps taken per day

```r
  mean(stepsDate$steps)
```

```
## [1] 10766.19
```

```r
  median(stepsDate$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
  stepsInt <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=T)
  plot(stepsInt$interval, stepsInt$steps, type="l",xlab="interval",ylab="steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
  maxIdx <- which.max(stepsInt$steps)
  stepsInt[maxIdx,]$interval
```

```
## [1] 835
```

### Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
  sum(is.na(activity))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

    In this assessment, I will use <b>the mean for the the 5-minute interval</b>.
    
* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
  activity <- merge(activity, stepsInt, by="interval", suffixes=c("",".int"))
  allNa <- is.na(activity$steps)
  activity[allNa,]$steps <- activity[allNa,]$steps.int
  activity <- activity[,c(1:3)]
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
  sptesDate <- aggregate(steps~date, data=activity, FUN=sum)
  barplot(stepsDate$steps,names.arg=stepsDate$date, xlab='date',ylab='steps')
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
  mean(stepsDate$steps)
```

```
## [1] 10766.19
```

```r
  median(stepsDate$steps)
```

```
## [1] 10765
```

  The impact of imputing missing data is very low in my assessment.
  
### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
mywday <- function(date){
    d <- as.POSIXlt.date(date)
    if(d$wday %% 6 == 0){
      "weekday"
    }
    else{
      "weekend"
    }
}

activity$wds <- sapply(activity$date, mywday)
activity$wds <- as.factor(activity$wds)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
library(lattice)
stepsIntNew <- aggregate(steps~interval + wds, data=activity,FUN=mean)
xyplot(steps~interval|wds, data=stepsIntNew,aspect=0.5,type="l")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
