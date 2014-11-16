---
title: 'Reproducible Research: Peer Assessment 1'
output: html_document
---


loading and preprocessing the data

read the data

```r
data<-read.csv("/Users/lucatang/Desktop/Coursera/activity.csv")
library(lattice)
data$date<-as.Date(data$date,"%Y-%m-%d")
```

##What is the mean total number of steps taken per day?

1.Make a histogram of the total number of steps taken each day


```r
stepstotal<-aggregate(steps~date,data=data,sum,na.rm=TRUE)
hist(stepstotal$steps,main="Total Steps by Day",xlab="day",col=1)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2.Calculate and report the mean and median total number of steps taken per day


```r
mean(stepstotal$steps)
```

```
## [1] 10766.19
```

```r
median(stepstotal$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
time<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
plot(row.names(time),time,type="l", main="Average number of steps averaged over all days", xlab="Interval", ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max<-which.max(time)
names(max)
```

```
## [1] "835"
```

##Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
data_NA <- sum(is.na(data))
data_NA
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.


```r
stepsaverage<-aggregate(steps~interval,data=data,FUN=mean)
fillNA<-numeric()
for (i in 1:nrow(data)){
  obs<-data[i, ]
  if(is.na(obs$steps)){
    steps <- subset(stepsaverage, interval == obs$interval)$steps
  }
  else {
    steps <- obs$steps
  }
  fillNA<-c(fillNA,steps)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_data<-data
new_data$steps<-fillNA
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepstotal2<-aggregate(steps~date,data=new_data,sum,na.rm=TRUE)
hist(stepstotal2$steps,main="Total Steps by Day",xlab="day",col=1)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(stepstotal2$steps)
```

```
## [1] 10766.19
```

```r
median(stepstotal2$steps)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.


```r
day <- weekdays(data$date)
weekday <- vector()
for (i in 1:nrow(data)) {
    if (day[i] == "Saturday" | day[i] == "Sunday") {
        weekday[i] <- "Weekend"
    } 
    else {
        weekday[i] <- "Weekday"
    }
}
data$weekday <- weekday
data$weekday <- factor(data$weekday)
stepsbyday <- aggregate(steps ~ interval + weekday, data=data,mean)
names(stepsbyday)<-c("interval","weekday","steps")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
xyplot(steps ~ interval | weekday, stepsbyday, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps",col="red")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
