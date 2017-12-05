---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

```r
stepsPerDay <- sapply(split(data, data$date), function(x){sum(x$steps, na.rm = TRUE)})
```

Make a histogram of the total number of steps taken each day

```r
hist(stepsPerDay, xlab = "Total Steps Per Day", main = "Histogram of Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsPerDay)
```

```
## [1] 9354.23
```

```r
median(stepsPerDay)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanDailyActivity = sapply(split(data, data$interval), function(x){mean(x$steps, na.rm = TRUE)})
plot(names(meanDailyActivity), meanDailyActivity, type = "l", xlab = "Interval", ylab = "Mean Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(meanDailyActivity[meanDailyActivity == max(meanDailyActivity)])
```

```
## [1] "835"
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dataFilled <- data
#Fill in NA values with the mean value for that 5-minute interval, found in meanDailyActivity.
for(i in 1:nrow(dataFilled))
{
	if(is.na(dataFilled[i,1]))
	{
		dataFilled[i,1] = meanDailyActivity[[as.character(dataFilled[i,3])]]
	}
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
stepsPerDayFilled <- sapply(split(dataFilled, dataFilled$date), function(x){sum(x$steps, na.rm = TRUE)})
hist(stepsPerDayFilled, xlab = "Total Steps Per Day", main = "Histogram of Total Steps Per Day with NA Values Filled")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(stepsPerDayFilled)
```

```
## [1] 10766.19
```

```r
median(stepsPerDayFilled)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
*Inputting missing data does change the output by raising the mean and median. The mean was impacted much more (9354.23 vs 10766.19) than the median (10395 vs 10766.19)*

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekend <- c('Saturday','Sunday')
dataFilled$weekday <- c('weekday', 'weekend')[(weekdays(dataFilled$date) %in% weekend) + 1L]
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(ggplot2)
ggplot(dataFilled, aes(interval, steps)) +
	facet_grid(weekday ~ .) +
	stat_summary(fun.y = mean, geom = "line") +
	labs(x = "Interval", y = "Mean Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
