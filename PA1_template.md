---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
##### Note: this assumes that "activity.csv" is in the same directory that this code is being run in.

```r
QuantifiedSelf<-read.csv("activity.csv")
QuantifiedSelf$date <- as.Date(QuantifiedSelf$date)
```


```r
steps_per_day<-tapply(QuantifiedSelf$steps, QuantifiedSelf$date, sum, na.rm = TRUE)

hist(as.numeric(as.character(steps_per_day)), col = "purple", breaks=50, main="Total Steps Per Day", xlab = "Steps")
```

![](PA1_template_files/figure-html/hist total steps per day-1.png)<!-- -->

## What is mean total number of steps taken per day?

##### For this part of the assignment, you can ignore the missing values in the dataset.
##### Note to reviewers: I interpreted "ignore" as "remove," since the mean would be na otherwise.
#### Mean total steps per day


```r
mean(steps_per_day)
```

```
## [1] 9354.23
```
#### Median total steps per day

```r
median(steps_per_day)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
##### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps 
##### taken, averaged across all days (y-axis).
  

```r
mean_steps_per_interval<-tapply(QuantifiedSelf$steps, QuantifiedSelf$interval, mean, na.rm=TRUE)

plot(mean_steps_per_interval, 
     type="l", 
     xlab="5-min Interval", 
     ylab="Avg number of steps taken")
```

![](PA1_template_files/figure-html/time series graph-1.png)<!-- -->

##### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
which.max(as.vector(mean_steps_per_interval))
```

```
## [1] 104
```

## Imputing missing values
##### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing
##### days may introduce bias into some calculations or summaries of the data.

##### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).


```r
sum(is.na(QuantifiedSelf$steps))
```

```
## [1] 2304
```

##### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be 
##### sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##### Create a new dataset that is equal to the original dataset but with the missing data filled in.
##### I used the calculated mean-per-interval (missing values removed first) as a substitute for #steps where value is na.


```r
QuantifiedSelfNoMissing<-subset(QuantifiedSelf, !is.na(QuantifiedSelf$steps))
mean_steps_per_interval_no_missing <- tapply(QuantifiedSelfNoMissing$steps, QuantifiedSelfNoMissing$interval, mean, simplify=T)
mean_steps_per_interval_no_missing<-as.data.frame(mean_steps_per_interval_no_missing)
QuantifiedSelfImputed<-QuantifiedSelf

for(i in 1:nrow(QuantifiedSelfImputed))
  {
    if(is.na(QuantifiedSelfImputed[i,1]))
      {
        row_number_of_desired_interval<-which(rownames(mean_steps_per_interval_no_missing)==QuantifiedSelfImputed[i,3])
        QuantifiedSelfImputed[i,1]<-mean_steps_per_interval_no_missing[row_number_of_desired_interval,1]
      }
  }
```
##### Make a histogram of the total number of steps taken each day.

```r
steps_per_day_imputed<-tapply(QuantifiedSelfImputed$steps, QuantifiedSelfImputed$date, sum)
hist(as.numeric(as.character(steps_per_day_imputed)), 
     col = "purple", 
     breaks=50, 
     main="Total Steps Per Day with Imputed Values", 
     xlab = "Steps")
```

![](PA1_template_files/figure-html/histogram imputed-1.png)<!-- -->

##### Report the mean and median total number of steps taken per day. 
#### Mean total steps per day with imputed values

```r
mean(steps_per_day_imputed)
```

```
## [1] 10766.19
```
#### Median total steps per day with imputed values

```r
median(steps_per_day_imputed)
```

```
## [1] 10766.19
```

##### Do these values differ from the estimates from the first part of the assignment? 
##### Yes, they are larger, which is expected, since more non-zero data points were added.
##### What is the impact of imputing missing data on the estimates of the total daily number of steps?
##### Higher step counts appear at higher frequency.

## Are there differences in activity patterns between weekdays and weekends?
##### For this part the weekdays() function may be of some help here. Use the dataset with the 
##### filled-in missing values for this part.

##### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating 
##### whether a given date is a weekday or weekend day.


```r
weekdaysVector<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
logicalVec<-weekdays(QuantifiedSelfImputed$date) %in% weekdaysVector
weekdaysFactor<-factor(logicalVec, levels=c(FALSE, TRUE), labels = c("Weekend", "Weekday"))
QuantifiedSelfImputed$weekPart<-weekdaysFactor
mean_steps_per_interval_by_weekPart<-aggregate(QuantifiedSelfImputed$steps, list(QuantifiedSelfImputed$interval, QuantifiedSelfImputed$weekPart), mean)
colnames(mean_steps_per_interval_by_weekPart)<-c("Interval", "WeekPart", "MeanSteps")
```

##### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##### and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
##### See the README file in the GitHub repository to see an example of what this plot should look like 
##### using simulated data.


```r
library(ggplot2)
p<-ggplot(mean_steps_per_interval_by_weekPart, aes(Interval, MeanSteps)) + geom_line()
p + facet_grid(WeekPart ~ .)
```

![](PA1_template_files/figure-html/plot weekday weekend-1.png)<!-- -->



