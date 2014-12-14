# Reproducible Research: Peer Assessment 1

  
## Loading and preprocessing the data
#### Loading the 'activity.csv' file

```r
data_initial <- read.table("activity/activity.csv", header=TRUE, sep=",")
data <- data_initial[complete.cases(data_initial),]
```
#### Finding the total number of steps taken each day

```r
total_steps_each_day = tapply(data$steps, data$date, sum)
```

#### Genering a histogram of the total number of steps taken each day

```r
png(filename="figures/histogram.png", width=720, height=480)
hist(total_steps_each_day, breaks=nrow(total_steps_each_day), 
     main="Distribution of Total # Of Steps Taken Each Day",
     col="lightgreen", xlab="Total # Of Steps")
dev.off()
```

```
## pdf 
##   2
```

## What is mean total number of steps taken per day?
#### Calculating the mean and the median    

```r
mean = mean(total_steps_each_day, na.rm=TRUE)
median = median(total_steps_each_day, na.rm=TRUE)
```
#### The 'mean' and the 'median' of the total number of steps taken per day

```
##    mean median
## 1 10766  10765
```


## What is the average daily activity pattern?

#### Calculating the averaged steps for each 5-min intervals

```r
average_steps_each_interval = tapply(data$steps, data$interval, mean)
```

#### Time series plot of the 5-min interval (x-axis) and the average
#### number of steps taken, averaged across all days (y-axis)

```r
png(filename="figures/time_series.png", width=720, height=480)
plot(unique(data$interval), average_steps_each_interval, type='l',
     xlab='5-minute intervals', 
     ylab='average number of steps averaged across all days',
     main='Average Steps Across 5-min Intervals')
dev.off()
```

```
## pdf 
##   2
```

#### Finding the interval containing the maximum number of steps

```r
max_interval = names(which(average_steps_each_interval == 
                         max(average_steps_each_interval)))
```

#### the interval of max. number of steps

```r
print(paste("max_interval: ", max_interval))
```

```
## [1] "max_interval:  835"
```

## Imputing missing values
#### Finding the total number of missing values in the datasets

```r
num_of_NA = nrow(data_initial[!(complete.cases(data_initial)),])
```

#### Creating a new dataset by replacing each of the missing step values with
#### its corresponding 5-min mean value

```r
na_indices = which(is.na(data_initial$steps))
data_na_replaced <- data_initial
data_na_replaced$steps[na_indices] = 
    as.numeric(average_steps_each_interval[as.character(data_initial$interval[na_indices])])
```

#### Finding the total number of steps taken each day from the new dataset 

```r
total_steps_each_day_new_data = tapply(data_na_replaced$steps, data_na_replaced$date, sum)
```

#### Histogram of the total number of steps taken each day [DATASET with NA filled]

```r
png(filename="figures/histogram_NA_filled.png", width=720, height=480)
hist(total_steps_each_day_new_data, breaks=nrow(total_steps_each_day_new_data), 
     main="Distribution of Total # Of Steps Taken Each Day",
     col="lightgreen", xlab="Total # Of Steps")
dev.off()
```

```
## pdf 
##   2
```

#### The 'mean' and the 'median' of total number of steps taken per day [DATASET with NA filled]

```r
mean_new_data = mean(total_steps_each_day_new_data)
median_new_data = median(total_steps_each_day_new_data)
```

#### Comparing the mean and the median between the old dataset and the new dataset

```r
print(data.frame(old_dataset = mean, new_dataset = mean_new_data))
```

```
##   old_dataset new_dataset
## 1       10766       10766
```

```r
print(data.frame(old_dataset = median, new_dataset = median_new_data))
```

```
##   old_dataset new_dataset
## 1       10765       10766
```

## Are there differences in activity patterns between weekdays and weekends?
#### Creating a new factor variable in the dataset with two levels - "weekday" and "weekend"

```r
data_na_replaced$date <- weekdays(as.Date(data_na_replaced$date))

weekdays = data_na_replaced[which(data_na_replaced$date=="Monday" | 
                                  data_na_replaced$date=="Tuesday" |
                                  data_na_replaced$date=="Wednesday" |
                                  data_na_replaced$date=="Thursday" |
                                  data_na_replaced$date=="Friday"),]

weekend = data_na_replaced[which(data_na_replaced$date=="Saturday"|data_na_replaced$date=="Sunday"),]
```

#### Finding the averaged steps for each [weekdays, weekend] intervals

```r
weekdays_by_interval = tapply(weekdays$steps, weekdays$interval, mean)
weekend_by_interval = tapply(weekend$steps, weekend$interval, mean)
```

#### The panel plot containing a time series plot for the weekdays and weekend

```r
png(filename="figures/time_series_weekdays_weekend.png", width=720, height=480)
par(mfrow=(c(2,1)))
par(mar=c(2.5,2.5, 1.5, 1.5))
plot(unique(weekdays$interval), weekdays_by_interval, type='l', xlab='', ylab='', main='Weekdays')
plot(unique(weekend$interval), weekend_by_interval, type='l', xlab='5-min Interval', ylab='',main='Weekend')
dev.off()
```

```
## pdf 
##   2
```
