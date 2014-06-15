Peer Assessments 1  
========================================================

This is an R Markdown document of peer assessment 1 in Reproducible Research. 

##Loading and processing the data

1. Load the data (i.e. read.csv())


```r
setwd("C:/R/coursera")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
    dest = "repro_peer1.zip")
```

```
## Error: unsupported URL scheme
```

```r
unzip("repro_peer1.zip")
dt <- read.csv("activity.csv")
```


2. Process/transform the data (if necessary) into a format suitable for your analysis

##What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
stepsDate <- tapply(dt$steps, dt$date, sum, na.rm = TRUE)
par(las = 2, cex.axis = 0.5)
barplot(stepsDate)
title("Total number of steps by dates")
```

![plot of chunk histogram](figure/histogram.png) 


2. Calculate and report the mean and median total number of steps taken per day


```r
mean(stepsDate, na.rm = TRUE)
```

```
## [1] 9354
```

```r
median(stepsDate, na.rm = TRUE)
```

```
## [1] 10395
```


##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
par(las = 2, cex.axis = 1)
stepsInterval <- tapply(dt$steps, dt$interval, mean, na.rm = TRUE)
plot(names(stepsInterval), stepsInterval, type = "l", xlab = "Interval", ylab = "Avg.Steps")
title("Average number of steps by interval")
```

![plot of chunk time series plot](figure/time_series_plot.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
- Accoding to the result, maximum number of average steps is 206.1698 at 835.


```r
max(stepsInterval)
```

```
## [1] 206.2
```

```r
which.max(stepsInterval)
```

```
## 835 
## 104
```


##Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
- Accoding to the result, the totla number of missing values in the dataset is 2304.


```r
dim(subset(dt, is.na(dt$steps)))
```

```
## [1] 2304    3
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- 


```r
new_dt <- dt
for (i in row.names(new_dt)) {
    i <- as.numeric(i)
    if (is.na(new_dt[i, "steps"])) {
        new_dt[i, "steps"] <- stepsInterval[paste(new_dt[i, "interval"])]
    }
}
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
- The total number of steps taken each day is not differ from the estimates from the original data, except when the total number of steps taken .


```r
new_stepsDate <- tapply(new_dt$steps, new_dt$date, sum, na.rm = TRUE)
par(mfrow = c(2, 1), las = 2, cex.axis = 0.5)
barplot(stepsDate)
title("Total number of steps from original data")
barplot(new_stepsDate)
title("Total number of steps from revised data")
```

![plot of chunk compare_histogram](figure/compare_histogram.png) 


##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels ??? ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.


```r
for (i in row.names(new_dt)) {
    i <- as.numeric(i)
    day <- weekdays(as.Date(new_dt[i, "date"]))
    if (day == "<U+C77C><U+C694><U+C77C>" | day == "<U+D1A0><U+C694><U+C77C>" | 
        day == "Sunday" | day == "Saturday") {
        new_dt[i, "day"] <- "weekend"
    } else {
        new_dt[i, "day"] <- "weekday"
    }
}
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
weekday <- subset(new_dt, new_dt$day == "weekday")
weekend <- subset(new_dt, new_dt$day == "weekend")
weekday_stepsInterval <- tapply(weekday$steps, weekday$interval, mean, na.rm = TRUE)
weekend_stepsInterval <- tapply(weekend$steps, weekend$interval, mean, na.rm = TRUE)
par(mfrow = c(2, 1), las = 1, cex.axis = 1)
plot(names(weekday_stepsInterval), weekday_stepsInterval, type = "l", xlab = "Interval", 
    ylab = "Avg.Steps")
title("Average number of steps by interval on weekday")
plot(names(weekend_stepsInterval), weekend_stepsInterval, type = "l", xlab = "Interval", 
    ylab = "Avg.Steps")
title("Average number of steps by interval on weekend")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


