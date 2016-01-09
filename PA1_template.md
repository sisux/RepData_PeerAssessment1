# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)


```r
if (!file.exists("activity.csv")) {
        unzip("activity.zip")}

activity <- read.csv(file = "activity.csv", sep = ",", header = TRUE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.


```r
activity.p1 <- na.omit(activity)
```

1. Make a histogram of the total number of steps taken each day


```r
activity.p1.sum <- aggregate(x = list(TotalSteps = activity.p1$steps),
                     FUN = sum,
                     by = list(GroupedDate = activity.p1$date))

hist(x = activity.p1.sum$TotalSteps)
```

![](Figure/hist_totalStepsPerDay-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
activity.p1.mean <- mean(activity.p1.sum$TotalSteps)
activity.p1.median <- median(activity.p1.sum$TotalSteps)

sprintf("Mean = %f", activity.p1.mean)
```

```
## [1] "Mean = 10766.188679"
```

```r
sprintf("Median = %f", activity.p1.median)
```

```
## [1] "Median = 10765.000000"
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activity.5m <- aggregate(steps~interval, data=activity.p1, FUN=mean, na.rm=TRUE)
plot(x = activity.5m$interval, y = activity.5m$steps, type = "l") 
```

![](Figure/plot_5minIntervalbyAvgSteps-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- max(activity.5m$steps, na.rm=TRUE)
maxInterval <- activity.5m[activity.5m$steps==maxSteps,"interval"]

sprintf("Max 5-min Interval = %d", maxInterval)
```

```
## [1] "Max 5-min Interval = 835"
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
na.cases <- activity[!complete.cases(activity),]
nrow(na.cases)
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
complete.cases <- activity[complete.cases(activity),]

na.cases2 <- merge(x = na.cases, y = activity.5m, by = "interval")
na.cases2$steps.x <- NULL
names(na.cases2)[names(na.cases2)=="steps.y"] <- "steps"

full.ds <- rbind(complete.cases, na.cases2)
head(full.ds)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activity.p4.sum <- aggregate(x = list(TotalSteps = full.ds$steps),
                     FUN = sum,
                     by = list(GroupedDate = full.ds$date))

hist(x = activity.p4.sum$TotalSteps)
```

![](Figure/hist_totalStepsPerDay-Na-1.png) 


```r
activity.p4.mean <- mean(activity.p4.sum$TotalSteps)
activity.p4.median <- median(activity.p4.sum$TotalSteps)

sprintf("Mean = %f", activity.p4.mean)
```

```
## [1] "Mean = 10766.188679"
```

```r
sprintf("Median = %f", activity.p4.median)
```

```
## [1] "Median = 10766.188679"
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
getDayType <- function(x) {
  if (x['weekday'] == 'Sunday' || x['weekday'] == 'Saturday')
    return('weekend')
  else
    return('weekday')
}

Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
full.ds$weekday <- weekdays(full.ds$date)
full.ds$dayType <- apply(full.ds, 1, FUN=getDayType)
full.ds$dayType <- as.factor(full.ds$dayType)
```

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:


```r
activity.5m_2 <- aggregate(steps~interval+dayType, data=full.ds, FUN=mean, na.rm=TRUE)

library(lattice)
xyplot(steps ~ interval | dayType, data = activity.5m_2, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](Figure/plot_5minIntervalByAvgSteps-dayType-1.png) 
