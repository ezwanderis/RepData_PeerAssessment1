# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
#Load the data (i.e. read.csv())
df <- read.csv("activity.csv")

#Process/transform the data (if necessary) into a format suitable for your analysis

df$date <- as.Date(df$date)
```

## What is mean total number of steps taken per day?


```r
#histogram of the total number of steps taken each day

library(ggplot2)
totalStepDaily <- aggregate(x = df$steps, by = list(df$date), FUN = sum, na.rm=TRUE)
names(totalStepDaily) <- c("date","steps")
histplot <- ggplot(totalStepDaily,aes(x = steps)) +
            ggtitle("Histogram of daily steps") +
            xlab("Steps (binwidth 2000)") +
            geom_histogram(binwidth = 2000)
histplot
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#mean total number of steps taken per day

mean(totalStepDaily$steps , na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
#median total number of steps taken per day

median(totalStepDaily$steps , na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
#Time series plot of 5-minute interval and the average number of steps taken, averaged across all days

averageStepByInterval  <- aggregate(x = df$steps , by = list(df$interval), FUN = mean ,na.rm=TRUE)
names(averageStepByInterval) <- c("interval","steps")

averageStepLine <- ggplot(averageStepByInterval,aes(interval,steps)) +
                 ggtitle("Time Series Plot of Average Steps by Interval") +
                 geom_line(size = 0.3, colour = "red")
averageStepLine  
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#The 5-min time interval contains the maximum number of steps?

averageStepByInterval[which.max(averageStepByInterval$steps),c("interval")]
```

```
## [1] 835
```

## Imputing missing values

```r
#total number of missing values in the dataset
nrow(df[is.na(df$steps),])
```

```
## [1] 2304
```

```r
#imputing missing step values with mean step at time interval
dfImputed <- merge(x = df, y = averageStepByInterval, by = "interval", all.x = TRUE)
dfImputed[is.na(dfImputed$steps.x),c("steps.x")] <- dfImputed[is.na(dfImputed$steps.x),c("steps.y")]

#cleaning data
dfImputed$date <- as.Date(dfImputed$date)
dfImputed$date.x <- NULL
dfImputed$Group.1 <- NULL
dfImputed$steps <- dfImputed$steps.x
dfImputed$steps.x <- NULL
dfImputed$steps.y <- NULL

#histogram with new dataframe
totalStepDaily <- aggregate(x = dfImputed$steps , by = list(dfImputed$date), FUN = sum ,na.rm=TRUE)
names(totalStepDaily) <- c("date","steps")
histplot <- ggplot(totalStepDaily,aes(x = steps)) +
            ggtitle("Histogram of daily steps after imputation") +
            xlab("Steps (binwidth 2000)") +
            geom_histogram(binwidth = 2000)
histplot 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#mean total number of steps taken per day
mean(totalStepDaily$steps , na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#median total number of steps taken per day
median(totalStepDaily$steps , na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
#Factor variable with two levels indicating a weekday or weekend.
dfImputed$weekday <- as.factor(ifelse(weekdays(dfImputed$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")) 

averageStepByIntervalWeekday  <- aggregate(x = dfImputed$steps , 
                                                    by = list(dfImputed$interval,dfImputed$weekday), FUN = mean ,na.rm=TRUE)
names(averageStepByIntervalWeekday) <- c("interval","weekday","steps")

#panel time series plot of the 5-minute interval and the average number of steps taken 
#averaged across all weekday days or weekend days.
averageStepLine <- ggplot(averageStepByIntervalWeekday,aes(interval,steps)) +
                 ggtitle("Time Series Plot of Average Steps by Interval after Imputation") +
                 facet_grid(weekday ~ .  ) +
                 geom_line(size = 0.3, colour = "red")

averageStepLine  
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
