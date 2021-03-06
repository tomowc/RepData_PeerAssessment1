# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
library(plyr)

Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
df <- read.csv("./activity.csv", stringsAsFactors = FALSE)
df[,2] <- as.Date(df[,2])
```

## What is mean total number of steps taken per day?


```r
# calculating total steps per day
day_sum <- tapply(df$steps, df$date, function(x) sum(x, na.rm=T))
# making simple histogram
hist(day_sum, breaks=10, main="Histogram of total steps per day", xlab="Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# mean number of total steps per day
mean(day_sum, na.rm=T)
```

```
## [1] 9354.23
```

```r
# median of total steps per day
median(day_sum, na.rm=T)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
# calculating average nymber of steps in each interval
interval_avg <- tapply(df$steps, df$interval, function(x) mean(x, na.rm=T))
# simple time series plot of calculated values
plot(unique(df$interval), interval_avg, type="l", main="Average number of steps in each interval", xlab="Interval", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# reporting interval with maximum number of steps
names(interval_avg)[which(interval_avg==max(interval_avg))]
```

```
## [1] "835"
```


## Imputing missing values

```r
# getting indices of missing cases
missing_case_ind <- which(!complete.cases(df))
# number of missing values
length(missing_case_ind)
```

```
## [1] 2304
```

```r
# missing values will be inputed with average steps for particualr interval
# variable interval_avg will be used for this

# changing variable interval_avg_df into data.frame, so it can later be merged
interval_avg_df <- data.frame(interval = names(interval_avg), avg = interval_avg)

# merging two dataframes (common column is "interval" so the values should be properly placed)
int_avg <- merge(df[missing_case_ind,],interval_avg_df)
# rearanging cases by date (because result of merging is sorted by interval)
int_avg <- int_avg[order(int_avg$date),]

# new dataframe
df_inp <- df
# inputing values
df_inp$steps[missing_case_ind] <- int_avg$avg
```


```r
# calculating new values of total steps per day
day_sum_inp <- tapply(df_inp$steps, df$date, function(x) sum(x, na.rm=T))
# making simple plot
hist(day_sum_inp, breaks=10, main="Histogram of total steps per day (inputed values)", xlab="Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
# mean number of total steps per day (with inputed values)
mean(day_sum_inp, na.rm=T)
```

```
## [1] 10766.19
```

```r
# median of total steps per day (with inputed values)
median(day_sum_inp, na.rm=T)
```

```
## [1] 10766.19
```

Questions here were: *Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?* The answers are: yes, they differ - total number of steps increased. Additionally mean and median are now the same.

## Are there differences in activity patterns between weekdays and weekends?

```r
# creating new factor variable
# firstly filling all with "weekday"
x <- rep("weekday", dim(df_inp)[1]) 
# then changing some values to "weekend"
x[weekdays(df_inp$date) == "Saturday" | weekdays(df_inp$date) == "Sunday"] <- "weekend" 
# adding new column "day_type"
df_inp$day_type <- factor(x)

# calculating average number of steps for each interval in weekdays and in weekend days
df_inp_avg <- ddply(df_inp, .(interval, day_type), function(x) mean(x[,1]))
# changing name of the variable
names(df_inp_avg)[3] <- "steps"
# initiating plot
p1 <- ggplot(df_inp_avg, aes(interval, steps))
# drawing plot
p1 + geom_line() + facet_grid(day_type ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

