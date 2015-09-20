# Reproducible Research: Peer Assessment 1


```
## Warning: package 'knitr' was built under R version 3.2.2
```


## Loading and preprocessing the data


I read the (unzipped) data frame and make two transformations:
    1. the date column to R date format
    2. the intervale column to minutes (e.g: 210 = 130, Two O'clock and ten minutes equals 130 minutes)

```r
data_file <- "activity.csv"
zip_file <- "activity.zip"
unzip(zipfile = zip_file)
df <- read.csv(data_file,header = TRUE)
df$interval_as_min = ts(df$interval%%100 + df$interval%/%100 * 60)
```

## What is mean total number of steps taken per day?


```r
library(plyr)
total_steps_per_day <- ddply(df,~date,summarise,total_steps=sum(steps,na.rm = TRUE))
hist(total_steps_per_day$total_steps, xlab = "total number of steps per day",
     main = "An histogram of the total number of steps taken each day", labels = TRUE)

mean_steps <- mean(total_steps_per_day$total_steps)
abline(v = mean_steps, col = "blue", lwd = 2 )
text(mean_steps-3000, 20 , paste("mean",round(mean_steps,0)), col = "blue")

median_steps <- median(total_steps_per_day$total_steps)
abline(v = median_steps, col = "red", lwd = 2)
text(median_steps+3000, 25 , paste("median",round(median_steps,0)), col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean total number of steps taken per day is 9354.23

```r
mean(total_steps_per_day$total_steps)
```

```
## [1] 9354.23
```

The median total number of steps taken per day is 10395

```r
median(total_steps_per_day$total_steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
steps_per_interval <- ddply(df,~interval_as_min,summarise,mean=mean(steps,na.rm = TRUE))
plot(steps_per_interval$interval_as_min,steps_per_interval$mean, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


```r
max_steps <- max(steps_per_interval$mean)
max_interval <- steps_per_interval[steps_per_interval$mean == max_steps,]
#max_interval$interval%%60 + max_interval$interval%/%60 * 100
```


## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(df))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
df$date_as_date <- as.Date(df$date)
```


## Are there differences in activity patterns between weekdays and weekends?
