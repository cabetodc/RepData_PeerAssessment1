# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

---

### 1. Load the data (i.e. read.csv())


```r
if(!file.exists("./data")){dir.create("./data")}
if(file.exists("./data/activity.zip")){unzip("./data/activity.zip", exdir = "./data")}
data = read.csv("../data/activity.csv")
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
data$date = as.Date(data$date, format = "%Y-%m-%d")
```

---

## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day


```r
tns = aggregate(steps ~ date, data, FUN = sum, na.rm = TRUE)
```


### 2. Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
qplot(steps, data = tns, binwidth = 1000, xlab = "total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(tns$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(tns$steps, na.rm = T)
```

```
## [1] 10765
```

---

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalav = aggregate(x = list(steps = data$steps), by = list(interval = data$interval), FUN = mean, na.rm = TRUE)
ggplot(intervalav, aes(x = interval, y = steps)) + 
geom_line(color = "blue", size = 1) +  
labs(x = "5-minute interval", y = "Average Number of Steps") +
theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 


### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
round(intervalav[which.max(intervalav$steps), ], digits = 1)
```

```
##     interval steps
## 104      835 206.2
```

---

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
sum(is.na(data))
```

```
## [1] 2304
```


### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

All of the missing values are filled in with mean value for 5-minute interval.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filling = merge(data, intervalav, by = 'interval')
names(filling) = c("interval", "steps", "date", "mean_steps")
filling$steps[is.na(filling$steps)] <- as.integer(
        round(filling$mean_steps[is.na(filling$steps)]))
filling <- filling[order(filling$date, filling$interval),]
n <- names(data)
filling <- filling[n]
```


### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
tnsfill <- aggregate(steps ~ date, filling, sum, na.rm = TRUE)
qplot(steps, data = tnsfill, binwidth = 1400, xlab = "total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(tnsfill$steps)
```

```
## [1] 10765.64
```

```r
median(tnsfill$steps)
```

```
## [1] 10762
```

**Do these values differ from the estimates from the first part of the assignment?**\

They do differ, but ever so slightly.

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**\

The impact of imputing missing data seems to depend on how you impute the missing data. After replacing missing steps values with the mean steps of associated interval value, these NA values are removed of total number of steps taken each day.

---

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
daytype = filling
weekend <- weekdays(as.Date(daytype$date)) %in% c("Saturday", "Sunday")
daytype$day <- "weekday"
daytype$day[weekend == TRUE] = "weekend"
daytype$day = as.factor(daytype$day)
str(daytype)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  2 0 0 0 0 2 1 1 0 1 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```


### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
ans <- aggregate(steps ~ interval + day, daytype, mean)
ggplot(ans, aes(x = interval, y = steps)) + 
geom_line(color = "blue", size = 1) + 
facet_wrap(~day, nrow = 2, ncol = 1) + 
labs(x = "Interval", y = "Number of Steps") +
theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

---
