---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

**Gilbert Toroitich Tarus**




#### **Loading and preprocessing the data**

```r
activity <- read.csv(unz("activity.zip", "activity.csv"))

# Transform the data
activity <- transform(activity,date = as.Date(date,"%Y-%m-%d"))
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## **What is mean total number of steps taken per day?**

```r
## Load the required packages
library(ggplot2)
library(dplyr)
```

#### **Plot Histogram of total number of steps taken each day.**

```r
total_steps <- activity %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps,na.rm = TRUE))

ggplot(total_steps,aes(x = steps))+
  geom_histogram(fill = "navyblue",bins = 10)+
  coord_cartesian(ylim = c(0,20))+
  theme_dark()+
  labs(title = "Total number of steps taken each day", x ="Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

#### **Mean and median total number of steps taken per day.**

#### **Mean**

```r
mean(total_steps$steps)
```

```
## [1] 9354.23
```

#### **Median**

```r
median(total_steps$steps)
```

```
## [1] 10395
```

#### **What is the average daily activity pattern?**
**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) **

```r
activity %>% 
  group_by(interval) %>% 
  summarise(Average = mean(steps,na.rm = TRUE)) %>% 
  ggplot(aes(x= interval,y = Average))+
  geom_line(color = "Wheat")+
  theme_dark()+
  labs(title = "Average number of steps taken against 5-minute interval",x=" Interval", y = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
max_interval <- activity %>% 
  group_by(interval) %>% 
  filter(!is.na(steps)) %>% 
  summarise(Mean = mean(steps))

# Maximum interval
max_interval[which.max(max_interval$Mean),]$interval
```

```
## [1] 835
```


#### **Imputing missing values**
**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

```r
nas <- activity %>% 
  filter(is.na(steps)) %>% 
  summarise(Total = n())
nas
```

```
##   Total
## 1  2304
```

```r
mean <- mean(is.na(activity$steps))
```
The Missing values accounts for **``0.1311475``**. There are **``2304``** missing values.

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

```r
imp_steps <- max_interval$Mean[match(activity$interval,max_interval$interval)]
```

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```r
imputed_data <- transform(activity,steps = ifelse(is.na(activity$steps),imp_steps,activity$steps))

total_steps_imp <- imputed_data %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))
total_steps_imp
```

```
## Warning: `...` is not empty.
## 
## We detected these problematic arguments:
## * `needs_dots`
## 
## These dots only exist to allow future extensions and should be empty.
## Did you misspecify an argument?
```

```
## # A tibble: 61 x 2
##    date        steps
##    <date>      <dbl>
##  1 2012-10-01 10766.
##  2 2012-10-02   126 
##  3 2012-10-03 11352 
##  4 2012-10-04 12116 
##  5 2012-10-05 13294 
##  6 2012-10-06 15420 
##  7 2012-10-07 11015 
##  8 2012-10-08 10766.
##  9 2012-10-09 12811 
## 10 2012-10-10  9900 
## # ... with 51 more rows
```

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
## Histogram of the total number of steps per day
total_steps_imp %>%
  ggplot(aes(x = steps))+
  geom_histogram(fill = rgb(0.6,0.20,0.4),bins = 10)+
  coord_cartesian(ylim = c(0,30))+
  labs(title = "Histogram of the total number of steps taken per day",x ="Steps",y = "Frequency",subtitle = "for the imputed data")+
  theme_dark()
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#### **Mean and median of the imputed data per day**

### **Mean**

```r
mean(total_steps_imp$steps)
```

```
## [1] 10766.19
```

### **Median**

```r
median(total_steps_imp$steps)
```

```
## [1] 10766.19
```
#### **Are there differences in activity patterns between weekdays and weekends?**

**Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**

```r
activity$day <- with(activity,ifelse(weekdays(date)=="Saturday" | weekdays(date)=="Sunday","Weekend","Weekday"))
activity <- transform(activity,day = as.factor(day))
```

### **Plot**


```r
plotdata <- activity %>% 
  group_by(interval,day) %>% 
  summarise(Average = mean(steps, na.rm = TRUE))

plot <- ggplot(plotdata,aes(x = interval,y = Average, color = day))+
  geom_line()+
  facet_wrap(day~.,ncol = 1, nrow = 2)+
  labs(x = "Interval", y = "Number of Steps",title = "Activity patterns between weekdays and weekends")

plot
```

![](PA1_template_files/figure-html/unnamed-chunk-25-1.png)<!-- -->
