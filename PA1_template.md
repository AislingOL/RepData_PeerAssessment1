---
title: "Course5Assignment1"
author: "AO"
date: "10/08/2020"
output: html_document
---

# Reproducible Research Week 2 Course Project 1


## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Install packages
```{r}
#install.packages("tidyverse")
install.packages("knitr")
```

## Loading and preprocessing the data

```{r, echo = TRUE}
library(tidyverse)
library(knitr)
```


### 1. Load the Data

Check if data file exists, unzipping the file and reading the data from the activity.csv file into R if it does not exist in the wording directory.
```{r, echo = TRUE}
if (!file.exists('activity.csv')) {
     unzip(zipfile = "activity.zip")
} # Checking if the activity.csv file exists and unzipping it if not

activity <- read.csv(file = "activity.csv", 
     header = TRUE) #reading the file
```

### 2.Process/transform the data (if necessary) into a format suitable for your analysis

Summarising the data and viewing the structure.
```{r, echo = TRUE}
summary(activity) # summarising the activity data
```
```{r, echo = TRUE}
str(activity) #viewing the structure of the activity data
```
Converting the variable "date" to a date from a factor class of variable and reviewing the change.
```{r, echo = TRUE}
activity$date <- as.Date(activity$date) #changing the date to a date format
str(activity)
```

## What is mean total number of steps taken per day?

### 1.Calculate the total number of steps taken per day

Calculating the sum of steps by date, ignoring NAs.

```{r, echo = TRUE}
sum_steps <-
     activity %>% 
     filter(!is.na(steps)) %>% #removing NAs
     group_by(date) %>% #grouping by date to summarise
     summarise(daily_steps = sum(steps))
head(sum_steps, 10) %>% arrange (desc(daily_steps)) #viewing section of the results by max daily steps
```


### 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

Generating a histogram of this data

```{r, echo = TRUE}
ggplot(sum_steps, aes(x = daily_steps)) +
     geom_histogram( binwidth = 1000, color="darkblue", fill="lightblue") +
     labs(title = "Histogram - Total Steps Taken per Day", x = "Steps per day", y = "Frequency")

```
### 3.Calculate and report the mean and median of the total number of steps taken per day

Using the new table with NA values removed:

Calculating the mean
```{r, echo = TRUE}
mean_daily_steps <- mean(sum_steps$daily_steps) #calculating mean
mean_daily_steps
```
Calculating the median
```{r, echo = TRUE}
median_daily_steps <- median(sum_steps$daily_steps) # calculating median
median_daily_steps
```


## What is the average daily activity pattern?

### 1.Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

Calculating steps per interval, first aggregating steps

```{r, echo = TRUE}
interval_steps <- aggregate(steps ~ interval, 
                            data = activity, FUN = mean, na.rm = TRUE) #aggregating steps by interval
head(interval_steps, 10) # viewing section of results
```

Plotting the aggregated steps by interval
```{r, echo = TRUE, include = TRUE}
ggplot(interval_steps, aes(x = interval, y = steps)) + 
     geom_line(col = "darkblue") + 
     labs(title = "Time Series: Average Daily Activity Pattern - Steps per Interval", x = "Interval", y = "Steps")
```


### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Determining which interval has the max steps
```{r, echo = TRUE}
interval_steps$interval[which.max(interval_steps$steps)]
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA\color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)

Calculating a sum of the rows where there are missing values
```{r, echo = TRUE}
sum(is.na(activity))
```

### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Filling in missing values using the mean for the interval.

### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```{r, echo = TRUE}
new_activity_data <- activity
    for (i in interval_steps$interval) {
        new_activity_data[new_activity_data$interval == i & is.na(new_activity_data$steps), ]$steps <- 
        interval_steps$steps[interval_steps$interval == i]
} #imputing the NA values with the mean interval steps
head(new_activity_data, 10)
```
Checking that we have replaced all mising values.

```{r, echo = TRUE}
sum(is.na(new_activity_data))
```

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r, echo = TRUE}
sum_new_steps <-
    new_activity_data %>% 
    filter(!is.na(steps)) %>% 
    group_by(date) %>% 
    summarise(new_daily_steps = sum(steps))
sum_new_steps
```

Generating a histogram of this data

```{r, echo = TRUE, include = TRUE}
ggplot(sum_new_steps, aes(x = new_daily_steps)) +
    geom_histogram( binwidth = 1000, color="darkblue", fill="lightblue") +
    labs(title = "Histogram - Total Steps Taken per Day", x = "Steps per day", y = "Frequency")
```



Calculating the mean to find the impact of imputing the missing data on the mean
```{r, echo = TRUE}
mean_new_daily_steps <- mean(sum_new_steps$new_daily_steps)
mean_new_daily_steps
```
Calculating the median to find the impact of imputing the missing data on the median
```{r, echo = TRUE}
median__new_daily_steps <- median(sum_new_steps$new_daily_steps)
median__new_daily_steps
```

The mean and the median values are now the same after imputing in the missing values. The mean is consistent with the mean calculated earlier in the assignment, but the median is not. The data points which have been imputed give more data points equal to the mean, with less variation, meaning that the median will also be more consistent/the same.


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays()\color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Adding a variable and converting new variable to a factor.

```{r, echo = TRUE}
weekday_data <- new_activity_data %>% 
     mutate (day_type = as.factor(ifelse(weekdays(date) %in% #creating a new variable
     c("Saturday", "Sunday"), "Weekend", "Weekday")))
head(weekday_data,10) # viewing a section of the data
```


### 2.Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r, echo = TRUE, include = TRUE}
weekday_steps <- aggregate(steps ~ interval + day_type, data = weekday_data, FUN = mean)
ggplot(weekday_steps, aes(x = interval, y = steps)) + 
   geom_line(col = "darkblue") + 
   facet_wrap(~ day_type, nrow=2) + 
   labs(title = "Time Series: Average Steps Taken by Day Type", x = "Interval", y = "Steps")
```

