# Reproducible Research: Peer Assessment 1
Alexander Gedranovich

## Loading and preprocessing the data
Loading libraries needed for data transformation and analysis:

```r
library(dplyr)
library(lubridate)
library(ggplot2)
```


Reading the data and demonstrating its' content:

```r
df <- read.table("data/activity.csv", sep = ",", header = T)
head(df)
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



## What is mean total number of steps taken per day?


## What is the average daily activity pattern?


## Imputing missing values


## Are there differences in activity patterns between weekdays and weekends?

