---
title: "Reproducible Research: Peer Assessment 1"
author: "Deepak Bhutada"
date: "7/16/2019"
output: html_document
---

# Reproducible Research - Course Project 1

This document analyzes an activity dataset containing number of steps recorded in 5-minute intervals across various days.

It was generated using [Knitr](https://github.com/yihui/knitr)

	install.packages('knitr', dependencies = TRUE)
	library(knitr)
	knit("PA1_template.Rmd", force_v1 = TRUE)

It also includes some sample data output to verify results.


	
## Loading and preprocessing the data

The following code loads the data into a variable `activity`. The script will attempt to download and unzip the file if it doesn't exist inside the work directory.

```{r, echo=TRUE}
#we check if the source file is loaded onto the wd of the reader, if not, we download it and unzip the file:
if(!file.exists("activity.csv")) {
       tempfile <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = tempfile)
        unzip(tempfile)
        unlink(tempfile)
}

activity <- read.csv("activity.csv", header = TRUE)
# To prevent scientific notation
options(scipen = 100)
```

Let's take a look at the data:

```{r, echo=TRUE}
dim(activity)
str(activity)
head(activity)
summary(activity)
```

To prepare the data I'm going to use the `dplyr` package:

```{r, echo=TRUE}
library(dplyr)
```



## What is mean total number of steps taken per day?

```{r, echo=FALSE}
# to show all, use:
# as.data.frame(data)
```

```{r, echo=TRUE}
# Get the total steps per date
steps_by_date <- select(activity, steps, date, interval) %>%
	#filter(!is.na(steps)) %>% # removed filter to include NAs as 0-step days in histogram
    group_by(date) %>% 
    summarize(total_steps = sum(steps, na.rm = TRUE))
dim(steps_by_date)
	
# Plot the histogram
hist(steps_by_date$total_steps, 
	main = "Histogram of total steps per day", 
	xlab = "Total steps per day", 
	ylab = "Frequency [number of days]", 
	breaks = 20,
	border = "cadetblue4",
	col = "cadetblue3"
)
mean_total_steps <- mean(steps_by_date$total_steps)
median_total_steps <- median(steps_by_date$total_steps)
abline(v = mean_total_steps, lwd = 1, lty = 2, col = "red")
abline(v = median_total_steps, lwd = 1, lty = 2, col = "red")
```
        
The mean number of steps taken each day is `r round(mean_total_steps, 0)`, the median number is `r round(median_total_steps, 0)`. 
These numbers are also indicated with the red dashed lines above.
		

		
## What is the average daily activity pattern?

```{r, echo=TRUE}
# Get the average number of steps per interval
steps_by_interval <- select(activity, steps, date, interval) %>% 
	group_by(interval) %>% 
	summarize(average_steps = mean(steps, na.rm = TRUE))
dim(steps_by_interval)
	
plot(steps_by_interval$interval, steps_by_interval$average_steps, type = "l",
	main = "Average steps by 5-minute interval",
	xlab = "Interval",
	ylab = "Average number of steps",
	col = "cadetblue4"
)
max_average_steps <- max(steps_by_interval$average_steps)
max_average_steps_interval <- steps_by_interval[
	steps_by_interval$average_steps == max_average_steps,
]$interval
abline(v = max_average_steps_interval, lwd = 1, lty = 2, col = "red")
abline(h = max_average_steps, lwd = 1, lty = 2, col = "red")
```

The average daily activity pattern is shown in the plot above.
`r max_average_steps_interval` is the 5-minute interval that, on average, contains the maximum number of `r round(max_average_steps)` steps. 
I have also marked this point with the red dashed lines.



## Imputing missing values

Of `r nrow(activity)` total rows, `r nrow(activity[which(is.na(activity$steps)),])` have an NA value instead of a usable number of steps.
My strategy is to replace NA values with the average number of steps for the given 5-minute interval. 
The average number of steps per interval has already been calculated above, so I'm going to look the averages in `steps_by_interval`.

```{r, echo=TRUE}
# Sample of some averages in steps_by_interval:
steps_by_interval[steps_by_interval$interval %in% c(600,1000,2000),]
# Sample of NAs before replacement:
head(activity[which(is.na(activity$steps) & activity$interval %in% c(600,1000,2000)),], n = 30)
# I tried this mutate statement first, but it updated only the first NA, and left the others alone:
# updated_activity <- mutate(activity,
#	steps = ifelse(is.na(steps), 
#		steps_by_interval[steps_by_interval$interval==interval,]$average_steps, 
#		steps))
# Not very elegant, but working solution - loop and replace NAs one by one:
updated_activity <- activity
for(r in 1:nrow(updated_activity)) {
	if (is.na(updated_activity$steps[r])) {
		i <- updated_activity$interval[r];
		avg <- steps_by_interval[steps_by_interval$interval==i,]$average_steps
		
		updated_activity$steps[r] <- avg
	}
}
# Todo: Find a better way
# Sample of NAs after replacement:
head(updated_activity[which(
	updated_activity$date %in% c("2012-10-01", "2012-10-08", "2012-11-01", "2012-11-04", "2012-11-09", "2012-11-10") & 
	updated_activity$interval %in% c(600,1000,2000)),], n = 30)
updated_steps_by_date <- select(updated_activity, steps, date, interval) %>% 
    group_by(date) %>% 
    summarize(total_steps = sum(steps, na.rm = TRUE))
hist(updated_steps_by_date$total_steps, 
	main = "Histogram of total steps per day,\nreplacing NAs with average steps for given interval", 
	xlab = "Total steps per day", 
	ylab = "Frequency [number of days]", 
	breaks = 20,
	border = "cadetblue4",
	col = "cadetblue3"
)
updated_mean_total_steps <- mean(updated_steps_by_date$total_steps)
updated_median_total_steps <- median(updated_steps_by_date$total_steps)
abline(v = updated_mean_total_steps, lwd = 1, lty = 2, col = "red")
abline(v = updated_median_total_steps, lwd = 1, lty = 2, col = "red")
```

The large number of 0-step days disappeared from the histogram after replacing 
the NAs with the average number of steps for a given interval. Also note how mean 
and median number of steps meet up at the same point now.
The mean number of steps taken each day is now `r round(updated_mean_total_steps, 0)` 
(compared to `r round(mean_total_steps, 0)` with NAs), and the median number 
is `r round(updated_median_total_steps, 0)` now (compared to `r round(median_total_steps, 0)` with NAs). 



## Are there differences in activity patterns between weekdays and weekends?

```{r, echo=TRUE, fig.width=10}
activity_with_weekday <- mutate(updated_activity, 
	weekday = weekdays(as.Date(updated_activity$date)),
	weekday_is = as.factor(ifelse(weekday=="Saturday" | weekday=="Sunday", "weekend", "weekday"))
)
# Sample of the extended data
head(select(activity_with_weekday, date, weekday, weekday_is) %>% distinct(date), n = 20)
library(lattice) 
# Get average number of steps per weekday/weekend and interval
summary <- select(activity_with_weekday, steps, date, interval, weekday_is) %>% 
    group_by(weekday_is, interval) %>% 
    summarize(average_steps = mean(steps, na.rm = TRUE))
xyplot(average_steps ~ interval | weekday_is, data = summary, layout = c(1,2), type = "l",
	main = "Average steps by 5-minute interval",
	xlab = "Interval",
	ylab = "Average number of steps",
	col = "cadetblue4"
)
```

Yes, there is a difference in activity patterns between weekdays and weekends.
It looks like the activities start a little later and more gradually on the weekends. The step volume is also a bit higher throughout the day.
On weekdays the step volume appears to be higher and increase quicker (more steeply) in the mornings, but remains a little lower throughout the day.

```{r, echo=TRUE}
# Clean up
rm(list=ls(all=TRUE)) 
```