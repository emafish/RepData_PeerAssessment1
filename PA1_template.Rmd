---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
library(data.table)
df <- read.csv("activity.csv")

## What is mean total number of steps taken per day?
library(dplyr)
total_steps <- aggregate(steps ~ date, FUN = sum, data = df)
png("instructions_fig/plot1.png", width = 480, height = 480)
  hist(total_steps$steps)
dev.off()
mean(total_steps$steps)
median(total_steps$steps)

## What is the average daily activity pattern?
tmp <- summarize(group_by(df, interval), mean(steps, na.rm = T))

png("instructions_fig/plot2.png", width = 480, height = 480)
  
  plot(tmp, type = "l", 
       main = "Average across all Days",
       xlab = "5-minute Interval",
       ylab = "Average Number of Steps Taken")
  
dev.off()

tmp <- summarize(group_by(df, interval), mean(steps, na.rm = T))
summary(tmp)
## interval      mean(steps, na.rm = T)
## Min.   :   0.0   Min.   :  0.000       
## 1st Qu.: 588.8   1st Qu.:  2.486       
## Median :1177.5   Median : 34.113       
## Mean   :1177.5   Mean   : 37.383       
## 3rd Qu.:1766.2   3rd Qu.: 52.835       
## Max.   :2355.0   Max.   :206.170 

filter(tmp, tmp$`mean(steps, na.rm = T)` > 206)
## Interval 835 contains the maximum number of steps.

## Imputing missing values

summary(df)
## missing value = 2304 rows
library(tidyr)
## replace NA with mean for that 5-minute interval
new <- replace_na(df, replace = list(steps = tmp$'mean(steps, na.rm = T)'))
write.csv(new, "padded.csv", row.names = F)
## Histogram of total number of steps take each day
png("instructions_fig/plot3.png", width = 480, height = 480)
  tmp <- aggregate(steps ~ date, FUN = sum, data = new)
  hist(tmp$steps)
dev.off()
mean(tmp$steps)
median(tmp$steps)
## Are there differences in activity patterns between weekdays and weekends?
library(lubridate)
x <- sub('2', 'weekday',
        sub('3', 'weekday',
            sub('4', 'weekday',
                sub('5', 'weekday',
                    sub('6', 'weekday',
                        sub('7', 'weekend', 
                            sub('1', 'weekend',
                                wday(new$date))))))))
weekdays <- bind_cols(new, data.table(weekdays = x))

png("instructions_fig/plot4.png", width = 480, height = 480)
  par(mfrow = c(1,2))
  weekdayfilter(new, weekdays == "weekday")
    plot(tidyData4$tidyTime, tidyData4$Global_active_power, 
       type = "l", 
       ylab = "Global Active Power (kilowatts)", 
       xlab = "")
  
  plot(tidyData4$tidyTime, tidyData4$Voltage, 
       type = "l", 
       ylab = "Voltage", 
       xlab = "datetime")
    legend("topright", 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
         col = c("black", "red", "blue"), 
         lty = c(1,1))
dev.off()
