---
title: "PA1_template"
author: "Steven Watson"
date: "Wednesday, April 15, 2015"
output: html_document
keep_md: true
---

This file was created to fulfil the requirements of the Cousera Reproduceable Research course
The R code (commented) with figures is below, but to make it simple I will sum it up at the end...




```r
#created 4-13-15 by Steven Watson for Coursera Reproduceable Research

#Load and process the data
activitydata <- read.csv("./activity.csv") #reads in the data
library(dplyr) #loads dplyr
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#Summarize by day
daysummary<-group_by(activitydata,date) #grouping by day
stepsbyday<- summarize(daysummary,sum(steps)) #summing steps by day
hist(stepsbyday[[2]]) #plotting the histogram of steps by day
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
meansteps<-mean(stepsbyday[[2]], na.rm=TRUE) #shows mean steps by day
mediansteps<-median(stepsbyday[[2]], na.rm=TRUE) #shows median steps by day

#Summarize by time
intervalsummary<-group_by(activitydata,interval) #grouping by interval
intervalsummary2<-na.omit(intervalsummary) #removing NAs
stepsbyinterval<- summarize(intervalsummary2,mean(steps)) #averaging by interval
names(stepsbyinterval)<-sub("\\(","",names(stepsbyinterval)) #removing parenthesis
names(stepsbyinterval)<-sub("\\)","",names(stepsbyinterval))#removing parenthesis
with(stepsbyinterval,plot(interval,meansteps),type="l") #time plot
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png) 

```r
maxsteps<-max(stepsbyinterval$meansteps) #returns max value
maxtime<-stepsbyinterval$interval[which.max(stepsbyinterval$meansteps)] #returns max time

#Impute missing values
missingvalues<- length(which(is.na(activitydata))) #counts NAs
activitydatanew<-activitydata #making a new data frame with no NAs

#for all values, if a value is NA, replace it with the average step value
for(i in 1:17568) {  
    if(is.na(activitydata$steps[i])==TRUE) {
    activitydatanew$steps[i]= mean(activitydata$steps,na.rm=TRUE) 
    }
}

#Summarize new data by day
daysummarynew<-group_by(activitydatanew,date) #grouping by day
stepsbydaynew<- summarize(daysummarynew,sum(steps)) #summing steps by day
hist(stepsbydaynew[[2]]) #plotting the histogram of steps by day
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png) 

```r
meanstepsnew<-mean(stepsbydaynew[[2]], na.rm=TRUE) #shows mean steps by day
medianstepsnew<-median(stepsbydaynew[[2]], na.rm=TRUE) #shows median steps by day

weekdaydatanew<-activitydatanew #making a new data frame for weekdays
weekdaydatanew$date<-as.Date(weekdaydatanew$date) #converting to dates
weekdaydatanew<- mutate(weekdaydatanew, weekday=weekdays(date)) #adding day of week

#converting to weekend/weekday
weekdaydatanew$weekday<- sub("Monday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Tuesday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Wednesday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Thursday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Friday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Saturday","Weekend",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Sunday","Weekend",weekdaydatanew$weekday)

weekdaysummarynew<-group_by(weekdaydatanew,interval) #grouping by interval
avstepsbyweekdaynew<-weekdaysummarynew #new datafram
avstepsbyweekdaynew$mean <- ave(avstepsbyweekdaynew$steps, avstepsbyweekdaynew$interval, FUN=mean) #adding in means

library(lattice) #load lattice

xyplot(mean ~ interval | weekday, avstepsbyweekdaynew) #plotting mean steps by period for weekday and weekend
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png) 

Making your grading easy:

The first image is histogram of steps by day.

The mean is 'r rmeansteps' and the median is 'r mediansteps'.

The second image is a time series plot by interval (averaged by days).

The five minute interval that is the most is 'maxtime' with 'maxsteps' steps.

Missing data was given the average step value across all observations.

The third image is a histrogram of steps by day WITH missing values replaced.

The fourth image is a panel plot looking at weekend vs weekday values.

All code is contained above and commented.


