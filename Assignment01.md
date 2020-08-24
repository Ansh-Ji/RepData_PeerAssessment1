---
title: "Peer Graded Assigmnent"
author: "Janish Parikh"
date: "25/08/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Reproducible Research Assignment 01


```{r,  message=FALSE}
library('dplyr')
library('lubridate')
library('ggplot2')
```

1. *Code for reading in the dataset and/or processing the data*
```{r , cache=TRUE}
activity_data<-as.data.frame(read.csv("activity.csv", header = T, comment.char = ""))
head(activity_data)

```
    We can observe NA values in steps so we remove them and change class of attribute date from character to date
```{r}
#
missing_data<-is.na(activity_data$steps)
complete_activity_data<-activity_data[missing_data==F,]
complete_activity_data$date<-ymd(complete_activity_data$date)
head(complete_activity_data)
```

2. *Histogram of the total number of steps taken each day*
```{r, warning=FALSE}
aggregate_data<-aggregate(complete_activity_data$step,by =    
                            list(complete_activity_data$date), FUN = sum )
colnames(aggregate_data)<-c("Date","Total No of Steps")

ggplot(aggregate_data,aes(y=aggregate_data$`Total No of Steps`,x=aggregate_data$Date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")

qplot(aggregate_data$`Total No of Steps`, geom = "histogram", 
      fill=as.factor(weekdays(aggregate_data$Date)), bins =20)+
      ggtitle("Histogram of total no of steps taken ")+
      labs(x="No of steps", y ="Frequency", fill = "Day of the week")
```

3. *Mean and median number of steps taken each day*
```{r}
Mean <- round(mean(aggregate_data$`Total No of Steps`), digits = 2)
Median<-median(aggregate_data$`Total No of Steps`)
```
  Mean no of steps taken each day `r Mean`
    Median no of steps taken each day `r Median`

4. *Time series plot of the average number of steps taken*
```{r}
aggregate_data1<-aggregate(complete_activity_data$steps, 
                           by = list(complete_activity_data$date), FUN = mean)
colnames(aggregate_data1)<-c("Date","Average no of steps taken")
plot(aggregate_data1,pch =19, main="Time series plot of average no of steps taken", 
     ylab ="Average no of steps", xlab = "Date", type='l', lty=6 , col="darkblue")
```


5. *The 5-minute interval that, on average, contains the maximum number of steps*
```{r}
aggregate_data2<-aggregate(complete_activity_data$steps, 
                           by = list(complete_activity_data$interval), FUN = mean)
colnames(aggregate_data2)<-c("Interval","Average no of steps taken")
aggregate_data2[which.max(aggregate_data2$`Average no of steps taken`),]$Interval
```

6. *Code to describe and show a strategy for imputing missing data*

- We have a total of 2304 missing values all belonging to attribute steps
- First let us see the no of missing values spread over date & interval
- Therefore an imputing strategy must be devised to replace all of these missing values with usable numeric measurements. To do so, I decided to replace each missing value with the mean value for the same interval, averaged across all days.
-I used a for loop to achieve this, first testing if each observation was an NA value, and if so, replacing it with the mean average for that interval, (as calculated in a previous question).

```{r}
imputedData <- activity_data
imputedData$date<-ymd(imputedData$date)

for(x in 1:17568) {
    if(is.na(imputedData[x, 1])==TRUE) {
        imputedData[x, 1] <- aggregate_data2[aggregate_data2$Interval 
                                             %in% imputedData[x, 3], 2]
    }
}

head(imputedData)
```

7. *Histogram of the total number of steps taken each day after missing values are imputed*
```{r, warning=FALSE}
aggregate_data4<-aggregate(imputedData$step,
                           by = list(imputedData$date), FUN = sum )
colnames(aggregate_data4)<-c("Date","Total No of Steps")

ggplot(aggregate_data4,aes(y=aggregate_data4$`Total No of Steps`,x=aggregate_data4$Date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")

qplot(aggregate_data4$`Total No of Steps`, geom = "histogram", 
      fill=as.factor(weekdays(aggregate_data4$Date)), bins =20)+
      ggtitle("Histogram of total no of steps taken daily after imputing missing    
              values")+
      labs(x="No of steps", y ="Frequency", fill = "Day of the week")
```

8. *Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends*
```{r}
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
  imputedData$datetype <- sapply(imputedData$date, 
        function(x) {
                if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
head(imputedData)
```

```{r}
activityData2Interval <- aggregate(steps ~ interval + datetype, data= imputedData, 
                                   mean)
ggp <- ggplot(activityData2Interval, aes(interval, steps, colour=datetype)) +
        geom_line(linetype ='solid' , size =1.25) +
        facet_grid(datetype ~ .) +
        labs(title = "Frequency Distribution of Step Activity",
            y="Average no of steps in 5-minute interval",
            x="The ID of the 5 minute interval") 
print(ggp)

```