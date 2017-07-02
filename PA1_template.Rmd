---
title: "Reproducable Research Assignment 1"
---
=====================================================================================================

We start with downloading the data into a data frame(df)
```{r}
df<-read.csv("activity.csv")
head(df)
str(df)
```
Now that we have familiarized ourselves with the dataset, we will start with the analysis that has been asked of us as part of this assignment 
```{r warning=FALSE}
#We download the library files for the analysis
library(ggplot2)
library(gridExtra)
```

### Histogram of the total number of steps taken each day
```{r}
#we start with saving the sum of all steps each day in a dataframe(dftemp)
dftemp<-aggregate(df$steps, by=list(date=df$date), FUN=sum)
#set NA values to zero
dftemp[is.na(dftemp)]<-0
hist(dftemp$x,main="Histogram of total number of steps per day",  xlab="Total number of steps in a day")
```

### Mean and median number of steps taken each day
```{r}
dfmean<- aggregate(df$steps, by=list(date=df$date), FUN=mean)
colnames(dfmean)<-c("Date","Mean")
summary(dfmean)
dfmedian<- aggregate(df$steps, by=list(date=df$date), FUN=median, na.rm=T)
colnames(dfmedian)<-c("Date","Median")
summary(dfmedian)
```
### Time series plot of the average number of steps taken

```{r,fig.width=15, fig.height=8, echo = TRUE}
mp<-ggplot(dftemp,aes(x=date,y=x))+ geom_point()+geom_line(aes(group=1))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("Steps")+ggtitle("Time series analysis of steps")

mp
```

### The 5-minute interval that, on average, contains the maximum number of steps

```{r echo = TRUE}
dfmeanint<- aggregate(df$steps, by=list(interval=df$interval), FUN=mean,na.rm=T)
colnames(dfmeanint)<- c("Interval","Average.steps")
head(dfmeanint)
dfmeanint$Interval[dfmeanint$Average.steps==max(dfmeanint$Average.steps)]
```
*So the interval with maximum number of steps on average is **835th interval**.*

### Code to describe and show a strategy for inputing missing data

Before we start we check the number of NAs that are present in the original data frame(df)
```{r echo = TRUE}
sum(is.na(df$steps))
```
So there are **2304** NA values in our data set.

*Our strategy for inputing the missing data would be to **take the average value of that specific interval.** We do this by refering to the data frame **dfmeanint**, which we created in the previous segment. This contains the mean value of steps taken each interval*.

```{r echo = TRUE}
# we round the values for steps to whole numbers
dfmeanint$Average.steps<-round(dfmeanint$Average.steps,0)
#Create another dataset(df3) merging the original and the mean interval dataset. 
df3<-merge(df,dfmeanint, by.x="interval", by.y="Interval")
df3$steps[is.na(df3$steps)]<- df3$Average.steps[is.na(df3$steps)]
df$steps<-df3$steps
head(df)
```
Now that we have removed the NA values, we see how many NA values are there in our dataset now.

```{r echo = TRUE}
sum(is.na(df$steps))
```
We see that the NA values have been replaced by the average seps taken in that interval.

### Histogram of the total number of steps taken each day after missing values are inputed

```{r echo = TRUE}
dfsum<- aggregate(df$steps, by=list(date=df$date), FUN=sum)
hist(dfsum$x,main="Histogram of total number of steps per day",  xlab="Total number of steps in a day")

```

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, fig.width=15, fig.height=8, echo = TRUE }
#create a data set with column stating the day of the week for the day
dfweek<-cbind(dfsum,weekdays(as.Date(as.character(dfsum$date))))
colnames(dfweek)<- c("Date","Total.steps","Day")
#create a data frame to save the total steps on each day for weekdays
dfweekday<- dfweek[dfweek$Day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),]
#create a data frame to save the total steps on each day for weekdays
dfweekend<- dfweek[dfweek$Day %in% c("Saturday","Sunday"),]

p1<-ggplot(dfweekday,aes(x=Date,y=Total.steps))+geom_line(aes(group=1))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(label = "Weekdays")
p2<- ggplot(dfweekend,aes(x=Date,y=Total.steps))+geom_line(aes(group=1))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(label = "Weekends")
grid.arrange(p1, p2, ncol = 1, top = "Steps comparison of weekdays and weekends")

```

Thus we complete the assignment. Hope you liked it. Please leave a comment to let me know how i did. Thank you.