# Reproducible-Research

##Step 1
##Code for reading in the dataset and/or processing the data
```{r, echo = TRUE}
setwd("E:/1UnderGraduate/COURSERA OTHER COURSES/DATA SCIENCE SPECIALIZATION/5- REPRODUCIBLE RESEARCH/WEEK 2/peer graded assignment/work/RepData_PeerAssessment1")

activity <- read.csv("activity.csv")
```

##Step 2
##Histogram of the total number of steps taken each day
```{r, echo = TRUE}
library(ggplot2)
Q2<-data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
Q2$date<-rownames(Q2)
rownames(Q2)<-NULL
names(Q2)[[1]]<-"Total Steps"

#Total Steps by date bar chart
ggplot(Q2,aes(y=Q2$`Total Steps`,x=Q2$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
```
![alt text](https://github.com/hashaam007/RepData_PeerAssessment1/blob/master/plot2.png)

```{r, echo = TRUE}
ggplot(Q2,aes(y=Q2$`Total Steps`,x=Q2$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")

#Histogram of total steps
qplot(Q2$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
qplot(Q2$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
```

![alt text](https://github.com/hashaam007/RepData_PeerAssessment1/blob/master/plot2.1.png)

##Step 4
##Time series plot of the average number of steps taken
```{r, echo = TRUE}
Q4<-Q3
Q4$date<-as.Date(Q4$date,format="%Y-%m-%d")
ggplot(Q4,aes(x=Q4$date,y=Q4$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean Steps Every day")+xlab("Date")+ggtitle("Mean Steps by Date")
ggplot(Q4,aes(x=Q4$date,y=Q4$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean Steps Every day")+xlab("Date")+ggtitle("Mean Steps by Date")
```
![alt text](https://github.com/hashaam007/RepData_PeerAssessment1/blob/master/plot4.png)

## Step 7
##Histogram of the total number of steps taken each day after missing values are imputed

```{r, echo = TRUE}
qplot(Q6.6$Steps,geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")
dev.off()
qplot(Q6.6$Steps,geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")
```
![alt text](https://github.com/hashaam007/RepData_PeerAssessment1/blob/master/plot7.png)

## Step 8
##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, echo = TRUE}
Q8<-Q6.6
levels(Q8$weekday)<-c(1,2,3,4,5,6,7)
Q8$WDWE<-Q8$weekday %in% c(1,2,3,4,5)
Q8.1<-aggregate(data=Q8,Steps~interval+WDWE,mean,na.rm=TRUE)
Q8.1$WDWE<-as.factor(Q8.1$WDWE)
levels(Q8.1$WDWE)<-c("Weekend","Weekday")

ggplot(data=Q8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")

ggplot(data=Q8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")
```
![alt text] (https://github.com/hashaam007/RepData_PeerAssessment1/blob/master/plot8.png)

#Producing the panel plot
```{r, echo = TRUE}
Q8.1$interval<-as.numeric(as.character(Q8.1$interval))
library(lattice)
xyplot(data=Q8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
library(hexbin)
hexbinplot(data=Q8.1,Steps~interval|WDWE, aspect = 1, bins=50)
```
![alt text] (https://github.com/hashaam007/RepData_PeerAssessment1/blob/master/plott8.1.png)

```{r, echo = TRUE}
xyplot(data=Q8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
hexbinplot(data=Q8.1,Steps~interval|WDWE, aspect = 1, bins=50)
```
![alt text] (https://github.com/hashaam007/RepData_PeerAssessment1/blob/master/plot8.2.png)

