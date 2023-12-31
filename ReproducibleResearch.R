#Assignment Reproducible Research

#Loading and preprocessing
data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")

#convert date to date data type
data$date <- as.Date(data$date) 

#Plot histogram of the total number of steps taken each day:
data.ignore.na <- na.omit(data) 

# sum steps by date
daily.steps <- rowsum(data.ignore.na$steps, format(data.ignore.na$date, '%Y-%m-%d')) 
daily.steps <- data.frame(daily.steps) 
names(daily.steps) <- ("steps") 
hist(daily.steps$steps, 
     main=" ",
     breaks=10,
     xlab="Total Number of Steps Taken Daily")
mean(daily.steps$steps); 
median(daily.steps$steps) 

library(plyr)
# Calculate average steps for each of 5-minute interval during a 24-hour period
interval.mean.steps <- ddply(data.ignore.na,~interval, summarise, mean=mean(steps))
#Plot time series of the 5-minute interval and the average number of steps taken, averaged across all days

library(ggplot2)
qplot(x=interval, y=mean, data = interval.mean.steps,  geom = "line",
      xlab="5-Minute Interval (military time)",
      ylab="Number of Step Count",
      main="Average Number of Steps Taken Averaged Across All Days"
      )

interval.mean.steps[which.max(interval.mean.steps$mean), ]

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

library(sqldf)
tNA <- sqldf(' 
    SELECT d.*            
    FROM "data" as d
    WHERE d.steps IS NULL 
    ORDER BY d.date, d.interval ') 
## Loading required package: tcltk
NROW(tNA) 

t1 <- sqldf('  
    SELECT d.*, i.mean
    FROM "interval.mean.steps" as i
    JOIN "data" as d
    ON d.interval = i.interval 
    ORDER BY d.date, d.interval ') 

t1$steps[is.na(t1$steps)] <- t1$mean[is.na(t1$steps)]

t1.total.steps <- as.integer( sqldf(' 
    SELECT sum(steps)  
    FROM t1') );

t1.total.steps.by.date <- sqldf(' 
    SELECT date, sum(steps) as "t1.total.steps.by.date" 
    FROM t1 GROUP BY date 
    ORDER BY date') 

daily.61.steps <- sqldf('   
    SELECT date, t1_total_steps_by_date as "steps"
    FROM "t1.total.steps.by.date"
    ORDER BY date') 

#Make a histogram of the total number of steps taken each day.

hist(daily.61.steps$steps, 
     main=" ",
     breaks=10,
     xlab="After Imputate NA -Total Number of Steps Taken Daily")

#Calculate and report the mean and median total number of steps taken per day.

t1.mean.steps.per.day <- as.integer(t1.total.steps / NROW(t1.total.steps.by.date) )
t1.mean.steps.per.day

t1.median.steps.per.day <- median(t1.total.steps.by.date$t1.total.steps.by.date)
t1.median.steps.per.day
#Create a factor variable weektime with two levels (weekday, weekend). The folowing dataset t5 dataset contains data: average number of steps taken averaged across all weekday days and weekend days, 5-min intervals, and a facter variable weektime with two levels (weekday, weekend).

t1$weektime <- as.factor(ifelse(weekdays(t1$date) %in% 
                c("Saturday","Sunday"),"weekend", "weekday"))

t5 <- sqldf('   
    SELECT interval, avg(steps) as "mean.steps", weektime
    FROM t1
    GROUP BY weektime, interval
    ORDER BY interval ')

#Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

library("lattice")
p <- xyplot(mean.steps ~ interval | factor(weektime), data=t5, 
       type = 'l',
       main="Average Number of Steps Taken 
       \nAveraged Across All Weekday Days or Weekend Days",
       xlab="5-Minute Interval (military time)",
       ylab="Average Number of Steps Taken")
print (p)    
