# Read in the file
# 1.
unzip("activity.zip")
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

# Library to format the dates
library(lubridate)

# 2. 
# Formatting the dates
activity$date <- ymd(activity$date)

# Replacing zeroes with NAs
# activity$steps[activity$steps==0]<-NA

# What is mean total number of steps taken per day?
library(dplyr)

# 1.

daily_steps_sum <- activity %>%
  group_by(date) %>%
  summarise(total_steps=sum(steps, na.rm=TRUE))

# 2
library(ggplot2)
g <- ggplot(daily_steps_sum, aes(x=total_steps))
g + geom_histogram(fill="red", color="grey", binwidth=1000) +
  xlab("Steps") + ylab("Frequency") + ggtitle("Total steps per day")

# 3. mean, median steps taken per day
mean(daily_steps_sum$total_steps, na.rm=TRUE)
median(daily_steps_sum$total_steps, na.rm=TRUE)

# What is the average daily activity pattern?


#timestamps <- data.frame(seq(ymd('2012-10-01'),ymd('2012-10-02'), by="5 min"))
#timestamps <- data.frame(timestamps[-nrow(timestamps),]) 
#timefive <- data.frame(seq(from=0,to=2355,by=5))
#colnames(timestamps) <- "time"

#timestamps$minutes <- hms(timestamps$time)

timeseries <- activity %>%
  group_by(interval) %>%
  summarise(avg=mean(steps, na.rm=TRUE))

plot(timeseries, type="l", main="Average daily steps", ylab="Steps", xlab="Time of day (5 minute intervals)")

timeseries %>% filter(avg==max(avg))

# Imputing missing values

# 1.
sum(is.na(activity))
mean(is.na(activity))

# 2.
blank <- activity %>% group_by(date) %>%
  summarise(NAs = sum(is.na(steps)))

blank_only <- subset(blank, NAs>0)
blank_only <- blank_only$date

activity_adjusted <- activity

# 3. 
for(i in 1:length(blank_only)){
  activity_adjusted[activity_adjusted$date==blank_only[i],1] <- timeseries$avg  
}

# 4.
daily_steps_sum_adjusted <- activity_adjusted %>%
  group_by(date) %>%
  summarise(total_steps=sum(steps, na.rm=TRUE))

g <- ggplot(daily_steps_sum_adjusted, aes(x=total_steps))
g + geom_histogram(fill="red", color="grey", binwidth=1000) +
  xlab("Steps") + ylab("Frequency") + ggtitle("Total steps per day")

mean(daily_steps_sum_adjusted$total_steps, na.rm=TRUE)
median(daily_steps_sum_adjusted$total_steps, na.rm=TRUE)

# Are there differences in activity patterns between weekdays and weekends?

# 1. 
activity_adjusted$day <- weekdays(activity_adjusted$date)

activity_adjusted$day_type <- ifelse(grepl("Saturday", activity_adjusted$day), "weekend",
                                     ifelse(grepl("Sunday", activity_adjusted$day), "weekend", "weekday"))

activity_adjusted$day_type <- as.factor(activity_adjusted$day_type)

weekdayend <- activity_adjusted %>%
  group_by(day_type, interval) %>%
  summarise(avg_steps=mean(steps))

# 2.

g <- ggplot(weekdayend, aes(x=interval, y=avg_steps))
g + geom_line(aes(color=day_type)) + facet_grid(day_type~.) +
  ggtitle("Average steps by day of the week between 10/1/12 and 11/30/12") +
  xlab("Time of day (5 minute intervals)") + ylab("Average steps") 
