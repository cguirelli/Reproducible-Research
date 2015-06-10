# clean all
#closeAllConnections()
rm(list=ls())

library(ggplot2)

# load data
activity_data <- read.csv("activity.csv" , sep = "," , stringsAsFactors = FALSE)
dim(activity_data) # verify size 17568 

# changes date field from character to Date
activity_data$date <- as.Date(paste(activity_data$date), format="%Y-%m-%d")

#
# What is mean total number of steps taken per day?
# 

# 1 - Calculate the total number of steps taken per day
total_steps <- aggregate(activity_data[c("steps")], by=activity_data[c("date")], FUN=sum)

# 2 - If you do not understand the difference between a histogram and a barplot, research 
#     the difference between them. Make a histogram of the total number of steps taken each day
hist(total_steps$steps,
     col = "blue",
     ylim=c(0,40),     
     main="steps per day",
     xlab='steps per day')
grid(col="blue") # grid to make reading easy

# 3- Calculate and report the mean and median of the total number of steps taken per day

summary(total_steps$steps)
# or:
mean(total_steps$steps, na.rm=TRUE)
median(total_steps$steps, na.rm=TRUE)

#
# What is the average daily activity pattern?
# 

# 1 - Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
#     the average number of steps taken, averaged across all days (y-axis)
steps_5_minute <- aggregate(activity_data[c("steps")],
                            by=activity_data[c("interval")],
                            FUN=mean,na.rm=TRUE)

#plot(steps_5_minute,
#     type="l",
#     col="red",
#     lwd=1)
#grid(col="black")
library(ggplot2)
qplot(steps_5_minute$interval,steps_5_minute$steps, geom = c("line")) +
    xlab("5 minutes interval") + 
    ylab("steps") + 
    ggtitle("average number of steps taken averaged across all days ")

# 2 - Which 5-minute interval, on average across all the days in the dataset, contains
#     the maximum number of steps?
summary(steps_5_minute$steps)
steps_5_minute[which.max(steps_5_minute$steps),1]


#
# Imputing missing values
#
# Note that there are a number of days/intervals where there are missing
# values (coded as NA).
# The presence of missing days may introduce bias into some calculations
# or summaries of the data.

# 1 - Calculate and report the total number of missing values in the dataset 
#     (i.e. the total number of rows with NAs)

sum(is.na(activity_data$steps))

# 2 - Devise a strategy for filling in all of the missing values in the dataset.
#     The strategy does not need to be sophisticated. For example, you could use
#     the mean/median for that day, or the mean for that 5-minute interval, etc.

    # replace missing data by interval mean

# 3 - Create a new dataset that is equal to the original dataset but with the
#     missing data filled in.

new_activity_data <- activity_data 
for (i in 1:nrow(new_activity_data)) {
    if (is.na(new_activity_data$steps[i])) {
        new_activity_data$steps[i] <- steps_5_minute[which(new_activity_data$interval[i] 
                                                           == steps_5_minute$interval),]$steps
    }
}

# 4 - Make a histogram of the total number of steps taken each day and Calculate
#     and report the mean and median total number of steps taken per day. Do these 
#     values differ from the estimates from the first part of the assignment? 
#     What is the impact of imputing missing data on the estimates of the total 
#     daily number of steps?

new_total_steps <- aggregate(new_activity_data[c("steps")], by=new_activity_data[c("date")], FUN=sum)
hist(new_total_steps$steps,
     col = "blue",
     breaks=30,   
     main="steps per day",
     xlab='steps per day')
grid(col="blue") # grid to make reading easy
summary(new_total_steps$steps)
# or:
mean(new_total_steps$steps, na.rm=TRUE)
median(new_total_steps$steps, na.rm=TRUE)

# quick comparison - 2 histogram side by side
D1 <- hist(total_steps$steps,  plot=FALSE)$counts
D2 <- hist(new_total_steps$steps,  plot=FALSE)$counts
dat <- rbind(D1, D2)
barplot(dat, beside=TRUE, space=c(0, 0.1), las=2)


# Are there differences in activity patterns between weekdays and weekends?
#
# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
# indicating whether a given date is a weekday or weekend day.

Sys.setlocale("LC_TIME", "English")
weekday <- weekdays(new_activity_data$date)
for (j in 1:length(weekday)) {
    if ((weekday[j] == "Saturday") | (weekday[j] == "Sunday")) 
        weekday[j] = "weekend" else weekday[j] = "weekday"
}
new_activity_data$day_type <- as.factor(weekday)


# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
# interval (x-axis) and the average number of steps taken, averaged across all weekday
# days or weekend days (y-axis). See the README file in the GitHub repository to see an
# example of what this plot should look like using simulated data.

weekday_data <- aggregate(new_activity_data[c("steps")],
                          by=new_activity_data[c("interval","day_type")],
                          FUN=mean)
qplot(interval,
      steps,     
      data = weekday_data,
      geom = c("line"),
      facets = day_type ~ .)