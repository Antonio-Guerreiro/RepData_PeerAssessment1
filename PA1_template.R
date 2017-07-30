################################################################################
# title: 'Reproducible Research: Peer Assessment 1'
# author: "Antonio Guerreiro"
# date: "29/07/2017"
################################################################################
  
################################################################################
#Loading and preprocessing the data.
################################################################################

# 1.  Load the data.

activity <- read.csv("activity.csv")

################################################################################
## What is mean total number of steps taken per day?
################################################################################

# For this part we ignore (remove) the missing values in the dataset.
noNA_activity = na.omit(activity)

# 1.  Make a histogram of the total number of steps taken each day.

hist(aggregate(noNA_activity$steps, list(noNA_activity$date), sum)$x,
    breaks = 10, main = "Hist of the total number of steps taken each day", 
    xlab = "Total number of steps taken each day")

# 2.  Calculate and report the mean and median total number of steps taken per day.

print("Mean total number of steps taken per day:")
asis_mean <- mean(aggregate(noNA_activity$steps, list(noNA_activity$date), sum)$x)
print(asis_mean)
print("Median total number of steps taken per day, without imputing missing values")
asis_median <- median(aggregate(noNA_activity$steps, list(noNA_activity$date), sum)$x)
print(asis_median)

################################################################################
# What is the average daily activity pattern?
################################################################################

# 1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#     and the average number of steps taken, averaged across all days (y-axis).

interval_means <- aggregate(noNA_activity$steps, list(noNA_activity$interval), mean)
plot(interval_means$Group.1, interval_means$x, type = "l", 
     main = "Average number of steps taken for each of the 5-minute interval",
     xlab = "5-minute interval", ylab = "Average number of steps taken")

# 2.  Which 5-minute interval, on average across all the days in the dataset,
#     contains the maximum number of steps?

print("5-min interval which on average across all the days in the dataset, contains the maximum number of steps:")
print("Interval:")
print(interval_means[interval_means$x == max(interval_means$x),]$Group.1)
print("Value:")
print(interval_means[interval_means$x == max(interval_means$x),]$x)

################################################################################
# Imputing missing values.
################################################################################

# 1.  Calculate and report the total number of missing values in the original 
#dataset (i.e. the total number of rows with NAs).

print("Total number of rows with NAs:")
print(nrow(activity[is.na(activity$steps),]))

# 2.  Devise a strategy for filling in all of the missing values in the dataset.

print("Stategy used for filling in all of the missing values in the dataset:")
print("Use the mean value computed previously for that this interval")

# 3. Create a new dataset that is equal to the original dataset
# but with the missing data filled in.

for (n in 1:nrow(activity)) {
  if (is.na(activity$steps[n])) {
    activity$steps[n] <- round(interval_means[interval_means$Group.1 == activity$interval[n], ]$x)
  }
}

print("New head of the dataset:")
print(head(activity))

# 4.  Make a histogram of the total number of steps taken each day and Calculate 
#     and report the mean and median total number of steps taken per day. 
#     Do these values differ from the estimates from the first part of the assignment? 
#     What is the impact of imputing missing data on the estimates 
#     of the total daily number of steps?


#     Make a histogram of the total number of steps taken each day.

hist(aggregate(activity$steps, list(activity$date), sum)$x,
     breaks = 10, main = "Hist of the new total number of steps taken each day", 
     xlab = "New total number of steps taken each day")

#   Calculate and report the mean and median total number of steps taken per day.

print("Mean total number of steps taken per day, inputing missing values:")
new_mean <- mean(aggregate(activity$steps, list(activity$date), sum)$x)
print(new_mean)
print("vs:")
print(asis_mean)
print("Median total number of steps taken per day, inputing missing values:")
new_median <- median(aggregate(activity$steps, list(activity$date), sum)$x)
print(new_median)
print("vs:")
print(asis_median)
print("Estimates are quite similar with our without inputing missing values.")

################################################################################
# Are there differences in activity patterns between weekdays and weekends?
################################################################################

# 1.  Create a new factor variable in the dataset with two levels
#     -- "weekday" and "weekend" indicating whether a given date is a weekday
#     or weekend day.
#     French localized computer.

activity$day <- weekdays(as.Date(activity$date),abbreviate = FALSE)

for (n in 1:nrow(activity)) {
  if (activity$day[n] == "Samedi" || activity$day[n] == "Dimanche") {
    activity$daytype[n] <- "weekend"
  } else {
    activity$daytype[n] <- "weekday"
  }
}
print("New head of the dataset:")
print(head(activity))

# 2.  Make a panel plot containing a time series plot (i.e. `type = "l"`)
#     of the 5-minute interval (x-axis) and the average number of steps taken,
#     averaged across all weekday days or weekend days (y-axis). 

weekend_activity <- activity[activity$daytype == "weekend",]
weekday_activity <- activity[activity$daytype == "weekday",]
print("Head of weekend sub-dataset:")
print(head(weekend_activity))
print("Head of weekday sub-dataset:")
print(head(weekday_activity))

par(mfrow=c(2,1))
plot(
  aggregate(weekend_activity$steps, list(weekend_activity$interval), mean)$Group.1, 
  aggregate(weekend_activity$steps, list(weekend_activity$interval), mean)$x, 
  type = "l", main ="Weekend", xlab = "Interval", ylab = "Average number of steps taken",  ylim=c(0, 250))

plot(
  aggregate(weekday_activity$steps, list(weekday_activity$interval), mean)$Group.1, 
  aggregate(weekday_activity$steps, list(weekday_activity$interval), mean)$x,
  type = "l", main =  "Weekday", xlab = "Interval", ylab = "Average number of steps taken", ylim=c(0, 250))
