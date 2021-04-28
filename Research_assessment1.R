# installing packages
install.packages("knitr")
library(knitr)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

opts_chunk$set(echo = TRUE)

# creating work directory
setwd("F:/Desktop backup 19-03-2021/Research-assessmt")

# reading file
steps <- read.csv("activity - activity.csv")

# Process/transform the data into a format suitable for analysis
steps$date <- as.Date(as.character(steps$date))

# What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day
daySteps <- aggregate(steps ~ date, data = steps, FUN=sum)

hist(daySteps$steps, breaks = length(daySteps$date), angle = 45, col = "blue",
     xlab = "Steps per day",
     main = "Total number of steps taken each day")
rug(daySteps$steps)

# Calculate the mean and median of the total number of steps taken per day:
summary(daySteps$steps)[3:4]

# What is the average daily activity pattern?
intervalSteps <- aggregate(steps ~ interval, data = steps, FUN=mean, na.rm=TRUE)

with( intervalSteps, plot(x = interval, y = steps, type ="l", col = "blue",
                          main = "Average Daily Activity Pattern",
                          xlab = "Interval",
                          ylab = "Number of steps taken"))

intervalSteps[which.max( intervalSteps$steps ),]

# Imputing missing values
summary(steps$steps)[7]

imputedSteps <- steps
imputedSteps[which(is.na(imputedSteps$steps)), "steps"] <- rep(mean(imputedSteps$steps, na.rm=TRUE), times=length(which(is.na(imputedSteps$steps))))

# Calculate the total number of steps taken per day
daySteps2 <- aggregate(steps ~ date, data = imputedSteps, FUN=sum)
# plot the imputed steps
hist(daySteps2$steps, breaks = length(daySteps2$date), angle = 45, col = "blue",
     xlab = "Steps per day",
     main = "Total number of steps taken each day\nwith imputed missing values")
rug(daySteps2$steps)

summary(daySteps2$steps)[3:4]

# Are there differences in activity patterns between weekdays and weekends?
# Create a new variable:
imputedSteps$daytype <- NULL
# Loop troug dataset
for(i in 1:nrow(imputedSteps)){
  # Check if weekday is 6 or 7 (Weekend)
  if(format(imputedSteps[i,2], "%u") %in% c("6","7")){
    imputedSteps[i,"daytype"] <- "weekend" 
  } else {
    imputedSteps[i,"daytype"] <- "weekday" 
  }
}
# make daytype as a factor insted as a character: 
imputedSteps$daytype <- as.factor(imputedSteps$daytype)

# aggregate mean steps  by daytype and interval
daytypeStepsInterval <- aggregate(imputedSteps$steps, 
                                  by = list(interval = imputedSteps$interval,                                                                daytype =imputedSteps$daytype),
                                  FUN=mean)


ggplot(daytypeStepsInterval, aes(x=interval, y=x)) + 
  geom_line(color="blue") + 
  facet_wrap(~ daytype, nrow=2, ncol=1) +
  labs(x="5 min. Interval", y="Number of steps")  + theme_bw()

