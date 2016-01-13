#Loading the data
data <- read.csv(unz("./RepData_PeerAssessment1/doc/activity.zip", "activity.csv"), header=T, stringsAsFactors=F, check.names=F)
data$date <- as.Date(data$date, format="%Y-%m-%d")
str(data)

#For the mean total number of steps taken per day, as sugested on assignment instructions,
#the missing values in the dataset were ignored.
bydate <- aggregate(steps ~ date, data = data, FUN = "sum", na.rm=T)
hist(bydate$steps, main = "Total Steps per day", xlab = "quantity of steps")
mean(bydate$steps) ; median(bydate$steps)

#Daily activity pattern
by5min <- aggregate(steps ~ interval, data = data, FUN = "mean", na.rm=T)
plot(y = by5min$steps, x = by5min$interval, type = "l", main = "Average steps across all day by 5 minutes interval", xlab="Acumulated 5-minute interval", ylab="Steps mean across all days")
by5min$interval[which.max(by5min$steps)]

#Calculating missing values
sum(is.na(data$steps))
  #missing values in % of the total of observations
  sum(is.na(data$steps))*100 / nrow(data)
#Imputing missing using the polynomial interpolation
require(zoo)
newdata = data
newdata$steps <- na.spline(newdata$steps)
#Histogram, mean and median for the new data
newbydate <- aggregate(steps ~ date, data = newdata, FUN = "sum", na.rm=T)
hist(newbydate$steps, main = "Total Steps per day with the missing values filled", xlab = "quantity of steps")
mean(newbydate$steps) ; median(newbydate$steps)
  #So, filling the missing values reduces the mean and the median of the total number of steps.

#New data including weekdays and weekends
newdata$week <- lapply(newdata$date, function(x) if ( weekdays(x, abbreviate=T)<"sat" )
                        {return("weekday")} else {return("weekend")})
newdata$week <- unlist(newdata$week)

#Ploting by 5-minutes mean aggregated
newby5min <- aggregate(steps ~ interval+week, data = newdata, FUN = "mean", na.rm=T)
library(lattice)
xyplot(steps ~ interval | week, data=newby5min, type = "l", layout = c(1, 2), xlab="5-minute interval",ylab="Average steps")
