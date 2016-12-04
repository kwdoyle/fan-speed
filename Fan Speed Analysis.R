library(ggplot2)
library(plyr)
library(scales)

data <- read.csv("fanspeeds.csv", header = F)

# Remove timezone info, since I can't convert that in the timestamp
data[,1] <- sub("EDT ", "", data[,1])
data[,1] <- sub("EST ", "", data[,1])
#test <- sub("EDT ", "", data[1,1])

# plotting as all in same day together
date <- as.Date(strptime(data[,1], format="%a %b %d %H:%M:%S %Y"))
date2 <- strptime(data[,1], format="%a %b %d %H:%M:%S %Y")
#data[,3] <- strptime(data[,1], format="%a %b %d %H:%M:%S %Y")


plot(date[1:6], data[,2][1:6])
plot(seq(1,6), data[,2][1:6], xaxt="n")
axis.POSIXct(1, date[1:6])

# Adding timestamped dates to data frame
data$Date <- date

# add colum names to dataset
colnames(data) <- c("textDate", "speed", "Date")


# try to average all from same day

days <- unique(data[,3])

#avgs <- c()
#for (i in nrow(data)) {
#  for (j in length(days)) {
#    tmp <- c()
#    if (data[i,3] == days[j]) {
#      tmp <- append(tmp, data[i,2])
#      # save the nums & then take the average for each day.
#    }
#  }
#}

# Test
#test.data <- head(data, 6)
#colnames(test.data) <- c("oldDate", "speed", "goodDate")

# can do this instead to take means of speeds from same day
#aggregate(test.data$speed, by=list(test.data$goodDate), FUN="mean")

# by extension, this should work for the whole dataset
avg.by.day <- aggregate(data$speed, by=list(data$Date), FUN="mean")
colnames(avg.by.day) <- c("Date", "AvgSpeed")



# plot average fan speeds per day
ggplot(avg.by.day, aes(x=avg.by.day$Date, y=avg.by.day$AvgSpeed)) + geom_line() + 
  theme_bw() + labs(x="Day", y="Fan Speed (rpm)") + 
  ggtitle("Average Fan Speed Per Day") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-01-26")), colour="El Cap")) + #red
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-04-13")), colour="New Mac")) + #blue
  scale_x_date(labels=date_format("%m/%d"), breaks=date_breaks("4 weeks")) +
  scale_colour_manual("", 
                      breaks = c("El Cap", "New Mac"),
                      values = c("red", "blue"))



# Need to determine "arbitrary" seasonal groups of Summer, Fall, Winter

# These seasonal groups are determined by the calendar. Should probably try to look at the temperatures through the past year and make groups based on that.
summer <- subset(avg.by.day, Date >= "2015-07-30" & Date < "2015-09-23")
fall <- subset(avg.by.day, Date >= "2015-09-23" & Date < "2015-12-22")
winter <- subset(avg.by.day, Date >= "2015-12-22" & Date < "2016-03-20")

Season.df <- data.frame("Season"=rep("summer", nrow(summer)), "AvgSpeed"=summer$AvgSpeed)
Season.df <- rbind.data.frame(Season.df, data.frame("Season"=rep("fall", nrow(fall)), "AvgSpeed"=fall$AvgSpeed))
Season.df <- rbind.data.frame(Season.df, data.frame("Season"=rep("winter", nrow(winter)), "AvgSpeed"=winter$AvgSpeed))


# Boxplot of Seasonal Effect
boxplot(Season.df$AvgSpeed ~ Season.df$Season, ylab="Fan Speed (rpm)", main="Seasonal Effect on \n Fan Speed")

ggplot(Season.df, aes(Season.df$Season, Season.df$AvgSpeed)) + geom_boxplot() +
  theme_bw() + labs(x="Season") + ggtitle("Seasonal Effect on Fan Speed") +
  scale_y_continuous("Fan Speed (rpm)", limits=c(1500, 5000))



# Time to do some model fitting!
fit <- lm(Season.df$AvgSpeed ~ Season.df$Season - 1)  # makes no sense to remove the intercept in this case. the model isn't expected to go through the origin.
fit2 <- lm(Season.df$AvgSpeed ~ Season.df$Season)
summary(fit)

# Season has an effect?!?!?!
anova(fit)
anova(fit2)


# Fall & summer are diff; winter had higher fan speeds than fall too--might be due to other (counfounding) factors.
TukeyHSD(aov(Season.df$AvgSpeed ~ Season.df$Season))




#scale_x_date(labels = date_format("%m/%d"))
#scale_x_date(date_labels = "%Y-%m-$d")
#+ geom_point(avg.by.day, aes(x=avg.by.day$Date[50], y=avg.by.day$AvgSpeed[50])) #geom_vline(xintercept=20)






### More testing stuff ###


# Ugh
#ggplot(head(data, 6), aes(x=data[,3][1:6], y=data[,2][1:6])) + geom_line()

#dumb <- data.frame(seq(1, nrow(data)), data[,2])
#ggplot(dumb, aes(x=dumb[,1], y=dumb[,2]))
#plot(dumb[,1], dumb[,2], type="l")


# this might be plotting all points from same day "together"?
#plot(data[,3], data[,2], type="l")

#test.dates <- c()
#for (i in 1:5) {
#  stamp <- strptime(data[,1][i], format="%a %b %d %H:%M:%S %Y")
#  #print(stamp)
#  test.dates <- append(test.dates, stamp)
  #test.dates <- c(test.dates, strptime(data[,1][i], format="%a %b %d %H:%M:%S %Y"))
#}
#test.dates



#x <- c()
#for (i in 1:5) {
#  poo <- strptime(data[,1][i], format="%a %b %d %H:%M:%S %Y")
#  x <- append(x, poo)
#}
#print(x)



#date <- strptime(test, format="%a %b %d %H:%M:%S %Y")
#strptime("Thu Jul 30 21:32:43 2015", format="%a %b %d %H:%M:%S %Y")

#ggplot(data.frame(data[1:5,2]), aes(x=data[1:5,1], y=data[1:5,2]))