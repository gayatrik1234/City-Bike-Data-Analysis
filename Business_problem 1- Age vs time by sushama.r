Data_Feb_2014 <- read.csv("/Users/Sushama/Desktop/Spring 2016/IS690Q Big Data/Project/Citibike data/2014-02 - Citi Bike trip data.csv")

install.packages("plyr")
install.packages("ggplot2")
install.packages("reshape2")

library(plyr)
library(ggplot2)
library(reshape2)

head(Data_Feb_2014)

#To create dataset with start date records as Feb 1, 2016

Data_Feb_2014[Data_Feb_2014$startdate == "2/1/2014",]$startdate

Feb01 <- Data_Feb_2014[Data_Feb_2014$startdate == "2/1/2014",]
Feb02 <- Data_Feb_2014[Data_Feb_2014$startdate == "2/2/2014",]
Feb03 <- Data_Feb_2014[Data_Feb_2014$startdate == "2/3/2014",]
Feb04 <- Data_Feb_2014[Data_Feb_2014$startdate == "2/4/2014",]
Feb05 <- Data_Feb_2014[Data_Feb_2014$startdate == "2/5/2014",]
Feb06 <- Data_Feb_2014[Data_Feb_2014$startdate == "2/6/2014",]
Feb07 <- Data_Feb_2014[Data_Feb_2014$startdate == "2/7/2014",]

agevstripduration <- ddply(Feb01, ~age, summarise, avgtripduration=mean(tripduration))

ggplot(agevstripduration, aes(age, avgtripduration)) + geom_point() + geom_smooth()+ xlab("Age") + ylab("Average Trip Duration") + ggtitle("Feb 2014 - Age group vs tripduration")

#1st Feb - Saturday - Starttime vs age
agevstime <- ddply(Feb01, ~starttime, summarise,avgage=mean(age))

ggplot(agevstime, aes(starttime, avgage)) + geom_point() + geom_smooth()+ xlab("Starttime of trip") + ylab("Average age") + ggtitle("1st Feb 2014 - Saturday - Starttime vs age")

#2nd Feb - Sunday - Starttime vs age
agevstime2 <- ddply(Feb02, ~starttime, summarise,avgage=mean(age))

ggplot(agevstime2, aes(starttime, avgage)) + geom_point() + geom_smooth()+ xlab("Starttime of trip") + ylab("Average age") + ggtitle("2nd Feb 2014 - Sunday - Starttime vs age")

#3rd Feb - Sunday - Starttime vs age
agevstime3 <- ddply(Feb03, ~starttime, summarise,avgage=mean(age))

ggplot(agevstime3, aes(starttime, avgage)) + geom_point() + geom_smooth()+ xlab("Starttime of trip") + ylab("Average age") + ggtitle("3rd Feb 2014 - Monday - Starttime vs age")

#4th Feb - Sunday - Starttime vs age
agevstime4 <- ddply(Feb04, ~starttime, summarise,avgage=mean(age))

ggplot(agevstime4, aes(starttime, avgage)) + geom_point() + geom_smooth()+ xlab("Starttime of trip") + ylab("Average age") + ggtitle("4th Feb 2014 - Tuesday - Starttime vs age")

#5th Feb - Sunday - Starttime vs age
agevstime5 <- ddply(Feb05, ~starttime, summarise,avgage=mean(age))

ggplot(agevstime5, aes(starttime, avgage)) + geom_point() + geom_smooth()+ xlab("Starttime of trip") + ylab("Average age") + ggtitle("5th Feb 2014 - Wednesday - Starttime vs age")

#6th Feb - Sunday - Starttime vs age
agevstime6 <- ddply(Feb06, ~starttime, summarise,avgage=mean(age))

ggplot(agevstime6, aes(starttime, avgage)) + geom_point() + geom_smooth()+ xlab("Starttime of trip") + ylab("Average age") + ggtitle("6th Feb 2014 - Thrusday - Starttime vs age")

#7th Feb - Sunday - Starttime vs age
agevstime7 <- ddply(Feb07, ~starttime, summarise,avgage=mean(age))

ggplot(agevstime7, aes(starttime, avgage)) + geom_point() + geom_smooth()+ xlab("Starttime of trip") + ylab("Average age") + ggtitle("7th Feb 2014 - Friday - Starttime vs age")
