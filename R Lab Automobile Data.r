vehicles <- read.csv("/Users/Sushama/Desktop/Spring 2016/IS690Q Big Data/Assignment/Assignment 8/vehicles.csv", stringsAsFactors = FALSE)

head(vehicles)

head(vehicles, 10)

#Let's see the number of rows and columns
nrow(vehicles)
ncol(vehicles)

names(vehicles)

#Let's find out how many unique years of data exists in the dataset
length(unique(vehicles$year))

min(vehicles$year)
max(vehicles$year)

#How many different fuel types exists in the dataset?
table(vehicles$fuelType)

#Lets sort and see it again
sort(table(vehicles$fuelType))

#Let's plot mpg by year and see how it changes throughout the years. First we need to get mpg for each years.
mpgByYr <- ddply(vehicles, ~year, summarise, avgMPG = mean(comb08), avgHgwy = mean(highway08), avgCity = mean(city08))

class(mpgByYr)

ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") +ylab("Average MPG") + ggtitle("All cars")

#Let's just look at just gasoline cars. We need to get only gasoline gas first.
gasCars <- subset(vehicles, fuelType1 %in% c("Regular Gasoline", "Premium Gasoline","Midgrade Gasoline") & fuelType2 == "" & atvType != "Hybrid")

mpgByYr_gas <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08), avgHgwy = mean(highway08), avgCity = mean(city08))

ggplot(mpgByYr_gas, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("All cars")

#
ggplot(gasCars, aes(displ, comb08)) + geom_point() + geom_smooth()

#

avgCarSize <- ddply(gasCars, ~year, summarise, avgDispl=mean(displ))

head(avgCarSize)

ggplot(avgCarSize, aes(year, avgDispl)) + geom_point() + geom_smooth()

#
byYear <- ddply(gasCars, ~year, summarise, avgMPG=mean(comb08), avgDispl=mean(displ))

head(byYear)

byYear2 <- melt(byYear, id="year")
levels(byYear2$variable) <- c("Average MPG", "Average Engine Displacement")

ggplot(byYear2, aes(year, value)) + geom_point() + geom_smooth() + facet_wrap(~variable, ncol = 1, scales = "free_y") + xlab("Year") + ylab("")

#Prediction  1
sort(table(vehicles$make))

vehicles[vehicles$make == "Volvo",]$model

volvo <- vehicles[vehicles$make == "Volvo",]

volvo <- vehicles[vehicles$make == "Volvo",][, c("year", "comb08")]

mpgData <- ddply(volvo, ~year, summarise, avgMPG=mean(comb08))

ggplot(mpgData, aes(year, avgMPG)) + geom_point() + geom_smooth()

#prediction 2

trainingPositions <- sample(nrow(mpgData), size = floor(nrow(mpgData) * 0.7))
training <- mpgData[trainingPositions,]
test <- mpgData[-trainingPositions,]

model <- lm(avgMPG~year, training)

prediction <- predict(model, test)
print(prediction)

result <- as.data.frame(cbind(test$year, test$avgMPG, prediction))
colnames(result) <- c("Year", "Actual", "Prediction")
print(result)

resultMelted <- melt(result, id="Year")
ggplot(data=resultMelted, aes(x=Year, y=value, colour=variable)) + geom_line()

exponential.model <- lm(log(avgMPG)~ year, training)
prediction <- predict(exponential.model, test)
print(prediction)

