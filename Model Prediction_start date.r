Data_Feb_2014 <- read.csv("/Users/Sushama/Desktop/Spring 2016/IS690Q Big Data/Project/Citibike data/2014-02 - Citi Bike trip data.csv")

birth.year.data <- ddply(Data_Feb_2014, ~birth.year, summarise, avgtripduration=mean(tripduration))

trainingPositions <- sample(nrow(birth.year.data),size = floor(nrow(birth.year.data) * 0.7))

training <- birth.year.data[trainingPositions,]
test <- birth.year.data[-trainingPositions,]

##############################Linear Model############

Model1 <- lm(avgtripduration ~ birth.year, birth.year.data)

Model1

prediction <- predict(Model1, test)
print(prediction)

result <- as.data.frame(cbind(test$birth.year, test$avgtripduration, prediction))
colnames(result) <- c("birth.year", "Actual", "Prediction")
print(head(result))
resultMelted <- melt(result, id="birth.year")
ggplot(data=resultMelted, aes(x=birth.year, y=value, colour=variable)) + geom_line()

####################Exponential Model#############
Model2 <- lm(log(avgtripduration) ~ start.station.id, start.station.data)

Model2

prediction <- exp(predict(Model2, test))
print(prediction)

result <- as.data.frame(cbind(test$start.station.id, test$avgtripduration, prediction))
colnames(result) <- c("start.station.id", "Actual", "Prediction")
print(head(result))
resultMelted <- melt(result, id="start.station.id")
ggplot(data=resultMelted, aes(x=start.station.id, y=value, colour=variable)) + geom_line()