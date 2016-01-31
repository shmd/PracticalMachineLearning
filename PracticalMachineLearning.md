# Use \\ for Windows
setwd("D:\\Data\\Coursera\\01 Practical Machine Learning\\Week 4\\Assignment")

library(caret)
set.seed(11107)


trainingData <- read.csv("pml-training.csv", header=T, na.strings=c("NA", "#DIV/0!"))
testingData  <- read.csv("pml-testing.csv",  header=T, na.string=c("NA", "#DIV/0!"))

dim(trainingData)

dim(testingData)


processedTrainingData <- trainingData
processedTrainingData <- processedTrainingData[, unlist(lapply(processedTrainingData, function(x) !any(is.na(x))))]

dim(processedTrainingData)

processedTestingData <- testingData
processedTestingData <- processedTestingData[, unlist(lapply(processedTestingData, function(x) !any(is.na(x))))]


# remove uncessecary stuff
processedTrainingData <-processedTrainingData[,-c(1:7)]
processedTestingData  <-processedTestingData [,-c(1:7)]


# Use 70 % for training, and 30% for cross validation

inTrain<-createDataPartition(processedTrainingData$classe, p=0.7, list=F)
training <- processedTrainingData[inTrain,]
testing  <- processedTrainingData[-inTrain,]



### measure the time to train, I didn't have time to look for alternatives. It might take up to 5 minutes to train... (coffee break)

start.time <- Sys.time()

ctrl  <- trainControl(allowParallel=T, method="cv", number=4)
model <- train(classe ~ ., data=training, model="rf", trControl=ctrl)
pred  <- predict(model, newdata=testing)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



sum(pred == testing$classe) / length(pred)

confusionMatrix(testing$classe, pred)$table

## the result to submitt
predict(model, newdata=processedTestingData)

varImp(model)


