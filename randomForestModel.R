library(randomForest)

config <- config[-23]
speedup = `colnames<-`(speedup,'speedup')
train = cbind(speedup,config)
(modelSpdUp <- randomForest(V1 ~ ., data=train, ntree = 500, na.action = na.omit))
predicted <- predict(modelSpdUp, test)


latencies = `colnames<-`(latencies,'latencies')
trainLantencies = cbind(latencies,config)
(modelLat <- randomForest(V1 ~ ., data=trainLantencies, ntree = 500, na.action = na.omit))
#retirer les valeurs NA de config... config <- config[-23]
predicted <- predict(modelLat, test)