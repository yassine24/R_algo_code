#install.packages("e1071")
#install.packages("hydroGOF")

#Load Library for rmse
library(hydroGOF)
library(MLmetrics)
#library(caret)
#Load Library for svm
library(e1071)

latencies <- read.table("~/Bureau/Rtest/latencies.txt", quote="\"", comment.char="")
speedup <- read.table("~/Bureau/Rtest/speedup.txt", quote="\"", comment.char="")

#Read data from .csv file
data <- read.csv("~/Bureau/Rtest/config.txt", header=F)
data <- data[-22]
head(data)

#Scatter Plot
#plot(data$speedup, main ="Scatter Plot")

speedup = `colnames<-`(speedup,'speedup')
train = cbind(speedup,data)

data = train[c(1:5),]
spd = speedup[c(1:5),]

test = train[c(6:10),]
             
#Regression with SVM
modelsvm = svm(spd~., data,na.action = na.omit)

#Predict using SVM regression
predYsvm = predict(modelsvm, test$speedup)

#Overlay SVM Predictions on Scatter Plot
#plot(data$speedup, predYsvm, col = "red", pch=16)

#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho

#Calculate RMSE 
RMSEsvm= hydroGOF::rmse(predYsvm,test$speedup)
rsquare = caret::R2(predYsvm,test$speedup)
mape = MLmetrics::MAE(predYsvm,test$speedup)

ape = function(pred,value){
  t= (pred-value)/value 
  r = abs(t)
  return (r*100)
}

ape(predYsvm,test$speedup)

