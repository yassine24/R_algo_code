library(som)

latencies <- read.table("~/Bureau/Rtest/latencies.txt", quote="\"", comment.char="")
speedup <- read.table("~/Bureau/Rtest/speedup.txt", quote="\"", comment.char="")
config <- read.csv("~/Bureau/Rtest/config.txt", header=FALSE)
config <- config[-22]

param_value_min_max <- read.csv("~/Bureau/Rtest/param_value_min_max.txt", header=FALSE)

spd = rbind(speedup,446.6279589102278)
ltc = rbind(latencies,2051.334305363)
v = rbind(spd,ltc)

strMin <-c(0.10,4294967296,5,16,65536,0,0,0.0,0.0,67108864,4,0,24,65536,0.10,0.10,20,4,1,4,3)
strMax <- c(0.60,10737418240,12,48,655360,1,1,1.0,1.0,536870912,20,1,72,655360,0.60,0.60,80,20,5,20,6)

normalised_data = (v - min(v)) / ( max(v) - min(v) )

x0 <- normalised_data[11,]
y0 <- normalised_data[22,]

library(randomForest)

speedup = `colnames<-`(speedup,'speedup')
train = cbind(speedup,config)
(modelSpdUp <- randomForest(speedup ~ ., data=train, ntree = 500, na.action = na.omit))

latencies = `colnames<-`(latencies,'latencies')
trainLantencies = cbind(latencies,config)
(modelLat <- randomForest(latencies ~ ., data=trainLantencies, ntree = 500, na.action = na.omit))

myNames = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21")


d <- function(e){
  p = as.data.frame(t(e))
  colnames(p) <- myNames
  
  supplemental_data_frame <- data.frame(t(speedup[1,]))
  colnames(supplemental_data_frame) <- c("speedup")
  supplemental_data_frame;
  
  s = cbind(supplemental_data_frame,p)
  
  supplemental_data_frame <- data.frame(t(latencies[1,]))
  colnames(supplemental_data_frame) <- c("latencies")
  supplemental_data_frame;
  
  l = cbind(supplemental_data_frame,p)
  #appelez randomforest ici et donnée les param reçu dans la variable e
  x = predict(modelSpdUp,s) 
  y = predict(modelLat,l)
  
  spd = rbind(spd,x)
  ltc = rbind(ltc,y)
  n = rbind(spd,ltc)
  
  normalised_data = (n - min(n)) / ( max(n) - min(n) )
  
  x = normalised_data[12,] 
  y = normalised_data[23,]
  
  v <- sqrt( (0.5*(x-x0)**2) + (0.5*(y-y0)**2) )
  returnValue(v)
}

GAM = genalg::rbga(stringMin = strMin, stringMax = strMax, popSize = 200,iters = 100,
             mutationChance = 0.01,suggestions = NULL,elitism=40,showSettings = FALSE, evalFunc = 
               d,verbose = F)


summary(GAM,echo=TRUE)


