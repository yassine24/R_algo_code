library(som)
library("rjson")
latencies <- fromJSON(file = "~/Bureau/result/train2/latenciesa.txt")
speedup <- fromJSON(file = "~/Bureau/result/train2/speedupa.txt")
config <- fromJSON(file = "~/Bureau/result/train2/config.txt")

#param_value_min_max <- read.csv("~/Bureau/Rtest/param_value_min_max.txt", header=FALSE)

spd = rbind(speedup,446.6279589102278)
ltc = rbind(latencies,2051.334305363)
v = rbind(spd,ltc)

strMin <-c(4,4,5,16,20,0.10,0.10,0.10,8)
strMax <- c(32,20,12,48,80,0.60,0.60,0.60,20)

normalised_data = (v - min(v)) / ( max(v) - min(v) )

x0 <- tail(normalised_data,n=1)
y0 <- tail(normalised_data,n=2)[1]

library(randomForest)

speedup = `colnames<-`(cbind(speedup),'speedup')

e = c()
for (c in 1:200) {
  e = rbind(e,config[[c]])
}
train = cbind(speedup,e)


(modelSpdUp <- randomForest(speedup ~ ., data=train, ntree = 500, na.action = na.omit ))

latencies = `colnames<-`(cbind(latencies),'latencies')

trainLantencies = cbind(latencies,e)
(modelLat <- randomForest(latencies~., data=trainLantencies, ntree = 500, na.action = na.omit))

myNames = c("V1","V2","V3","V4","V5","V6","V7","V8","V9")


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

config[[1]]$hfile_block_cache_size


