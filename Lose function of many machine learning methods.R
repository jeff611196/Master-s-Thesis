123
install.packages('pheatmap')
library(pheatmap)
#建立資料集test測試矩陣
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")
# 用pheatmap函式畫熱圖
pheatmap(test)


(2%%121)%/%11



#Regression Tree
library(rpart)
library(rpart.plot)
m = rpart(formula = PM2.5 ~ ., data = train[,-1],method  = "anova")


z = predict(m, test)
z = as.matrix(z)
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}

rmse(resid)

mae = function(error){
  mean(abs(error))
}

mae(resid)

rpart.plot(m)
summary(z)


#Bayesian
#install.packages("BAS")
library(BAS)
BAS = bas.lm(PM2.5 ~ ., data = train[,-1], prior = "ZS-null", modelprior = uniform())

z = predict(BAS, test[1:1000,-1],estimator = "BPM", se.fit = TRUE)
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}

rmse(resid)

mae = function(error){
  mean(abs(error))
}

mae(resid)


#KNN
#install.packages("FNN")
library(FNN)
knn1 = knn.reg(train = train[,-c(1,which(names(train)=='PM2.5'))], test = test[,-c(1,which(names(train)=='PM2.5'))], y = train[,which(names(train)=='PM2.5')], k = 1)
knn10 = knn.reg(train = train[,-c(1,which(names(train)=='PM2.5'))], test = test[,-c(1,which(names(train)=='PM2.5'))], y = train[,which(names(train)=='PM2.5')], k = 10)
knn100 = knn.reg(train = train[,-c(1,which(names(train)=='PM2.5'))], test = test[,-c(1,which(names(train)=='PM2.5'))], y = train[,which(names(train)=='PM2.5')], k = 100)

z = knn10$pred
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}

rmse(resid)

mae = function(error){
  mean(abs(error))
}

mae(resid)


#NN
#sessionInfo()
#install.packages("neuralnet") # for neuralnet(), nn model
#install.packages("nnet")      # for class.ind()
#install.packages("caret")     # for train(), tune parameters
library(neuralnet)
library(nnet)
library(caret)

bpn <- neuralnet(formula = PM2.5 ~ ., 
                 data = train,
                 hidden = c(2),       # 一個隱藏層：2個node
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 1e5        # 最大的ieration數 = 500000(5*10^5)
                 
)
#plot(model)

z = predict(bpn, test[,-1])
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}

rmse(resid)

mae = function(error){
  mean(abs(error))
}

mae(resid)

#NN plural
model <- train(form=PM2.5 ~.,     # formula
               data=train[1:1000,],           # 資料
               method="neuralnet",   # 類神經網路(bpn)
               
               # 最重要的步驟：觀察不同排列組合(第一層1~4個nodes ; 第二層0~4個nodes)
               # 看何種排列組合(多少隱藏層、每層多少個node)，會有最小的RMSE
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)),               
               
               # 以下的參數設定，和上面的neuralnet內一樣
               learningrate = 0.3,  # learning rate
               threshold = 0.1,     # partial derivatives of the error function, a stopping criteria
               stepmax = 1e5         # 最大的ieration數 = 500000(5*10^5)
)
warning()

train = round(train[,-1],2)



z = predict(model, test[,-1])
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}
rmse(resid)

mae = function(error){
  mean(abs(error))
}
mae(resid)

#Random forest
install.packages("sfsmisc")
install.packages("rsample")
install.packages("randomForest")
install.packages("ranger")
install.packages("caret")
install.packages("h2o")
install.packages("dplyr")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
library(dplyr)
library(magrittr)


m1 <- randomForest(
  formula = PM2.5 ~ .,
  data    = train[,-1]
)


z = as.matrix(predict(m1, test[,-1]))
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}

rmse(resid)

mae = function(error){
  mean(abs(error))
}

mae(resid)


#SVR
install.packages("e1071")
library(e1071)
svr.model <- svm(
  formula = PM2.5 ~ ., data = train[,-1], 
  type = "eps-regression",
  kernel = "radial"
)


z = predict(svr.model, test[,-1])
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}
rmse(resid)

mae = function(error){
  mean(abs(error))
}
mae(resid)

#Kernel
svr.model <- svm(
  formula = PM2.5 ~ ., data = train[,-1], 
  type = "eps-regression",
  kernel = "linear"
)


z = predict(svr.model, test[,-1])
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}
rmse(resid)

mae = function(error){
  mean(abs(error))
}
mae(resid)




svr.model <- svm(
  formula = PM2.5 ~ ., data = train[,-1], 
  type = "eps-regression",
  kernel = "polynomial"
)


z = predict(svr.model, test[,-1])
resid = test$PM2.5 - z 
A_t <- test$PM2.5
A_t[which(A_t == 0)] <- mean(A_t)

mape <- mean(abs(resid/A_t),na.rm = T)

rmse = function(error){
  sqrt(mean(error^2))
}
rmse(resid)

mae = function(error){
  mean(abs(error))
}
mae(resid)