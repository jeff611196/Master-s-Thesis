data = read.delim("E:/論文/final/all.csv", header = T,sep=",")
which(names(data)=='PM2.5')
for(i in 7:33)
{
  data[,i] = data[,i]/max(abs(data[,i]))
}

for(i in 35:ncol(data))
{
  data[,i] = data[,i]/max(abs(data[,i]))
}
data[,which(is.na(data[1,]) == "TRUE")] = 0
data = data[,-which(names(data)=='PM10')]

season = matrix(0,ncol = 4,nrow = nrow(data))
colnames(season) = c("spring","summer","fall","winter")
data = cbind(data,season)
data$spring[which(substring(data[,1],6,7) == "03")] = 1
data$spring[which(substring(data[,1],6,7) == "04")] = 1
data$spring[which(substring(data[,1],6,7) == "05")] = 1
data$summer[which(substring(data[,1],6,7) == "06")] = 1
data$summer[which(substring(data[,1],6,7) == "07")] = 1
data$summer[which(substring(data[,1],6,7) == "08")] = 1
data$fall[which(substring(data[,1],6,7) == "09")] = 1
data$fall[which(substring(data[,1],6,7) == "10")] = 1
data$fall[which(substring(data[,1],6,7) == "11")] = 1
data$winter[which(substring(data[,1],6,7) == "12")] = 1
data$winter[which(substring(data[,1],6,7) == "01")] = 1
data$winter[which(substring(data[,1],6,7) == "02")] = 1

data = data[,-1]

use = as.data.frame(cbind(data[,2],data[,3],data[,4],data[,6],data[,7],data[,9],data[,10],data[,11],data[,12],data[,13],data[,14],
                          data[,15],data[,16],data[,17],data[,18],data[,19],data[,20],data[,21],data[,22],data[,23],data[,24],data[,25],
                          data[,26],data[,27],data[,28],data[,30],data[,32],data[,34],data[,35],data[,36],data[,37],data[,39],data[,40],
                          data[,42],data[,43],data[,44],data[,45],data[,47]))

#write.table(use,file="C:\\Users\\jeff\\Desktop\\老闆的\\data.csv",sep=",",row.names = F, na = "NA")


colnames(use) = c(colnames(data[2]),colnames(data[3]),colnames(data[4]),colnames(data[6]),colnames(data[7]),colnames(data[9]),
                  colnames(data[10]),colnames(data[11]),colnames(data[12]),colnames(data[13]),colnames(data[14]),colnames(data[15]),
                  colnames(data[16]),colnames(data[17]),colnames(data[18]),colnames(data[19]),colnames(data[20]),colnames(data[21]),
                  colnames(data[22]),colnames(data[23]),colnames(data[24]),colnames(data[25]),colnames(data[26]),colnames(data[27]),
                  colnames(data[28]),colnames(data[30]),colnames(data[32]),colnames(data[34]),colnames(data[35]),colnames(data[36]),
                  colnames(data[37]),colnames(data[39]),colnames(data[40]),colnames(data[42]),colnames(data[43]),colnames(data[44]),
                  colnames(data[45]),colnames(data[47]))


data2 = read.delim("C:/Users/jeff/Desktop/老闆的/taipei2/loss_taipei.csv", header = F,sep=",")
x = as.data.frame(matrix(NA,ncol = 3,nrow = 168))
colnames(x) = colnames(data)[c(2,3,4)]

x[,1] = 0
x[,2] = 0
x[,3] = 0

data2 = cbind(x[,1:3],data2)
colnames(data2)[36:38] = c("spring","summer","winter")
data2[,36] = 0
data2[,37] = 0
data2[,38] = 1


random = matrix(1:nrow(use),ncol = 1)
use_random = cbind(random,use)

set.seed(1)
train = sample(use_random[,1], size = 0.7*nrow(use_random), replace = FALSE)
test = use_random[-train,]
train = use_random[train,]
colnames(data2) = colnames(train[,-1])

#library(xgboost)
set.seed(2)
xgb <- xgboost(data = as.matrix(train[,-c(1,which(names(train)=='PM2.5'))]), label = train$PM2.5,colsample_bytree = 1,  subsample = 0.9, 
               booster = "gbtree", max_depth = 9, eta = 0.03, eval_metric = "rmse", objective = "reg:linear", gamma = 0, nfold = 5, 
               nrounds = 5000, early_stopping_rounds = 30, print_every_n = 20) 

#test[which(test$PM2.5 == max(test$PM2.5)),-c(which(colnames(test) == "random"),which(colnames(test) == "PM2.5"))]

data2$PM2.5[order(data2$PM2.5,decreasing=T)[1:10]]
parameter = data2[order(data2$PM2.5,decreasing=T)[3],-which(colnames(data2) == "PM2.5")]
parameter_o = parameter
zz = as.matrix(predict(xgb, as.matrix(parameter)))

#traffic
select = matrix(NA,ncol = 1,nrow = 11^4)


for(e in 0:10)
{
  parameter$type42 = parameter_o$type42 - 0.1*e*parameter_o$type42
  for(c in 0:10)
  {
    parameter$type32 = parameter_o$type32 - 0.1*c*parameter_o$type32
    for(b in 0:10)
    {
      parameter$type31 = parameter_o$type31 - 0.1*b*parameter_o$type31
      for(a in 0:10)
      {
        parameter$type5 = parameter_o$type5 - 0.1*a*parameter_o$type5
        select[1+a+11*b+121*c+1331*e,1] = as.matrix(predict(xgb, as.matrix(parameter)))
      }
    }
  }
}


min10_PM2.5 = as.matrix(round(select[order(select,decreasing=F)[1:110]],digits = 2))
#min10_PM2.5 = t(min10_PM2.5)
min10_local = as.matrix(order(select,decreasing=F)[1:110])
#min10_PM2.5 = as.matrix(round(select[order(select,decreasing=F)[1:10]],digits = 2))
#min10_local = as.matrix(order(select,decreasing=F)[1:10])
a = as.matrix(min10_local%%11)
a[which(a == 1)] = 1
a[which(a == 2)] = 0.9
a[which(a == 3)] = 0.8
a[which(a == 4)] = 0.7
a[which(a == 5)] = 0.6
a[which(a == 6)] = 0.5
a[which(a == 7)] = 0.4
a[which(a == 8)] = 0.3
a[which(a == 9)] = 0.2
a[which(a == 10)] = 0.1
a[which(a == 0)] = 0

b = as.matrix((min10_local%%121)%/%11)
b[which(b == 1)] = 0.9
b[which(b == 0)] = 1
b[which(b == 2)] = 0.8
b[which(b == 3)] = 0.7
b[which(b == 4)] = 0.6
b[which(b == 5)] = 0.5
b[which(b == 6)] = 0.4
b[which(b == 7)] = 0.3
b[which(b == 8)] = 0.2
b[which(b == 9)] = 0.1
b[which(b == 10)] = 0

c = as.matrix((min10_local%%1331)%/%121)
c[which(c == 1)] = 0.9
c[which(c == 0)] = 1
c[which(c == 2)] = 0.8
c[which(c == 3)] = 0.7
c[which(c == 4)] = 0.6
c[which(c == 5)] = 0.5
c[which(c == 6)] = 0.4
c[which(c == 7)] = 0.3
c[which(c == 8)] = 0.2
c[which(c == 9)] = 0.1
c[which(c == 10)] = 0

e = as.matrix(min10_local%/%(11^3))
e[which(e == 1)] = 0.9
e[which(e == 0)] = 1
e[which(e == 2)] = 0.8
e[which(e == 3)] = 0.7
e[which(e == 4)] = 0.6
e[which(e == 5)] = 0.5
e[which(e == 6)] = 0.4
e[which(e == 7)] = 0.3
e[which(e == 8)] = 0.2
e[which(e == 9)] = 0.1
e[which(e == 10)] = 0

color = matrix(NA,ncol = 4,nrow = 11)
color[1,1] = length(which(a == 1))
color[2,1] = length(which(a == 0.9))
color[3,1] = length(which(a == 0.8))
color[4,1] = length(which(a == 0.7))
color[5,1] = length(which(a == 0.6))
color[6,1] = length(which(a == 0.5))
color[7,1] = length(which(a == 0.4))
color[8,1] = length(which(a == 0.3))
color[9,1] = length(which(a == 0.2))
color[10,1] = length(which(a == 0.1))
color[11,1] = length(which(a == 0))

color[1,2] = length(which(b == 1))
color[2,2] = length(which(b == 0.9))
color[3,2] = length(which(b == 0.8))
color[4,2] = length(which(b == 0.7))
color[5,2] = length(which(b == 0.6))
color[6,2] = length(which(b == 0.5))
color[7,2] = length(which(b == 0.4))
color[8,2] = length(which(b == 0.3))
color[9,2] = length(which(b == 0.2))
color[10,2] = length(which(b == 0.1))
color[11,2] = length(which(b == 0))

color[1,3] = length(which(c == 1))
color[2,3] = length(which(c == 0.9))
color[3,3] = length(which(c == 0.8))
color[4,3] = length(which(c == 0.7))
color[5,3] = length(which(c == 0.6))
color[6,3] = length(which(c == 0.5))
color[7,3] = length(which(c == 0.4))
color[8,3] = length(which(c == 0.3))
color[9,3] = length(which(c == 0.2))
color[10,3] = length(which(c == 0.1))
color[11,3] = length(which(c == 0))

color[1,4] = length(which(e == 1))
color[2,4] = length(which(e == 0.9))
color[3,4] = length(which(e == 0.8))
color[4,4] = length(which(e == 0.7))
color[5,4] = length(which(e == 0.6))
color[6,4] = length(which(e == 0.5))
color[7,4] = length(which(e == 0.4))
color[8,4] = length(which(e == 0.3))
color[9,4] = length(which(e == 0.2))
color[10,4] = length(which(e == 0.1))
color[11,4] = length(which(e == 0))

colnames(color) = paste(c("type5","type31","type32","type42"))
rownames(color) = paste(seq(100, 0, by = -10),"%")
# 用pheatmap函式畫熱圖
library(pheatmap)
pheatmap(color,color = colorRampPalette(colors = c("white","gray","black"))(100),cluster_row = FALSE,cluster_col = FALSE,
         display_numbers = TRUE,number_format = "%.0f")




#power
select2 = matrix(NA,ncol = 1,nrow = 11^5)


for(c in 0:10)
{
   parameter$Nuclear = parameter_o$Nuclear - 0.1*c*parameter_o$Nuclear
  for(b in 0:10)
  {
    parameter$Gas = parameter_o$Gas - 0.1*b*parameter_o$Gas
    for(a in 0:10)
    {
      parameter$Coal = parameter_o$Coal - 0.1*a*parameter_o$Coal
      select2[1+a+11*b+11^2*c,1] = as.matrix(predict(xgb, as.matrix(parameter)))
    }
  }
}

parameter = parameter_o

min10_PM2.5 = as.matrix(round(select2[order(select2,decreasing=F)[1:110]],digits = 2))
#min10_PM2.5 = t(min10_PM2.5)
min10_local = as.matrix(order(select2,decreasing=F)[1:110])
#min10_PM2.5 = as.matrix(round(select[order(select,decreasing=F)[1:10]],digits = 2))
#min10_local = as.matrix(order(select,decreasing=F)[1:10])
a = as.matrix(min10_local%%11)
a[which(a == 1)] = 1
a[which(a == 2)] = 0.9
a[which(a == 3)] = 0.8
a[which(a == 4)] = 0.7
a[which(a == 5)] = 0.6
a[which(a == 6)] = 0.5
a[which(a == 7)] = 0.4
a[which(a == 8)] = 0.3
a[which(a == 9)] = 0.2
a[which(a == 10)] = 0.1
a[which(a == 0)] = 0

b = as.matrix((min10_local%%121)%/%11)
b[which(b == 1)] = 0.9
b[which(b == 0)] = 1
b[which(b == 2)] = 0.8
b[which(b == 3)] = 0.7
b[which(b == 4)] = 0.6
b[which(b == 5)] = 0.5
b[which(b == 6)] = 0.4
b[which(b == 7)] = 0.3
b[which(b == 8)] = 0.2
b[which(b == 9)] = 0.1
b[which(b == 10)] = 0

c = as.matrix((min10_local%%1331)%/%121)
c[which(c == 1)] = 0.9
c[which(c == 0)] = 1
c[which(c == 2)] = 0.8
c[which(c == 3)] = 0.7
c[which(c == 4)] = 0.6
c[which(c == 5)] = 0.5
c[which(c == 6)] = 0.4
c[which(c == 7)] = 0.3
c[which(c == 8)] = 0.2
c[which(c == 9)] = 0.1
c[which(c == 10)] = 0



color = matrix(NA,ncol = 3,nrow = 11)
color[1,1] = length(which(a == 1))
color[2,1] = length(which(a == 0.9))
color[3,1] = length(which(a == 0.8))
color[4,1] = length(which(a == 0.7))
color[5,1] = length(which(a == 0.6))
color[6,1] = length(which(a == 0.5))
color[7,1] = length(which(a == 0.4))
color[8,1] = length(which(a == 0.3))
color[9,1] = length(which(a == 0.2))
color[10,1] = length(which(a == 0.1))
color[11,1] = length(which(a == 0))

color[1,2] = length(which(b == 1))
color[2,2] = length(which(b == 0.9))
color[3,2] = length(which(b == 0.8))
color[4,2] = length(which(b == 0.7))
color[5,2] = length(which(b == 0.6))
color[6,2] = length(which(b == 0.5))
color[7,2] = length(which(b == 0.4))
color[8,2] = length(which(b == 0.3))
color[9,2] = length(which(b == 0.2))
color[10,2] = length(which(b == 0.1))
color[11,2] = length(which(b == 0))

color[1,3] = length(which(c == 1))
color[2,3] = length(which(c == 0.9))
color[3,3] = length(which(c == 0.8))
color[4,3] = length(which(c == 0.7))
color[5,3] = length(which(c == 0.6))
color[6,3] = length(which(c == 0.5))
color[7,3] = length(which(c == 0.4))
color[8,3] = length(which(c == 0.3))
color[9,3] = length(which(c == 0.2))
color[10,3] = length(which(c == 0.1))
color[11,3] = length(which(c == 0))



colnames(color) = paste(c("Coal","Gas","Nuclear"))
rownames(color) = paste(seq(100, 0, by = -10),"%")
# 用pheatmap函式畫熱圖
pheatmap(color,color = colorRampPalette(colors = c("white","gray","black"))(100),cluster_row = FALSE,cluster_col = FALSE,
         display_numbers = TRUE,number_format = "%.0f")




#resid
parameter_resid = data2[,-which(colnames(data2) == "PM2.5")]
z = as.matrix(predict(xgb, as.matrix(parameter_resid)))


resid = data2$PM2.5 - z 
A_t <- data2$PM2.5
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

mape









#Power generation
data = data[which(data$North == 1),]
x = matrix(NA,ncol = 1,nrow = 12)

for(i in 0:11)
{
  x[i+1,1] = sum(data[,i+12])
}

x = x[-9,]
x = x/sum(x)
col = colnames(data)


which(data[,26] < 0)
colnames(data[26])
data[which(data[,26] < 0),26]






#power only two parameters
data = read.delim("E:/論文/final/all.csv", header = T,sep=",")
which(names(data)=='PM2.5')
for(i in 7:33)
{
  data[,i] = data[,i]/max(abs(data[,i]))
}

for(i in 35:ncol(data))
{
  data[,i] = data[,i]/max(abs(data[,i]))
}
data[,which(is.na(data[1,]) == "TRUE")] = 0
data = data[,-which(names(data)=='PM10')]

season = matrix(0,ncol = 4,nrow = nrow(data))
colnames(season) = c("spring","summer","fall","winter")
data = cbind(data,season)
data$spring[which(substring(data[,1],6,7) == "03")] = 1
data$spring[which(substring(data[,1],6,7) == "04")] = 1
data$spring[which(substring(data[,1],6,7) == "05")] = 1
data$summer[which(substring(data[,1],6,7) == "06")] = 1
data$summer[which(substring(data[,1],6,7) == "07")] = 1
data$summer[which(substring(data[,1],6,7) == "08")] = 1
data$fall[which(substring(data[,1],6,7) == "09")] = 1
data$fall[which(substring(data[,1],6,7) == "10")] = 1
data$fall[which(substring(data[,1],6,7) == "11")] = 1
data$winter[which(substring(data[,1],6,7) == "12")] = 1
data$winter[which(substring(data[,1],6,7) == "01")] = 1
data$winter[which(substring(data[,1],6,7) == "02")] = 1

data = data[,-1]

use = as.data.frame(cbind(data[,2],data[,3],data[,4],data[,6],data[,7],data[,9],data[,10],data[,11],data[,12],data[,13],data[,14],
                          data[,15],data[,16],data[,17],data[,18],data[,19],data[,20],data[,21],data[,22],data[,23],data[,24],data[,25],
                          data[,26],data[,27],data[,28],data[,30],data[,32],data[,34],data[,35],data[,36],data[,37],data[,39],data[,40],
                          data[,42],data[,43],data[,44],data[,45],data[,47]))

#write.table(use,file="C:\\Users\\jeff\\Desktop\\老闆的\\data.csv",sep=",",row.names = F, na = "NA")


colnames(use) = c(colnames(data[2]),colnames(data[3]),colnames(data[4]),colnames(data[6]),colnames(data[7]),colnames(data[9]),
                  colnames(data[10]),colnames(data[11]),colnames(data[12]),colnames(data[13]),colnames(data[14]),colnames(data[15]),
                  colnames(data[16]),colnames(data[17]),colnames(data[18]),colnames(data[19]),colnames(data[20]),colnames(data[21]),
                  colnames(data[22]),colnames(data[23]),colnames(data[24]),colnames(data[25]),colnames(data[26]),colnames(data[27]),
                  colnames(data[28]),colnames(data[30]),colnames(data[32]),colnames(data[34]),colnames(data[35]),colnames(data[36]),
                  colnames(data[37]),colnames(data[39]),colnames(data[40]),colnames(data[42]),colnames(data[43]),colnames(data[44]),
                  colnames(data[45]),colnames(data[47]))


data2 = read.delim("C:/Users/jeff/Desktop/老闆的/chiayi2/loss_chiayi.csv", header = F,sep=",")
x = as.data.frame(matrix(NA,ncol = 3,nrow = 168))
colnames(x) = colnames(data)[c(2,3,4)]

x[,1] = 0
x[,2] = 0
x[,3] = 1

data2 = cbind(x[,1:3],data2)
colnames(data2)[36:38] = c("spring","summer","winter")
data2[,36] = 0
data2[,37] = 0
data2[,38] = 1


random = matrix(1:nrow(use),ncol = 1)
use_random = cbind(random,use)

set.seed(1)
train = sample(use_random[,1], size = 0.7*nrow(use_random), replace = FALSE)
test = use_random[-train,]
train = use_random[train,]
colnames(data2) = colnames(train[,-1])

#library(xgboost)
set.seed(2)
xgb <- xgboost(data = as.matrix(train[,-c(1,which(names(train)=='PM2.5'))]), label = train$PM2.5,colsample_bytree = 1,  subsample = 0.9, 
               booster = "gbtree", max_depth = 9, eta = 0.03, eval_metric = "rmse", objective = "reg:linear", gamma = 0, nfold = 5, 
               nrounds = 5000, early_stopping_rounds = 30, print_every_n = 20) 

#test[which(test$PM2.5 == max(test$PM2.5)),-c(which(colnames(test) == "random"),which(colnames(test) == "PM2.5"))]

data2$PM2.5[order(data2$PM2.5,decreasing=T)[1:10]]
parameter = data2[order(data2$PM2.5,decreasing=T)[2],-which(colnames(data2) == "PM2.5")]
parameter_o = parameter
zz = as.matrix(predict(xgb, as.matrix(parameter)))

#power
select2 = matrix(NA,ncol = 1,nrow = 21^2)



for(b in 0:20)
{
  parameter$IPPgas = parameter_o$IPPgas - 0.05*b*parameter_o$IPPgas
  for(a in 0:20)
  {
    parameter$IPPCoal = parameter_o$IPPCoal - 0.05*a*parameter_o$IPPCoal
    select2[1+a+21*b,1] = as.matrix(predict(xgb, as.matrix(parameter)))
  }
}


#parameter = parameter_o

min10_PM2.5 = as.matrix(round(select2[order(select2,decreasing=F)[1:20]],digits = 2))
#min10_PM2.5 = t(min10_PM2.5)
min10_local = as.matrix(order(select2,decreasing=F)[1:20])
#min10_PM2.5 = as.matrix(round(select[order(select,decreasing=F)[1:10]],digits = 2))
#min10_local = as.matrix(order(select,decreasing=F)[1:10])
a = as.matrix(min10_local%%21)
a[which(a == 1)] = 1
a[which(a == 2)] = 0.95
a[which(a == 3)] = 0.9
a[which(a == 4)] = 0.85
a[which(a == 5)] = 0.8
a[which(a == 6)] = 0.75
a[which(a == 7)] = 0.7
a[which(a == 8)] = 0.65
a[which(a == 9)] = 0.6
a[which(a == 10)] = 0.55
a[which(a == 11)] = 0.5
a[which(a == 12)] = 0.45
a[which(a == 13)] = 0.4
a[which(a == 14)] = 0.35
a[which(a == 15)] = 0.3
a[which(a == 16)] = 0.25
a[which(a == 17)] = 0.2
a[which(a == 18)] = 0.15
a[which(a == 19)] = 0.1
a[which(a == 20)] = 0.05
a[which(a == 0)] = 0

b = as.matrix((min10_local%%121)%/%11)
b[which(b == 1)] = 0.95
b[which(b == 0)] = 1
b[which(b == 2)] = 0.9
b[which(b == 3)] = 0.85
b[which(b == 4)] = 0.8
b[which(b == 5)] = 0.75
b[which(b == 6)] = 0.7
b[which(b == 7)] = 0.65
b[which(b == 8)] = 0.6
b[which(b == 9)] = 0.55
b[which(b == 10)] = 0.5
b[which(b == 11)] = 0.45
b[which(b == 12)] = 0.4
b[which(b == 13)] = 0.35
b[which(b == 14)] = 0.3
b[which(b == 15)] = 0.25
b[which(b == 16)] = 0.2
b[which(b == 17)] = 0.15
b[which(b == 18)] = 0.1
b[which(b == 19)] = 0.05
b[which(b == 20)] = 0


color = matrix(NA,ncol = 2,nrow = 21)
color[1,1] = length(which(a == 1))
color[2,1] = length(which(a == 0.95))
color[3,1] = length(which(a == 0.9))
color[4,1] = length(which(a == 0.85))
color[5,1] = length(which(a == 0.8))
color[6,1] = length(which(a == 0.75))
color[7,1] = length(which(a == 0.7))
color[8,1] = length(which(a == 0.65))
color[9,1] = length(which(a == 0.6))
color[10,1] = length(which(a == 0.55))
color[11,1] = length(which(a == 0.5))
color[12,1] = length(which(a == 0.45))
color[13,1] = length(which(a == 0.4))
color[14,1] = length(which(a == 0.35))
color[15,1] = length(which(a == 0.3))
color[16,1] = length(which(a == 0.25))
color[17,1] = length(which(a == 0.2))
color[18,1] = length(which(a == 0.15))
color[19,1] = length(which(a == 0.1))
color[20,1] = length(which(a == 0.05))
color[21,1] = length(which(a == 0))

color[1,2] = length(which(b == 1))
color[2,2] = length(which(b == 0.95))
color[3,2] = length(which(b == 0.9))
color[4,2] = length(which(b == 0.85))
color[5,2] = length(which(b == 0.8))
color[6,2] = length(which(b == 0.75))
color[7,2] = length(which(b == 0.7))
color[8,2] = length(which(b == 0.65))
color[9,2] = length(which(b == 0.6))
color[10,2] = length(which(b == 0.55))
color[11,2] = length(which(b == 0.5))
color[12,2] = length(which(b == 0.45))
color[13,2] = length(which(b == 0.4))
color[14,2] = length(which(b == 0.35))
color[15,2] = length(which(b == 0.3))
color[16,2] = length(which(b == 0.25))
color[17,2] = length(which(b == 0.2))
color[18,2] = length(which(b == 0.15))
color[19,2] = length(which(b == 0.1))
color[20,2] = length(which(b == 0.05))
color[21,2] = length(which(b == 0))



colnames(color) = paste(c("IPPCoal","IPPGas"))
rownames(color) = paste(seq(100, 0, by = -5),"%")
# 用pheatmap函式畫熱圖
pheatmap(color,color = colorRampPalette(colors = c("white","gray","black"))(100),cluster_row = FALSE,cluster_col = FALSE,
         display_numbers = TRUE,number_format = "%.0f")