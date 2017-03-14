library(ISLR)
dim(Caravan)
summary(Purchase)
348/5822
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test=1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
#try for k=1
knn.pred = knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)
#use k=5
knn.pred = knn(train.X,test.X,train.Y,k=5)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
4/15
