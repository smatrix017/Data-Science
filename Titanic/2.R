prop.table(table(train$Sex, train$Survived),1) #row wise
prop.table(table(train$Sex, train$Survived),2) # column wise
test$survived = 0
test$survived[test$Sex == 'female'] = 1 
# consider age now
train$child =0
train$child[train$Age < 18] = 1
aggregate(Survived ~ child + Sex, data=train, FUN=sum)
aggregate(Survived ~ child + Sex, data=train, FUN=length(x))
aggregate(Survived ~ child + Sex, data=train, FUN=function(x){sum(x)/length(x)})
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + child + Sex , data=train, FUN=function(x){sum(x)/length(x)})
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit = data.frame(PassengerID = test$PassengerId,Survived = test$survived)
write.csv(submit, file = "2.csv",row.names = FALSE)
