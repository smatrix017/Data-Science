library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
#simple linear regression
lm.fit = lm(medv~lstat,data=Boston)
lm.fit
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="prediction")
plot(Boston$lstat,Boston$medv,pch=20)
abline(lm.fit,lwd=3)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))



#multiple

lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)
#excluding age
lm.fit1 = update (lm.fit,~.-age)
summary(lm.fit1)
#excluding 
lm.fit1 = update (lm.fit1,~.-indus)
summary(lm.fit1)
#adusted R-squared is 0.7348 