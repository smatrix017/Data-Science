library(ISLR)
names(Smarket)
library(MASS)
train=(Smarket$Year<2005)
Smarket.2005 = Smarket[!train,]
#lda
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket,data = Smarket, subset = train)
lda.fit
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)
#qda
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class = predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class == Direction.2005)
