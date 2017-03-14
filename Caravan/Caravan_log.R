library(ISLR)
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs = predict(glm.fit,Caravan[test,],type="response")
glm.pred = rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)