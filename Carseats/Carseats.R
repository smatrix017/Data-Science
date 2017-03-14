library(ISLR)
library(MASS)
#fix(Carseats)
names(Carseats)
lm.fit = lm(Sales~.+Income:Advertising + Price:Age , data =Carseats)
summary(lm.fit)
#Adjusted R-squared = 0.8719
lm.fit = lm(Sales~CompPrice + Income + Advertising + Price + ShelveLoc + Age + Income:Advertising + Price:Age , data =Carseats)
summary(lm.fit)
#Adjusted R-squared = 0.8719
#even though Adjusted R-squared is same for both models, latter one is better as complexity is less