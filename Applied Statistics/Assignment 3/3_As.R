# For data manipulation
library(tidyverse)

# For Decision Tree algorithm
library(rpart)

# for plotting the decision Tree
install.packages("rpart.plot") 
library(rpart.plot)

# Install readxl R package for reading excel sheets
install.packages("readxl")                                       
library("readxl")

library(tree)
library(ipred) 
library(dplyr)    
library(e1071)
require(caret)
library(randomForest)

#(a)

Bank = read.csv("D:/BIRMINGHAM/STUDIES/SEMESTER1/AppliedStatistics/3Assignment/bank.csv", header=T, sep=";")
Bank[sapply(Bank, is.character)] = lapply(Bank[sapply(Bank,is.character)], as.factor)
set.seed(42)
train = sample(nrow(Bank), nrow(Bank)*0.7)
Bank.train = Bank[train,]
Bank.test = Bank[-train,]

#(b)

bank_model = tree(y~., data = Bank.train, method = 'class')
plot(bank_model)
text(bank_model, pretty=0)

bank_model_cv = cv.tree(bank_model, FUN = prune.misclass)
plot(bank_model_cv$size,bank_model_cv$dev,type="b") #choose 5

bank_prune = prune.misclass(bank_model, best=5)
plot(bank_prune)
text(bank_prune, pretty=0)

#(c)

#error for training set
predict_tree0 = predict(bank_prune, Bank.train, type = "class")
error0 = mean(Bank.train$y != predict_tree0) 
error0
#error for test set
predict_tree1 = predict(bank_prune, Bank.test, type = "class")
error1 = mean(Bank.test$y != predict_tree1) 
error1

#(d)
set.seed(42)
bag <- bagging(y~., Bank.train)
varImp(bag)

predict_tree2 = predict(bag, Bank.test, type = "class")
error2 = mean(Bank.test$y != predict_tree2) 
error2

#(e)
set.seed(42)
rf_model = randomForest(y~., data = Bank.train)
varImpPlot(rf_model)
varImp(rf_model)
summary(rf_model)

predict_tree3 = predict(rf_model, Bank.test, type = "class")
error3 = mean(Bank.test$y != predict_tree3) 
error3

rf_er =  NULL
for(m in 1:16){
  set.seed(42)
  rf_model = randomForest(y~., data = Bank.train, mtry = m)
  predict_tree4 = predict(rf_model, Bank.test, type="class")
  rf_er = c(rf_er, mean(Bank.test$y != predict_tree4))
}
plot(rf_er, xlab="m", type="b", ylab="Test Error")
order(rf_er)

set.seed(42)
rf_model = randomForest(y~., data = Bank.train, mtry = 3)
varImpPlot(rf_model)
predict_tree5 = predict(rf_model, Bank.test, type = "class")
error5 = mean(Bank.test$y != predict_tree5) 
error5

