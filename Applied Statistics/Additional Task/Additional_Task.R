library(dplyr)
library(MASS)
library(readr)
library(tibble)
library(tidyverse)
library(rpart)
library(caret)
install.packages("rpart.plot") 
library(rpart.plot)
install.packages("readxl")                                       
library("readxl")
library(caret)
library(randomForest)
library(gbm)
library(tidyverse)
install.packages('topicmodels')

winedata = read.csv("D:/BIRMINGHAM/STUDIES/SEMESTER1/AppliedStatistics/Additional_Task/winedata.csv", header=T)

#Divide the data in two groups
groups_of_wine <- split(winedata, winedata$wine)

#Data for red wine
red_group = groups_of_wine$Red
red_group <- red_group[-13]
red_group <- red_group[-12]
means_red = colMeans(red_group)
standart_deviation_red = apply(red_group, 2, sd)

#Data for red wine
white_group = groups_of_wine$White
white_group <- white_group[-13]
white_group <- white_group[-12]
means_white = colMeans(white_group)
standart_deviation_white = apply(white_group, 2, sd)

#check the p-value for the least deference in means
res = means_white-means_red
res

min(abs(means_white-means_red))


t.test(as.factor(wine)~density, data=winedata, var.equal=T)


df <- data.frame(means_white, means_red, standart_deviation_white, standart_deviation_red) %>% 
  rowwise%>%
  mutate(pval = t.test(white_group,red_group)$p.value) %>%
  ungroup()
df

#Divide data randomly
set.seed(102)
r = rnorm(1)
test = sample(nrow(winedata), nrow(winedata)*r)
winedata.train = winedata[-test,]
winedata.test = winedata[test,]

nrow(winedata.test) #the number of red and white wine samples in my test set
winedata.test_groups <- split(winedata.test, winedata.test$wine)
nrow(winedata.test_groups$Red)
nrow(winedata.test_groups$White)

nrow(winedata.train) #the number of red and white wine samples in my training set
winedata.train_groups <- split(winedata.train, winedata.train$wine)
nrow(winedata.train_groups$Red)
nrow(winedata.train_groups$White)

#Different classification methods


#Logistic regression
#Fit a logistic regression model to predict Direction
glm.fit = glm(as.factor(wine)~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
                density+pH+sulphates+alcohol, data=winedata.train, family = binomial)
summary(glm.fit)

#contribution of density, fixed.acidity and citric.acid are not significant

class_pred = predict(glm.fit, winedata.test, type = 'response')

class_pred = ifelse(class_pred > 0.5, "White", "Red")

table(class_pred, winedata.test$wine)

error1 = mean(class_pred != winedata.test$wine)
error1

#Build a logistic regression model to predict Direction without insignificant variables
glm.fit = glm(as.factor(wine)~volatile.acidity+residual.sugar+
                chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
                pH+sulphates+alcohol, data=winedata.train, family = binomial)
summary(glm.fit)
class_pred = predict(glm.fit, winedata.test, type = 'response')

class_pred = ifelse(class_pred > 0.5, "White", "Red")

table(class_pred, winedata.test$wine)

error2 = mean(class_pred != winedata.test$wine)
error2

#LDA
lda.fit=lda(as.factor(wine)~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
              chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
              density+pH+sulphates+alcohol, data=winedata.train)

lda.fit

lda.pred=predict(lda.fit, winedata.test)


lda.class =lda.pred$class
error3 = mean(lda.class != winedata.test$wine) 
error3

#Build a LDA model to predict Direction without insignificant variables
lda.fit=lda(as.factor(wine)~volatile.acidity+residual.sugar+
              chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
              pH+sulphates+alcohol, data=winedata.train)

lda.fit

lda.pred=predict(lda.fit, winedata.test)


lda.class =lda.pred$class
error4 = mean(lda.class != winedata.test$wine) 
error4

#Classification trees
#Build a model
model = rpart(wine~., data = winedata.train, method = 'class')
rpart.plot(model)

#Predict
predict_tree = predict(model, winedata.test, type = "class")

table(winedata.test$wine, predict_tree)

error5 = mean(winedata.test$wine != predict_tree) 
error5

#Gradient Boosting
model_gbm = gbm(wine~., data = winedata.train, distribution = "multinomial")

summary(model_gbm)

pred_test = predict(model_gbm, winedata.test)

error6 = mean(winedata.test$wine != predict_tree)
error6

#Boosting with tuning
#cross-validation for tuning parameters
#choose 5 blocks for cross-validation
train_control = trainControl(method = "cv", number = 5)

#The tunning grid for choosing number of trees, splits and shrinkage parameter
gbmGrid <- expand.grid(max_depth = c(1, 2, 3), 
                       nrounds = (1:10)*50,
                       eta = c(.1, .4),
                       gamma = 0,
                       colsample_bytree = 0.6,
                       min_child_weight = 1,
                       subsample = 1)

model_boost = train(wine~., data = winedata.train, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)

summary(model_boost)
print(model_boost)

pred_test = predict(model_boost, winedata.test)

error7 = mean(winedata.test$wine != pred_test)
error7

#Random forest
rf_model = randomForest(as.factor(wine)~., data = winedata.train)

summary(rf_model)
print(rf_model)

pred_test = predict(rf_model, winedata.test)
error8 = mean(winedata.test$wine != pred_test)
error8

