library(dplyr)
library(ellipse)
library(caret)
library(ISLR)
library(tibble)
library(MASS)    # includes linear discriminant function analysis
library(caret)   # cross-validation
library(ggplot2) # graphics

#a
Cancer = read.csv("D:/BIRMINGHAM/STUDIES/SEMESTER1/AppliedStatistics/2Assignment/breast-cancer-wisconsin.csv", header=T)
Cancer[Cancer=='?']<-NA
Cancer = na.omit(Cancer)
Cancer$Bare_Nuclei=as.numeric(Cancer$Bare_Nuclei)
set.seed(42)
train = sample(nrow(Cancer), nrow(Cancer)*0.8)
Cancer.train = Cancer[train,]
Cancer.test = Cancer[-train,]
nrow(Cancer.test)
#b

fit_glm = glm(as.factor(Class)~Clump_Thickness+Uniformity_CellSize+Uniformity_CellShape+Adhesion+Single_Epithelial_CellSize+
                +Bare_Nuclei+Bland_Chromatin+Normal_Nucleoli+Mitoses, data=Cancer.train, family = binomial)
summary(fit_glm)

class_pred = predict(fit_glm, Cancer.test, type = 'response')

class_pred = ifelse(class_pred > 0.5, 4, 2)

table(class_pred, Cancer.test$Class)

error = mean(class_pred != Cancer.test$Class)
error

#fit a lda
library(MASS)
lda.fit=lda(as.factor(Class)~Clump_Thickness+Uniformity_CellSize+Uniformity_CellShape+Adhesion+Single_Epithelial_CellSize+
              +Bare_Nuclei+Bland_Chromatin+Normal_Nucleoli+Mitoses, data=Cancer.train)

lda.fit

lda.pred=predict(lda.fit, Cancer.test)


lda.class =lda.pred$class
error2 = mean(lda.class != Cancer.test$Class) 
error2


#b
Cancer <- Cancer %>%
  mutate(Class_new= if_else(Class == 2, 0, 1))
fit_glm = glm(Class_new~Clump_Thickness+Uniformity_CellSize+Uniformity_CellShape+Adhesion+Single_Epithelial_CellSize+
                +Bare_Nuclei+Bland_Chromatin+Normal_Nucleoli+Mitoses, data=Cancer, family = binomial)
ypred = predict(fit_glm, newdata=College.test)
prediction = predict(fit_glm, Cancer$train)
fit_glm

#simple_class = function(x, boundary, above = 1, below = 0) {
#  ifelse(x > boundary, above, below)
#}
#class_pred = simple_class(x = class_pred, 
#                          boundary = 3, above = "Malignant", below = "Begin")
