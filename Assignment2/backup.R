#librarily
library(ggplot2)
library(dplyr)
library(tree)
library(e1071) #for bayes & SVM model
library(ipred) #bagging
library(gbm) #boosting
library(randomForest) #random forest

#install.packages("adabag")
library(adabag)
#install.packages("rpart")
library(rpart)

library(caret) #useful classfication library
library(lattice)
library(MASS)

library(pROC)

#workspace set up
rm(list = ls())
WAUS <- read.csv("HumidPredict2023D.csv")
L <- as.data.frame(c(1:49))
set.seed(31538312) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

#-------------------------------------------------------------
attach(WAUS)
View(WAUS)

#this is just a prototype for now

WAUS = WAUS[,c(2,3,4,5,8,9,10,11,12,13,14,15,18,19,20,21,22)]

WAUS = WAUS[,c(2,3,4,5,8,9,10,11,12,13,15,21,22)] #may remove 20

WAUS$MHT = ifelse(WAUS$MHT == 1, "Yes", "No")
WAUS = na.omit(WAUS)

#convert categorical data as factor
WAUS <- WAUS %>% mutate_if(is.character, as.factor)

WAUS$Location = factor(WAUS$Location)
WAUS$MHT = factor(WAUS$MHT)
WAUS$WindGustDir = factor(WAUS$WindGustDir)
WAUS$WindDir3pm = factor(WAUS$WindDir3pm)
WAUS$WindDir9am = factor(WAUS$WindDir9am)

#Setting up training and test data*****************************************************

set.seed(31538312) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

dim(WAUS.test)
dim(WAUS.train)


#Classification model and testing*****************************************
#Bagging----------------------------------------

train.bagging = bagging(MHT ~. -Location , data = WAUS.train, mfinal = 10)
bagging.predict = predict.bagging(train.bagging, WAUS.test)

#consultation for this matrix
table(actual = WAUS.test$MHT, predicted = bagging.predict)
confusionMatrix(bagging.predict, WAUS.test$MHT) 

str(WAUS.test$MHT)

View(WAUS.test)

bag.roc.predict = prediction(bagging.predict$prob[,2], WAUS.test$MHT)
bag.roc.performance = performance(bag.roc.predict, "tpr", "fpr")

plot(bag.roc.performance, add = TRUE, col = "orange")
bag.auc = performance(bag.roc.predict, "auc")
print(as.numeric(bag.auc@y.values)) #AUC = 0.5805132


#Boosting--------------------------------------------

train.boost = boosting(MHT ~.-Location , data = WAUS.train, mfinal = 10)
boosting.predict = predict.boosting(train.boost, WAUS.test )

boost.roc.predict = prediction(boosting.predict$prob[,2], WAUS.test$MHT)
boost.roc.performance = performance(boost.roc.predict, "tpr", "fpr")

plot(boost.roc.performance, add = TRUE, col = "grey")

table(actual = WAUS.test$MHT, predicted = boosting.predict)

#Random Forest -------------------------------------------
#complete this
train.forest = randomForest(MHT ~.-Location , data = WAUS.train)
forest.predict = predict(train,forest, WAUS.test)

forest.roc = predict(forest.predict, WAUS.test, type = "prob")
forest.roc.predict = prediction(forest.roc[,2], WAUS.test$MHT)
forest.roc.performance = performance(forest.roc.predict, "tpr", "fpr")

plot(forest.roc.performance, add = TRUE, col = "red")

table(actual = WAUS.test$MHT, predicted = forest.predict)
confusionMatrix(forest.predict, WAUS.test$MHT) 



#Extension SVM support vector machine

train.svm = svm(MHT ~ . - Location, data = WAUS.train)
svm.predict = predict(train.svm, WAUS.test)

table(actual = WAUS.test$MHT, predicted = svm.predict)
confusionMatrix(svm.predict, WAUS.test$MHT) 


#Attribute importance #this just a reference

#Attribute importance cat("\n#Decision Tree Attribute  Importance\n") 
print(summary(M.tree)) 
cat("\n#Baging Attribute  Importance\n") 
print(M.bag$importance) 
cat("\n#Boosting Attribute  Importance\n") 
print(M.Boost$importance) 
cat("\n#Random Forest Attribute  Importance\n") 
print(M.rf$importance)















