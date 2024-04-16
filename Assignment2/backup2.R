#BACK UP 2

#librarily
library(ggplot2)
library(dplyr)
library(tree)
#library(e1071) #for bayes & SVM model
#library(ipred) #bagging
#library(gbm) #boosting
#library(randomForest) #random forest


#install.packages("tree") 
library(tree) 
#install.packages("e1071") 
library(e1071) 
#install.packages(("ROCR")) 
library(ROCR) 
#install.packages("randomForest") 
library(randomForest) 
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

length(WAUS)

colnames(WAUS)

View(L)


#Questions
#1--------------------------------------------------------------
#Explore the data

#proportion of days when its more humid than the previous day compred to those where it is less humid
MHT_data = ifelse(WAUS$MHT == 0, "No", "Yes")
MHT_count = table(MHT_data)
barplot(MHT_count) #make this nicer later

#percentage of missing value
as.data.frame(colMeans(is.na(WAUS))* 100) #percentage missing value of the data

#only numeric column
cor_data = WAUS[,c(3,4,5,9,12,13,14,15,18,19,21)]
na.omit(cor_data)
cor_matrix = cor(cor_data, use = "pairwise.complete.obs")
as.table(cor_matrix)

write.csv(cor_matrix, file = "cor_matrix.csv")
matrix = read.csv("cor_matrix.csv")
View(matrix)

#I want to remove Evaporation, Sunshine, cloud9am-3pm because they have 50% of N/A---------
#correlation between sunshine and maxtemp it should be positively correlated
ggplot(data = WAUS, aes(x = Sunshine, y = MaxTemp)) + 
  geom_point()
cor(Sunshine, MaxTemp, use = "pairwise.complete.obs")

#correlation between evaporation and maxtemp should be positively correlated 
ggplot(data = WAUS, aes(x = Evaporation, y = MaxTemp)) + 
  geom_point()
cor(Evaporation, MaxTemp, use = "pairwise.complete.obs")


#cloud and rain

ggplot(data = WAUS, aes(x = Cloud9am, y = Rainfall)) + 
  geom_point()
cor(Cloud9am, Rainfall, use = "pairwise.complete.obs")

ggplot(data = WAUS, aes(x = Cloud3pm, y = Rainfall)) + 
  geom_point()
cor(Cloud3pm, Rainfall, use = "pairwise.complete.obs")


#Ask questions about rainfall vs raintoday or no
#WindGust Dir Gust Speed and wind at different time



#Pre processing----------------------------------------------------

#this is just a prototype for now

WAUS = WAUS[,c(2,3,4,5,8,9,10,11,12,13,14,15,18,19,20,21,22)] #try with all attribute, think about preserve 10 location

WAUS = WAUS[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)] # all atrribute

WAUS = WAUS[,c(2,3,4,5,8,9,10,11,12,13,15,21,22)] #may remove 20 #stick with this

WAUS.Ann = WAUS #for ANN

colnames(WAUS) 

WAUS$MHT = ifelse(WAUS$MHT == 1, "Yes", "No")
WAUS = na.omit(WAUS)


#convert categorical data as factor
WAUS <- WAUS %>% mutate_if(is.character, as.factor)

WAUS$Location = factor(WAUS$Location)
WAUS$MHT = factor(WAUS$MHT)
WAUS$WindGustDir = factor(WAUS$WindGustDir)
WAUS$WindDir3pm = factor(WAUS$WindDir3pm)
WAUS$WindDir9am = factor(WAUS$WindDir9am)


#Mean of the variable----------------------------------------------


location = by(WAUS, WAUS$Location, function(df){
  aggregate(cbind(3:21) ~ Location, data = df, mean, na.rm = TRUE)
})

WAUS

aggregate(WAUS[ , 3:21], by = list(WAUS$Location), FUN = mean, na.rm = TRUE) #mean
aggregate(WAUS[ , 3:21], by = list(WAUS$Location), FUN = sd, na.rm = TRUE) #std

location_df <- as.data.frame(do.call(rbind, location))
location_df

library(dplyr)
res <- WAUS %>% 
  group_by(WAUS$Location) %>% 
  summarise_each(funs(mean, sd))
`colnames<-`(t(res[-1]), as.character(res$Location))


#Setting up training and test data*****************************************************

set.seed(31538312) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

dim(WAUS.test)
dim(WAUS.train)


#
matrix_accuracy = function(table){
  result <- sum(diag(table)) / sum(table)
  print(result)
}

#Classification model and testing*****************************************

#Decision Tree---------------------------------

#train tree
dev.off()
train.tree = tree(MHT ~.-Location , data = WAUS.train, method = "class")
summary(train.tree)
plot(train.tree)
text(train.tree, pretty = 0)
title("Decisition Tree for predicting MHT")

#predicted tree
WAUS.predict = predict(train.tree, WAUS.test, type = "class")
tree.table = table(predicted = WAUS.predict, actual = WAUS.test$MHT)
tree.table

matrix_accuracy(tree.table) # accuracy 0.5676


#tree ROC curve
tree.roc = predict(train.tree, WAUS.test, type = "vector")

tree.roc.predict = prediction(tree.roc[,2], WAUS.test$MHT)
tree.roc.performance = performance(tree.roc.predict, "tpr", "fpr")
plot(tree.roc.performance, col = "brown", main = "ROC curve")
abline(0,1)

#lift chart MAY BE USEFUL
tree.lift = performance(tree.roc.predict, "lift")
plot(tree.lift)

#AUC
tree.auc = performance(tree.roc.predict, "auc")
print(as.numeric(tree.auc@y.values)) # AUC = 0.5599078


#Cross Validation tree--------------------------------------------------
cv1 = cv.tree(train.tree, FUN = prune.misclass)
cv1 #size 4 is a sweet spot

#prune tree using size 4
prune.train.tree = prune.misclass(train.tree, best = 4)

dev.off()
plot(prune.train.tree)
text(prune.train.tree, pretty = 0)
title("Pruned Decisition Tree for predicting MHT")

summary(prune.train.tree) #no improvement

#apply prune tree to predict data and ROC curve and AUC
PD.predict = predict(prune.train.tree, WAUS.test, type = "class")
table(actual = WAUS.test$MHT, predicted = PD.predict) #no difference found

PD.roc.predict = predict(prune.train.tree, WAUS.test, type = "vector")
pred = prediction(PD.roc.predict[,2], WAUS.test$MHT)
perf = performance(pred, "tpr", "fpr")
plot(perf, add = TRUE, col = 'yellow')

pruned.auc = performance(pred, "auc")
print(as.numeric(pruned.auc@y.values)) #AUC = 0.548986

#NayesBayes---------------------------------

train.bayes = naiveBayes(MHT ~.-Location , data = WAUS.train)

bayes.predict = predict(train.bayes, WAUS.test)

bayes.table = table(predicted = bayes.predict, actual = WAUS.test$MHT)
bayes.table

matrix_accuracy(bayes.table) #0.5495



#ROC curve
bayes.roc = predict(train.bayes, WAUS.test, type = "raw")
bayes.roc.predict = prediction(bayes.roc[,2], WAUS.test$MHT)
bayes.roc.performance = performance(bayes.roc.predict, "tpr", "fpr")
plot(bayes.roc.performance, add = TRUE, col = "blue")

bayes.auc = performance(bayes.roc.predict, "auc")
print(as.numeric(bayes.auc@y.values)) #AUC = 0.5743925



#Bagging----------------------------------------

train.bagging = bagging(MHT ~. -Location , data = WAUS.train, mfinal = 10)
bagging.predict = predict.bagging(train.bagging, WAUS.test)

print(bagging.predict$confusion) #confusion matrix

matrix_accuracy(bagging.predict$confusion) # 0.5630631

str(WAUS.test$MHT)

View(WAUS.test)

bag.roc.predict = prediction(bagging.predict$prob[,2], WAUS.test$MHT)
bag.roc.performance = performance(bag.roc.predict, "tpr", "fpr")

plot(bag.roc.performance, add = TRUE, col = "orange")
bag.auc = performance(bag.roc.predict, "auc")
print(as.numeric(bag.auc@y.values)) #AUC = 0.5761485


#Boosting--------------------------------------------

train.boost = boosting(MHT ~.-Location , data = WAUS.train, mfinal = 10)
boosting.predict = predict.boosting(train.boost, WAUS.test )

boost.roc.predict = prediction(boosting.predict$prob[,2], WAUS.test$MHT)
boost.roc.performance = performance(boost.roc.predict, "tpr", "fpr")

print(boosting.predict$confusion) #confusion matrix
matrix_accuracy(boosting.predict$confusion) #5247748

plot(boost.roc.performance, add = TRUE, col = "grey")
boost.auc = performance(boost.roc.predict, "auc")
print(as.numeric(boost.auc@y.values))



#Random Forest -------------------------------------------
train.forest = randomForest(MHT ~.-Location , data = WAUS.train, na.action = na.exclude)
forest.predict = predict(train.forest, WAUS.test)

#works
forest.table = table(predicted = forest.predict, actual = WAUS.test$MHT)
forest.table
matrix_accuracy(forest.table) #0.5743243

forest.roc = predict(train.forest, WAUS.test, type = "prob")
forest.roc.predict = prediction(forest.roc[,2], WAUS.test$MHT)
forest.roc.performance = performance(forest.roc.predict, "tpr", "fpr")

plot(forest.roc.performance, add = TRUE, col = "red")

forest.auc = performance(forest.roc.predict, "auc")
print(as.numeric(forest.auc@y.values)) #0.5911915



#Identifying important attributes of for each of the classification
cat("\n#Tree Attribute  Importance\n") 
print(summary(train.tree)) 
cat("\n#Baging Attribute  Importance\n") 
print(train.bagging$importance) 
cat("\n#Boosting Attribute  Importance\n") 
print(train.boost$importance) 
cat("\n#Random Forest Attribute  Importance\n") 
print(train.forest$importance)


#Accuracy of model
accuracy_matrix <- matrix(c(
  matrix_accuracy(tree.table),
  matrix_accuracy(bayes.table),
  matrix_accuracy(bagging.predict$confusion),
  matrix_accuracy(boosting.predict$confusion),
  matrix_accuracy(forest.table) 
), ncol = 1)


rownames(accuracy_matrix) <- c("Tree", "Bayesian", "Bagging", "Boosting", "Forest")
colnames(accuracy_matrix) <- "Accuracy"

# Print the table
print(accuracy_matrix)



#ROC plot of 5 classification model

dev.off()

roc.curve <- function(){
  # Plot the first ROC curve
  plot(tree.roc.performance, col = "blue", lwd = 2, main = "ROC curve")
  
  # Add the other ROC curves to the plot
  plot(bayes.roc.performance, add = TRUE, lwd = 2, col = "green")
  plot(bag.roc.performance, add = TRUE, lwd = 2, col = "orange")
  plot(boost.roc.performance, add = TRUE, lwd = 2, col = "grey")
  plot(forest.roc.performance, add = TRUE, lwd = 2 ,col = "red")
  
  # Add a legend to the plot
  legend("bottomright", legend = c("Tree", "Bayesian", "Bagging", "Boosting", "Random Forest"), col = c("blue", "green", "orange", "grey", "red"), lty = 1)
  
}

roc.curve()


#AUC result

tree.auc = performance(tree.roc.predict, "auc")
bayes.auc = performance(bayes.roc.predict, "auc")
bag.auc = performance(bag.roc.predict, "auc")
boost.auc = performance(boost.roc.predict, "auc")
forest.auc = performance(forest.roc.predict, "auc")

auc_table <- data.frame(
  Classifier = c("Tree", "Bayesian", "Bagging", "Boosting", "Random Forest"),
  AUC = c(as.numeric(tree.auc@y.values), as.numeric(bayes.auc@y.values), 
          as.numeric(bag.auc@y.values), as.numeric(boost.auc@y.values), 
          as.numeric(forest.auc@y.values))
)

auc_table



#ANN----------------------------------------

#prepare data for ANN

#install.packages("neuralnet") 
library(neuralnet)



colnames(WAUS.Ann)
str(WAUS.Ann)

WAUS.mm = model.matrix(~WindGustDir+WindDir9am+WindDir3pm, data = WAUS.Ann)
WAUS.Ann = cbind(WAUS.Ann, WAUS.mm)
WAUS.Ann$MHT = as.numeric(WAUS.Ann$MHT)

View(WAUS.Ann)

colnames(WAUS.Ann)

set.seed(31538312) #Student ID as random seed
train.Ann.row = sample(1:nrow(WAUS.Ann), 0.7*nrow(WAUS.Ann))
WAUS.Ann.train = WAUS.Ann[train.Ann.row,]
WAUS.Ann.test = WAUS.Ann[-train.Ann.row,]

str(WAUS.Ann.train)

WAUS.nn = neuralnet(MHT == 1  ~ MinTemp + MaxTemp + Rainfall + WindGustSpeed + WindSpeed9am + Pressure3pm + RISK_MM +
                      WindGustDirN + WindGustDirNE + WindGustDirNNE + WindGustDirNNW + WindGustDirNW + WindGustDirS + WindGustDirSE +
                      WindGustDirSSE + WindGustDirSSW + WindGustDirSW + WindGustDirW + WindGustDirWNW + WindGustDirWSW + WindDir9amENE +
                      WindDir9amESE + WindDir9amN + WindDir9amNE + WindDir9amNNE + WindDir9amNNW + WindDir9amNW + WindDir9amS +
                      WindDir9amSE + WindDir9amSSE + WindDir9amSSW + WindDir9amSW + WindDir9amW + WindDir9amWNW + WindDir9amWSW +
                      WindDir3pmENE + WindDir3pmESE + WindDir3pmN + WindDir3pmNE + WindDir3pmNNE + WindDir3pmNNW + WindDir3pmNW +
                      WindDir3pmS + WindDir3pmSE + WindDir3pmSSE + WindDir3pmSSW + WindDir3pmSW + WindDir3pmW + WindDir3pmWNW +
                      WindDir3pmWSW, WAUS.Ann.train, hidden = 3)


WAUS.nn$result.matrix
WAUS.nn.pred = compute(WAUS.nn, WAUS.Ann.test)
WAUS.nn.predr = round(WAUS.nn.pred$net.result, 0)
cm = table(observed = WAUS.Ann.test$MHT, predicted = WAUS.nn.predr)
cm
accuracy <- sum(diag(cm)) / sum(cm)
accuracy





#Extension SVM support vector machine

train.svm = svm(MHT ~ . - Location, data = WAUS.train)
svm.predict = predict(train.svm, WAUS.test)

table(actual = WAUS.test$MHT, predicted = svm.predict)
confusionMatrix(svm.predict, WAUS.test$MHT) 

















