#library
library(ggplot2)
library(dplyr)

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
library(class)

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

#Explore the data************************************************************************************************

#proportion of of MHT-------------------------------------------

MHT_data <- ifelse(WAUS$MHT == 0, "No", "Yes")
MHT_count <- table(MHT_data)
MHT_count

bp <- barplot(MHT_count, ylim = c(0, 1100), main = "Proportion of MHT")
text(x = bp, y = MHT_count + 50, labels = MHT_count, col = "black")

#Spread of the numeric Data Box plot ----------------------------------------------
num_attribute = WAUS
num_attribute = num_attribute %>% mutate_if(is.character, as.factor)

#box plot
par(mar=c(8,4,2,2)) 
boxplot(num_attribute[, c(3:13, 16:21)], las = 2, main = "Box plot of all attribute")

boxplot(num_attribute[,14:15], main = "Box plot for pressure attribute") #pressure9

#percentage of N/A value---------------------------------------------------------------
as.data.frame(colMeans(is.na(WAUS))* 100) #percentage missing value of the data

#correlation matrix of numerical attribute
cor_data = WAUS[,c(3,4,5,9,12,13,14,15,18,19,21)]
na.omit(cor_data)
cor_matrix = cor(cor_data, use = "pairwise.complete.obs")
as.table(cor_matrix)

write.csv(cor_matrix, file = "cor_matrix.csv")
matrix = read.csv("cor_matrix.csv")
View(matrix)

#Pre processing*******************************************************************************

WAUS = WAUS[,c(2,3,4,5,8,9,10,11,12,13,14,15,18,19,20,21,22)] #year, evaporation, sunshine, cloud9am and cloud3pm are removed

WAUS.Ann = WAUS #Setting up Data for ANN 

WAUS$MHT = ifelse(WAUS$MHT == 1, "Yes", "No") #change MHT from numeric 1,0 to yes and no
WAUS = na.omit(WAUS)

#convert categorical data as factor
WAUS <- WAUS %>% mutate_if(is.character, as.factor)

#Setting up training and test data*****************************************************

set.seed(31538312) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

#Confusion matrix accuracy function
matrix_accuracy = function(table){
  result <- sum(diag(table)) / sum(table)
  print(result)
}

#Classification model*****************************************

#Decision Tree---------------------------------

#train tree
train.tree = tree(MHT ~.-Location , data = WAUS.train, method = "class")

#plotting tree
plot(train.tree)
text(train.tree, pretty = 0)
title("Decisition Tree for predicting MHT")

#predicted tree
WAUS.predict = predict(train.tree, WAUS.test, type = "class")

#confusion matrix
tree.table = table(predicted = WAUS.predict, actual = WAUS.test$MHT)
tree.table

#prepare tree ROC curve
tree.roc = predict(train.tree, WAUS.test, type = "vector")
tree.roc.predict = prediction(tree.roc[,2], WAUS.test$MHT)
tree.roc.performance = performance(tree.roc.predict, "tpr", "fpr")

#NayesBayes---------------------------------

train.bayes = naiveBayes(MHT ~.-Location , data = WAUS.train)

bayes.predict = predict(train.bayes, WAUS.test)

#confusion matrix
bayes.table = table(predicted = bayes.predict, actual = WAUS.test$MHT)
bayes.table

#prepare bayes ROC curve
bayes.roc = predict(train.bayes, WAUS.test, type = "raw")
bayes.roc.predict = prediction(bayes.roc[,2], WAUS.test$MHT)
bayes.roc.performance = performance(bayes.roc.predict, "tpr", "fpr")

#Bagging----------------------------------------

train.bagging = bagging(MHT ~. -Location , data = WAUS.train, mfinal = 10)
bagging.predict = predict.bagging(train.bagging, WAUS.test)

print(bagging.predict$confusion) #confusion matrix

#Prepare Bagging ROC curve
bag.roc.predict = prediction(bagging.predict$prob[,2], WAUS.test$MHT)
bag.roc.performance = performance(bag.roc.predict, "tpr", "fpr")

#Boosting--------------------------------------------

train.boost = boosting(MHT ~.-Location , data = WAUS.train, mfinal = 10)
boosting.predict = predict.boosting(train.boost, WAUS.test )

print(boosting.predict$confusion) #confusion matrix

#prepare boosting ROC curve
boost.roc.predict = prediction(boosting.predict$prob[,2], WAUS.test$MHT)
boost.roc.performance = performance(boost.roc.predict, "tpr", "fpr")

#Random Forest -------------------------------------------
train.forest = randomForest(MHT ~.-Location , data = WAUS.train, na.action = na.exclude)
forest.predict = predict(train.forest, WAUS.test)

#confusion matrix
forest.table = table(predicted = forest.predict, actual = WAUS.test$MHT)
forest.table

#prepare forest ROC curve
forest.roc = predict(train.forest, WAUS.test, type = "prob")
forest.roc.predict = prediction(forest.roc[,2], WAUS.test$MHT)
forest.roc.performance = performance(forest.roc.predict, "tpr", "fpr")


#Comparing initial results of the classification********************************************************

#Accuracy of model----------------------------------------------------------------
accuracy_matrix <- matrix(c(
  matrix_accuracy(tree.table),
  matrix_accuracy(bayes.table),
  matrix_accuracy(bagging.predict$confusion),
  matrix_accuracy(boosting.predict$confusion),
  matrix_accuracy(forest.table) 
), ncol = 1)

rownames(accuracy_matrix) <- c("Tree", "Bayesian", "Bagging", "Boosting", "Forest")
colnames(accuracy_matrix) <- "Accuracy"

print(accuracy_matrix)



#ROC curve of 5 classification model-------------------------------------------------------

dev.off()
roc.curve <- function(){
  # Plot the first ROC curve
  plot(tree.roc.performance, col = "blue", lwd = 2, main = "ROC curve for the 5 classification model")
  abline(0,1, lwd = 2)
  # Add the other ROC curves to the plot
  plot(bayes.roc.performance, add = TRUE, lwd = 2, col = "green")
  plot(bag.roc.performance, add = TRUE, lwd = 2, col = "orange")
  plot(boost.roc.performance, add = TRUE, lwd = 2, col = "grey")
  plot(forest.roc.performance, add = TRUE, lwd = 2 ,col = "red")
  
  # Add a legend to the plot
  legend("bottomright", legend = c("Tree", "Bayesian", "Bagging", "Boosting", "Random Forest"), col = c("blue", "green", "orange", "grey", "red"), lty = 1)
  
}

roc.curve()


#AUC-------------------------------------------------------------

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

#Identifying important attributes of for each of the classification*****************************************************
cat("\n#Tree Attribute  Importance\n") 
print(summary(train.tree)) 
cat("\n#Baging Attribute  Importance\n") 
print(as.data.frame(train.bagging$importance)) 
cat("\n#Boosting Attribute  Importance\n") 
print(as.data.frame(train.boost$importance)) 
cat("\n#Random Forest Attribute  Importance\n") 
print(train.forest$importance)

#Improving the tree*************************************************************************

#Remove irrelevant attribute from the model
WAUS$RainToday <- NULL
WAUS$RISK_MM <- NULL
WAUS$WindGustSpeed <- NULL
WAUS$WindSpeed3pm <- NULL
WAUS$WindSpeed9am <- NULL

#train with removed column datasample-----------------------------------------------------------
set.seed(31538312) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

#New Tree--------------------------------------------------------------
cv1 = cv.tree(train.tree, FUN = prune.misclass) #cross validation
cv1 #size 4 is a sweet spot

train.tree = tree(MHT ~.-Location , data = WAUS.train, method = "class")
prune.tree = prune.misclass(train.tree, best = 4)

dev.off()
plot(prune.tree)
text(prune.tree, pretty = 0)
title("Pruned Decisition Tree for predicting MHT")

PD.predict = predict(prune.tree, WAUS.test, type = "class")
tree.table2 = table(actual = WAUS.test$MHT, predicted = PD.predict) #new confusion matrix
tree.table2

matrix_accuracy(tree.table2) #accuracy of prune tree

PD.roc.predict = predict(prune.tree, WAUS.test, type = "vector")
pred = prediction(PD.roc.predict[,2], WAUS.test$MHT)
perf = performance(pred, "tpr", "fpr")

pruned.auc = performance(pred, "auc")
print(as.numeric(pruned.auc@y.values)) #AUC = 0.5124925

#************************************************************************************************
#IMPORTANT NOTE REPEAT THE CODE FROM NAIVES BAYES TO RANDOM FOREST WITH THE NEW TRAIN DATA BEFORE
#GETTING NEW ACCURACY AND ROC/AUC VALUE BELOW (repeat code from line 121-170)

#Confusion Matrix Accuracy 2-------------------------------------------------------------------------------

accuracy_matrix2 <- matrix(c(
  matrix_accuracy(tree.table2),
  matrix_accuracy(bayes.table),
  matrix_accuracy(bagging.predict$confusion),
  matrix_accuracy(boosting.predict$confusion),
  matrix_accuracy(forest.table) 
), ncol = 1)

rownames(accuracy_matrix2) <- c("Tree", "Bayesian", "Bagging", "Boosting", "Forest")
colnames(accuracy_matrix2) <- "Accuracy"

print(accuracy_matrix2)

#new ROC curve----------------------------------------------------------------------------------

dev.off()

roc.curve2 <- function(){
  # Plot the first ROC curve
  plot(perf, col = "blue", lwd = 2, main = "ROC curve for the improved 5 classification model")
  abline(0,1, lwd = 2)
  # Add the other ROC curves to the plot
  plot(bayes.roc.performance, add = TRUE, lwd = 2, col = "green")
  plot(bag.roc.performance, add = TRUE, lwd = 2, col = "orange")
  plot(boost.roc.performance, add = TRUE, lwd = 2, col = "grey")
  plot(forest.roc.performance, add = TRUE, lwd = 2 ,col = "red")
  
  # Add a legend to the plot
  legend("bottomright", legend = c("Tree", "Bayesian", "Bagging", "Boosting", "Random Forest"), col = c("blue", "green", "orange", "grey", "red"), lty = 1)
  
}

roc.curve2()

#AUC 2

bayes.auc = performance(bayes.roc.predict, "auc")
bag.auc = performance(bag.roc.predict, "auc")
boost.auc = performance(boost.roc.predict, "auc")
forest.auc = performance(forest.roc.predict, "auc")

auc_table2 <- data.frame(
  Classifier = c("Tree", "Bayesian", "Bagging", "Boosting", "Random Forest"),
  AUC = c(as.numeric(pruned.auc@y.values), as.numeric(bayes.auc@y.values), 
          as.numeric(bag.auc@y.values), as.numeric(boost.auc@y.values), 
          as.numeric(forest.auc@y.values))
)

auc_table2


#Extension Linear regression model********************************************************************

log_model <- glm(MHT ~. -Location, family = binomial, data = WAUS.train)

log_predict <- predict(log_model, newdata = WAUS.test, type = "response")
log_predict <- ifelse(log_predict > 0.5, 1, 0)

log_matrix <- table(Actual = WAUS.test$MHT, Predicted = log_predict) #confusion matrix
print(log_matrix)

matrix_accuracy(log_matrix) #0.5614849

#ROC and AUC
log_pr <- prediction(log_predict, WAUS.test$MHT)
log_perf <- performance(log_pr, measure = "tpr", x.measure = "fpr")

dev.off()
plot(log_perf, col = "blue", lwd = 2, main = "ROC curve for Logistic Linear Regression Model")
abline(0,1)
auc(WAUS.test$MHT, log_predict)


#ANN*************************************************

#prepare data for ANN

#install.packages("neuralnet") 
library(neuralnet)


WAUS.Ann = na.omit(WAUS.Ann)
WAUS.mm = model.matrix(~WindGustDir+WindDir9am+WindDir3pm, data = WAUS.Ann) #model matrix for wind direction attribute
WAUS.Ann = cbind(WAUS.Ann, WAUS.mm)
WAUS.Ann$MHT = as.numeric(WAUS.Ann$MHT)

#Training the data----------------------------------------------------------
set.seed(31538312) 
train.Ann.row = sample(1:nrow(WAUS.Ann), 0.7*nrow(WAUS.Ann))
WAUS.Ann.train = WAUS.Ann[train.Ann.row,]
WAUS.Ann.test = WAUS.Ann[-train.Ann.row,]

WAUS.nn = neuralnet(MHT == 1  ~ MinTemp + MaxTemp + Rainfall + WindSpeed9am + Pressure9am + Pressure3pm + Temp9am + Temp3pm +
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

#confusion Matrix
ann.cm = table(observed = WAUS.Ann.test$MHT, predicted = WAUS.nn.predr)
ann.cm

matrix_accuracy(ann.cm)



























