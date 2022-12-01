library(readxl)
myData <- read_excel("Desktop/MGMT 473/Ch13_Q63_Data_File.xlsx")
myScoreData 
  
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(adabag)

suppressWarnings(RNGversion("3.5.3"))
myData$Performance <- as.factor(myData$Performance)
myData$Management <- as.factor(myData$Management)
set.seed(1)
myIndex <- createDataPartition(myData$Performance, p=0.6, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

set.seed(1)
bagging_tree <- randomForest(Performance ~ ., data = trainSet, ntree=100, mtry=3, importance=TRUE)
random_tree <- randomForest(Performance ~ ., data = trainSet, ntree=100, mtry=2, importance=TRUE)
varImpPlot(bagging_tree, type=1)
varImpPlot(random_tree, type=1)

bagging_class <- predict(bagging_tree, validationSet)
random_class <- predict(random_tree, validationSet)
bagging_prob <- predict(bagging_tree, validationSet, type= 'prob')
random_prob <- predict(random_tree, validationSet, type= 'prob')
confusionMatrix(as.factor(ifelse(bagging_prob[,2]>0.6, '1', '0')), validationSet$Performance, positive = '1')
confusionMatrix(as.factor(ifelse(random_prob[,2]>0.6, '1', '0')), validationSet$Performance, positive = '1')

validationSet$Performance <- as.numeric(as.character(validationSet$Performance))
bagging_table <- gains(validationSet$Performance, bagging_prob[,2])
random_table <- gains(validationSet$Performance, random_prob[,2])
bagging_table
random_table

plot(c(0, bagging_table$cume.pct.of.total*sum(validationSet$Performance)) ~ c(0, bagging_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l", main="Cumulative Lift Comparison: Random Trees in Red")
lines(c(0, random_table$cume.pct.of.total*sum(validationSet$HELOC)) ~ c(0, random_table$cume.obs), col="red")

bagging_roc <- roc(validationSet$Performance, bagging_prob[,2])
plot.roc(bagging_roc, main="Bagging ROC")
random_roc <- roc(validationSet$Performance, random_prob[,2])
plot.roc(random_roc, main="Random ROC")
auc(bagging_roc)
auc(random_roc)
