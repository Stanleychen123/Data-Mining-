library(readxl)
myData <- read_excel("Desktop/MGMT 473/Ch13_Q13_Data_File.xlsx")
myScoreData <- read_excel("Desktop/MGMT 473/Ch13_Q13_Score_File.xlsx")

library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)

suppressWarnings(RNGversion("3.5.3"))
myData$ContinueEdu <- as.factor(myData$ContinueEdu)
set.seed(1)
myIndex <- createDataPartition(myData$ContinueEdu, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

set.seed(1)
default_tree <- rpart(ContinueEdu ~., data = trainSet, method = "class")
prp(default_tree, type = 1, extra = 1, under = TRUE)

set.seed(1)
full_tree <- rpart(ContinueEdu ~., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)

pruned_tree <- prune(full_tree, cp = 0.0172415)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

predicted_class <- predict(pruned_tree, validationSet, type = "class")
confusionMatrix(predicted_class, validationSet$ContinueEdu, positive = "1")
predicted_prob <- predict(pruned_tree, validationSet, type= 'prob')
head(predicted_prob)

confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.1, '1', '0')), validationSet$ContinueEdu, positive = '1')


validationSet$ContinueEdu <- as.numeric(as.character(validationSet$ContinueEdu))
gains_table <- gains(validationSet$ContinueEdu, predicted_prob[,2])
gains_table

barplot(gains_table$mean.resp/mean(validationSet$ContinueEdu), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 10.0), main="Decile-Wise Lift Chart")

roc_object <- roc(validationSet$ContinueEdu, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)

predicted_prob <- predict(pruned_tree, myScoreData, type= 'prob')
head(predicted_prob)

