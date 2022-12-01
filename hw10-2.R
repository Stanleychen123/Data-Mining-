library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
suppressWarnings(RNGversion("3.5.3"))

library(readxl)
mydata <- read_excel("Desktop/MGMT 473/Ch13_Q19_Data_File.xlsx")
scoredata <- read_excel("Desktop/MGMT 473/Ch13_Q19_Score_File.xlsx")

mydata$Grad<- as.factor(mydata$Grad)
set.seed(1)
myIndex <- createDataPartition(mydata$Grad, p=0.7, list=FALSE)
trainSet <- mydata[myIndex,]
validationSet <- mydata[-myIndex,]

set.seed(1)
default_tree <- rpart(Grad ~., data = trainSet, method = "class")
prp(default_tree, type = 1, extra = 1, under = TRUE)

set.seed(1)
full_tree <- rpart(Grad ~ ., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)

pruned_tree <- prune(full_tree, cp = 0.0034723 )
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

predicted_class <- predict(pruned_tree, validationSet, type = "class")
confusionMatrix(predicted_class, validationSet$Grad, positive = "1")

predicted_prob <- predict(pruned_tree, validationSet, type= 'prob')

validationSet$Grad <- as.numeric(as.character(validationSet$Grad))
gains_table <- gains(validationSet$Grad, predicted_prob[,2])
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Grad)) ~ c(0, gains_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$Grad))~c(0, dim(validationSet)[1]), col="red", lty=2)

roc_object <- roc(validationSet$Grad, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)

predicted_class_score <- predict(pruned_tree, scoredata, type = "class")
predicted_class_score




