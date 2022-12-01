library(readxl)
myData <- read_excel("Desktop/MGMT 473/Ch13_Q41_Data_File.xlsx")
myScoreData <- read_excel("Desktop/MGMT 473/Ch13_Q41_Score_File.xlsx")
  
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(forecast)

suppressWarnings(RNGversion("3.5.3"))
set.seed(1)
myIndex <- createDataPartition(myData$Spending, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

set.seed(1)
default_tree <- rpart(Spending ~., data = trainSet, method = "anova")
summary(default_tree)
prp(default_tree, type = 1, extra = 1, under = TRUE)

set.seed(1)
full_tree <- rpart(Spending ~ ., data = trainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)

printcp(full_tree)
pruned_tree <- prune(full_tree, cp =0.012344)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

predicted_value <- predict(pruned_tree, validationSet)
accuracy(predicted_value, validationSet$Spending)

predicted_value_score <- predict(pruned_tree, myScoreData)
predicted_value_score 

mean(predicted_value_score)
median(predicted_value_score)

