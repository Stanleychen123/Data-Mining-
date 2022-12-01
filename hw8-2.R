# Import the Subscription_Data data file into a data frame (table) and label it myData. Import the Subscription_Score data file into a data frame(table) and label it myScoreData.
myData <- read_excel("~/Desktop/MGMT 473/Ch12_Q19_Data_File.xlsx")
View(mydata)
# If you are using a version of R that is newer than version 3.5.3, execute the following line of code:
suppressWarnings(RNGversion("3.5.3"))

##Install the following r packages if you have not already done so.
install.packages(c("caret", "gains", "pROC"))

#a and b
library(caret)
library(gains)
library(pROC)
myData$Sex <- ifelse(myData$Sex == 'Female', 1, 0)
myData1<- scale(myData[2:4])
myData1<- data.frame(myData1, myData$Subscribe)
colnames(myData1)[4] <- 'Subscribe'
myData1$Subscribe<- as.factor(myData1$Subscribe)
set.seed(1)
myIndex<- createDataPartition(myData1$Subscribe, p=0.6, list=FALSE)
trainSet <- myData1[myIndex,]
validationSet <- myData1[-myIndex,]
myCtrl <- trainControl(method="cv", number=10)
myGrid <- expand.grid(.k=c(1:10))
set.seed(1)
KNN_fit <- train(Subscribe ~ ., data=trainSet, method = "knn", trControl=myCtrl, tuneGrid = myGrid)
KNN_fit

#c
KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
confusionMatrix(as.factor(ifelse(KNN_Class_prob[,2]>0.65, '1', '0')), as.factor(validationSet$Subscribe), positive = '1')
confusionMatrix(as.factor(ifelse(KNN_Class_prob[,2]>0.35, '1', '0')), as.factor(validationSet$Subscribe), positive = '1')
confusionMatrix(as.factor(ifelse(KNN_Class_prob[,2]>0.5, '1', '0')), as.factor(validationSet$Subscribe), positive = '1')

#d
myScoreData <- read_excel("~/Desktop/MGMT 473/Ch12_Q19_Score_File.xlsx")
myScoreData$Sex <- ifelse(myScoreData$Sex == "Female", 1, 0)
PreProcessing <- preProcess(myData[ , 2:4], method = c("center", "scale"))
myScoreData1 <- predict(PreProcessing, myScoreData) 
KNN_Score <- predict(KNN_fit, newdata=myScoreData1, type = "prob")
KNN_Score
myScoreData <- data.frame(myScoreData, KNN_Score)
Avg_Prob <- mean(KNN_Score[,2])
Avg_Prob

#e
KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
roc_object <- roc(validationSet$Subscribe, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)
