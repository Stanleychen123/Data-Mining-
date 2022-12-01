#b1
mydata <- read_excel("Desktop/MGMT 473/Ch12_Q8_Data_File.xlsx")
View(mydata)
library(caret)

mydata1 <- scale(mydata[2:3])
mydata1 <- data.frame(mydata1,mydata$Admit)
colnames(mydata1)[3] <- "admit"
mydata1$admit <- as.factor(mydata1$admit)

set.seed(1)
myindex <- createDataPartition(mydata1$admit,p=0.6,list = F)
trainset <- mydata1[myindex,]
validationset <- mydata1[-myindex,]

mycontrol <- trainControl(method = "cv", number=10)
mygrid <- expand.grid(.k=c(1:10))

set.seed(1)
KNN_fit <- train(admit~., data = trainset, method = "knn", trControl=mycontrol, tuneGrid=mygrid)
KNN_fit
KNN_class <- predict(KNN_fit, newdata = validationset)
confusionMatrix(KNN_class,validationset$admit,positive="1")

#c1
install.packages("pROC")
library("pROC")
install.packages("pROC")
KNN_class_prob <- predict(KNN_fit, newdata = validationset,type = "prob")
KNN_class_prob
roc_object <- roc(validationset$admit, KNN_class_prob[,2])
plot.roc(roc_object)
auc(roc_object)

#c2
nb_fit <- train(admit~., data = trainset, method = "nb", trControl=mycontrol)
nb_fit
nb_class <- predict(nb_fit, newdata = validationset)
confusionMatrix(nb_class, as.factor(validationset$admit), positive="1") 

#d
myScoreData <- read_excel("~/Desktop/Ch12_Q8_Score_File.xlsx")
myScoreData1<-scale(myScoreData)	
KNN_Score<-predict(KNN_fit,newdata=myScoreData1)
myScoreData<-data.frame(myScoreData,KNN_Score)
View(myScoreData)	



