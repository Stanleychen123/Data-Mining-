suppressWarnings(RNGversion("3.5.3"))
library(caret)
library(gains)
library(pROC)
library(readxl)		

myData <- read_excel("~/Desktop/MGMT 473/Survey.xlsx")
myData <- na.omit(myData)
myData <- myData[,-1]
View(myData)


myData1<-myData[,c(1:17, 19:23)]	
myData1<-data.frame(myData1, myData$HealthPlan)
colnames(myData1)[23]<-'HealthPlan'
myData1$HealthPlan<-as.factor(myData1$HealthPlan)
set.seed(1)						
myIndex<-createDataPartition(myData1$HealthPlan,p=0.6,list=FALSE)
trainSet<-myData1[myIndex,]
validationSet<-myData1[-myIndex,]

myCtrl<-trainControl(method='cv',number=10)			
myGrid<-expand.grid(.k=c(1:10)) 	

set.seed(1)							
KNN_fit<-train(HealthPlan~.,data=trainSet,method='knn',trControl=myCtrl,tuneGrid=myGrid)
KNN_fit	

KNN_Class <- predict(KNN_fit, newdata = validationSet)
confusionMatrix(KNN_Class, validationSet$HealthPlan, positive = '1')

KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
validationSet$HealthPlan <- as.numeric(as.character(validationSet$HealthPlan)) 
gains_table <- gains(validationSet$HealthPlan, KNN_Class_prob[,2])
gains_table
View(validationSet)
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$HealthPlan))~c(0, gains_table$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(validationSet$HealthPlan))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$HealthPlan), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0,3), main="Decile-Wise Lift Chart")
roc_object <- roc(validationSet$HealthPlan, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)
