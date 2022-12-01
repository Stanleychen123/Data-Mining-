library(readxl)
mydata <- read_excel("~/Desktop/Ch12_Q30_Data_File.xlsx")

mydata$TOEFL <- ifelse(mydata$TOEFL=="H",1,0)
View(mydata)

mydata1 <- scale(mydata[2:3])
mydata1 <- data.frame(mydata1,mydata$Accept)
colnames(mydata1)[3] <- "Accept"
mydata1$Accept<- as.factor(mydata1$Accept)

set.seed(1)
myindex <- createDataPartition(mydata1$Accept,p=0.6,list = F)
trainset <- mydata1[myindex,]
validationset <- mydata1[-myindex,]

mycontrol <- trainControl(method = "cv", number=10)
mygrid <- expand.grid(.k=c(1:10))
set.seed(1)

nb_fit <- train(Accept~., data = trainset, method = "nb", trControl=mycontrol)
nb_fit
nb_class <- predict(nb_fit, newdata = validationset)
confusionMatrix(nb_class,validationset$Accept,positive="1")

nb_class_prob <- predict(nb_fit, newdata = validationset,type = "prob")
roc_object <- roc(validationset$Accept, nb_class_prob[,2])
plot.roc(roc_object)
auc(roc_object)