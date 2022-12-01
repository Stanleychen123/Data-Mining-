library(readxl)
mydata <- read_excel("~/Desktop/MGMT 473/Ch12_Q43_Data_File.xlsx")
View(mydata)
library(caret)
library(gains)

age_bin <- quantile(mydata$Age, probs=seq(0,1,by=0.25))
income_bin <- quantile(mydata$Income, probs = seq(0,1,by=0.25))

mydata$binned_age <- cut(mydata$Age, breaks = age_bin, labels = c("1","2","3","4"),
                         include.lowest = T, right = T)
mydata$binned_income <- cut(mydata$Income, breaks = income_bin, labels = c("1","2","3","4"),
                         include.lowest = T, right = T)
View(mydata)

mydata1 <- scale(mydata[2:4])
mydata1 <- data.frame(mydata1,mydata$Performance)
colnames(mydata1)[4] <- "Performance"
mydata1$Performance <- as.factor(mydata1$Performance)

set.seed(1)
myindex <- createDataPartition(mydata1$Performance,p=0.6,list = F)
trainset <- mydata1[myindex,]
validationset <- mydata1[-myindex,]

mycontrol <- trainControl(method = "cv", number=10)
mygrid <- expand.grid(.k=c(1:10))

set.seed(1)
nb_fit <- train(Performance~., data = trainset, method = "nb", trControl=mycontrol)
nb_fit
nb_class <- predict(nb_fit, newdata = validationset)
confusionMatrix(nb_class,validationset$Performance,positive="1")

nb_class_prob <- predict(nb_fit, newdata = validationset,type = "prob")
nb_class_prob
validationset$Performance <- as.numeric(as.character(validationset$Performance))
gains_table <- gains(validationset$Performance, nb_class_prob[,2])
gains_table

plot(c(0, gains_table$cume.pct.of.total*sum(validationset$Performance))~c(0,gains_table$cume.obs),
     xlab= "# of cases", ylab = "cumulative",type="l")
lines(c(0,sum(validationset$Performance))~c(0, dim(validationset)[1]),col="red", lty=2)

barplot(gains_table$mean.resp/mean(validationset$Performance), names.arg = gains_table$depth,
        xlab="Percentile", ylab = "lift", ylim=c(0,2), main = "decile-wise lift chart")


roc_object <- roc(validationset$Performance, nb_class_prob[,2])
plot.roc(roc_object)
auc(roc_object)