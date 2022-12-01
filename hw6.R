#Q2
library(readxl)
mydata <- read_excel("Desktop/Ch9_Q17_V16_Data_File.xlsx")
mydata$Single <- ifelse(mydata$Plan=="Single",1,0)
logistic <- glm(Loyal~Age+Income+Single, family = binomial, data = mydata)
summary(logistic)

View(mydata)
predict(logistic,data.frame(Age=c(40,60),Income=80,Single=1), type = "response")
predict(logistic,data.frame(Age=c(40,60),Income=80,Single=0), type = "response")

#Q3
library(readxl)
mydata2 <- read_excel("Ch9_Q31_V05_Data_File.xlsx")
View(mydata2)
logistic <- glm(Subscribe~Discount+Age, family = binomial, data = mydata2)
pHatLog <- predict(logistic,type = "response")
yHatLog <- ifelse(pHatLog>=0.5,1,0)
100*mean(mydata2$Subscribe==yHatLog)
logistic <- glm(Subscribe~Discount+Age+Sex, family = binomial, data = mydata2)
pHatLog <- predict(logistic,type = "response")
yHatLog <- ifelse(pHatLog>=0.5,1,0)
100*mean(mydata2$Subscribe==yHatLog)
predict(logistic,data.frame(Discount=10,Age=50,Sex=c("Male","Female")), type = "response")


#Q4
library(readxl)
mydata3 <- read_excel("Desktop/MGMT 473/Ch9_Q43_V18_Data_File.xlsx")
View(mydata3)
tdata <- mydata3[1:300,]
vdata <- mydata3[301:400,]

logistic <- glm(Default~LTV+FICO+Age, family = binomial, data = tdata)
pHatLog <- predict(logistic,vdata,type = "response")
yHatLog <- ifelse(pHatLog>=0.5,1,0)
library(caret)
confusionMatrix(as.factor(yHatLog),as.factor(vdata$Default),positive='1')


logistic <- glm(Default~LTV+FICO+Age, family = binomial, data = tdata)
cutoff <- mean(tdata$Default==1)
yHatLog <- ifelse(pHatLog>=cutoff,1,0)
library(caret)
confusionMatrix(as.factor(yHatLog),as.factor(vdata$Default),positive='1')

