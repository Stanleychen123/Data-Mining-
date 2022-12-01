#Q1
library(readxl)
mydata <- read_excel("Desktop/Ch7_Q7_V13_Data_File.xlsx")
model <- lm(Salary~Education,data = mydata)
summary(model)
predict(model,data.frame(Education = 5))

#Q2  
library(readxl)
mydata2 <- read_excel("Desktop/Ch8_Q11_V11_Data_File.xlsx")
model2 <- lm(Consumption~Income,data = mydata2)
predict(model2,data.frame(Income=75000))
summary(model2)


model3 <- lm(Consumption~Income+Urban,data = mydata2)
summary(model3)
predict(model3, data.frame(Income=75000, Urban=1))
predict(model3, data.frame(Income=75000, Urban=0))

mydata2$interaction <- mydata2$Income*mydata2$Urban
model4 <- lm(Consumption~Income+Urban+interaction,data = mydata2)
summary(model4)
predict(model4, data.frame(Income=75000, Urban=1,interaction=75000))
predict(model4, data.frame(Income=75000, Urban=0,interaction=0))

#Q3
library(readxl)
mydata3 <- read_excel("Desktop/Ch7_Q33_V05_Data_File.xlsx")
model5 <- lm(Time~Miles+Load+Speed+Oil,data = mydata3)
summary(model5)

#Q4
library(readxl)
mydata4 <- read_excel("Desktop/MGMT 473/Ch8_Q25_V13_Data_File.xlsx")
mydata4$hoursquare <- (mydata4$Hours)^2
model6 <- lm(GPA~Hours+hoursquare,data = mydata4)
summary(model6)

Coeff<-coef(model6)
x <- -Coeff[2]/(2*Coeff[3])
x

#Q5
library(readxl)
mydata5 <- read_excel("Desktop/MGMT 473/Ch8_Q33_v07_Data_File.xlsx")
#Estimate the linear model that uses Rent as the response variable
model7 <- lm(Rent~Beds+Baths+Sqft, data = mydata5)
#Estimate the exponential model that uses log of Rent as the response variable
model8 <- lm(log(Rent)~Beds+Baths+Sqft, data = mydata5)
summary(model7)
summary(model8)
#Compute the predicted rent 
predict(model7,data.frame(Beds=3, Baths=2,Sqft=1500))
x <- predict(model8,data.frame(Beds=3,Baths=2,Sqft=1500))
se <- sigma(model8)

#Find the the proportion 
x1 <- lm(log(Rent)~Beds+Baths+Sqft,data=mydata5)
x2 <- predict(model8)
se <- sigma(model8)
pred_rent <- exp(x2+se^2/2)
cor(mydata5$Rent,pred_rent)^2 

#Q6
library(readxl)
myData<- read_excel("Desktop/MGMT 473/Ch8_Q45_Data_File.xlsx")


training <- myData[1:90,]
validation <- myData[91:120,]

model9 <- lm(Health~Social+Income+College,training)
summary(model9)
model10 <- lm(Health~Social+Income+College+Social*Income+Social*College,training)
summary(model10)

library(forecast)
x3 <- predict(model9,validation)
accuracy(validation$Health,x3)
x4 <- predict(model10,validation)
accuracy(validation$Health,x4)


#Use the k-fold method, with k = 4, to calculate the average RMSE of the two models
TData <- myData[c(1:90),]; VData <- myData[91:120,]
Model1 <- lm(Health ~ Social + Income + College, data = TData)
Pred0 <- predict(Model1, VData)
RMSE1a <- sqrt(mean((VData$Health-Pred0)^2))
Model2 <- lm(Health ~ Social + Income + College + Social*Income + Social*College, data = TData)
Pred2 <- predict(Model2, VData)
RMSE2a <- sqrt(mean((VData$Health-Pred2)^2))
TData <- myData[c(1:60, 91:120),]; VData <- myData[61:90,]
Model1 <- lm(Health ~ Social + Income + College, data = TData)
Pred1 <- predict(Model1, VData)
RMSE1b <- sqrt(mean((VData$Health-Pred1)^2))
Model2 <- lm(Health ~ Social + Income + College + Social*Income + Social*College, data = TData)
Pred2 <- predict(Model2, VData)
RMSE2b <- sqrt(mean((VData$Health-Pred2)^2))
TData <- myData[c(1:30, 61:120),]; VData <- myData[31:60,]
Model1 <- lm(Health ~ Social + Income + College, data = TData)
Pred1 <- predict(Model1, VData)
RMSE1c <- sqrt(mean((VData$Health-Pred1)^2))
Model2 <- lm(Health ~ Social + Income + College + Social*Income + Social*College, data = TData)
Pred2 <- predict(Model2, VData)
RMSE2c <- sqrt(mean((VData$Health-Pred2)^2))
TData <- myData[c(31:120),]; VData <- myData[1:30,]
Model1 <- lm(Health ~ Social + Income + College, data = TData)
Pred1 <- predict(Model1, VData)
RMSE1d <- sqrt(mean((VData$Health-Pred1)^2))
Model2 <- lm(Health ~ Social + Income + College + Social*Income + Social*College, data = TData)
Pred2 <- predict(Model2, VData)
RMSE2d <- sqrt(mean((VData$Health-Pred2)^2))
c(RMSE1a,RMSE1b,RMSE1c,RMSE1d)
(RMSE1a+RMSE1b+RMSE1c+RMSE1d)/4
c(RMSE2a,RMSE2b,RMSE2c,RMSE2d)
(RMSE2a+RMSE2b+RMSE2c+RMSE2d)/4
