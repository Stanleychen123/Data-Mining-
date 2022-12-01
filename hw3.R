library(readxl)
mydata <- read_excel("~/Desktop/Ch11_Q35_V10_Data_File.xlsx")

library(forecast)
accuracy1 <- accuracy(mydata$`Predicted Spending 1`,mydata$`Actual Spending`)
accuracy1 <- accuracy(mydata$`Predicted Spending 2`,mydata$`Actual Spending`)

accuracy1
accuracy2
