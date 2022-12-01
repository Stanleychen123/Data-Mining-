library(readxl)
myData <- read_excel("Desktop/Ch14_Q43_Data_File.xlsx")
View(myData)

myData1 <- myData[,5:7]
suppressWarnings(RNGversion("3.5.3"))
set.seed(1)
kResult <- pam(myData1, k=3)
summary(kResult)
plot(kResult)


myData1 <- myData[,2:9]
suppressWarnings(RNGversion("3.5.3"))
set.seed(1)
kResult <- pam(myData1, k=3)
summary(kResult)