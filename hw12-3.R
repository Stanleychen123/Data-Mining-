library(cluster)
library(readxl)

myData <- read_excel("Desktop/Ch14_Q31_Data_File.xlsx")
View(myData)

suppressWarnings(RNGversion("3.5.3"))
set.seed(1)
kResult <- pam(myData, k=3)
kResult <- pam(myData, k=4)
kResult <- pam(myData, k=5)
summary(kResult)

plot(kResult)