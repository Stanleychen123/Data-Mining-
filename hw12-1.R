library(readxl)
library(cluster)
myData <- read_excel("Desktop/Ch14_Q9_V08_Data_File.xlsx")

myData_std <- scale(myData[,2:5])
d <- dist(myData_std, method = "euclidean")
aResult <- agnes(d, diss = T, method = "ward")
aResult
plot(aResult)

myData$City<-as.factor(myData$City)
myData <- myData[myData$City=="1",]
View(myData)
myData_std <- scale(myData[,2:5])
d <- dist(myData_std, method = "euclidean")
aResult <- agnes(d, diss = T, method = "ward")
aResult
plot(aResult)  


myData$City<-as.factor(myData$City)
myData <- myData[myData$City=="0",]
View(myData)
myData_std <- scale(myData[,2:5])
d <- dist(myData_std, method = "euclidean")
aResult <- agnes(d, diss = T, method = "ward")
aResult
plot(aResult)  