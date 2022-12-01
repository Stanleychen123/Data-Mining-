library(readxl)
myData <- read_excel("~/Desktop/MGMT 473/Ch14_Q22_R_V11_Data_File.xlsx")
View(myData)

myData$Urban<-as.factor(myData$Urban)
myData$White<-as.factor(myData$White)
myData$Christian<-as.factor(myData$Christian)
d<-daisy(myData[,1:8], metric='gower')   
mResult<- agnes(d, method='ward')	 
plot(mResult)	             

mClusters<-cutree(mResult, k=4)	       
myData<-data.frame(myData, mClusters)   
View(myData)

summary(as.factor(mClusters))
