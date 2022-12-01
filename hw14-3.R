library(arules)
myData<-read.transactions("~/Desktop/MGMT 473/Ch14_Q65_Data_File.csv", format='basket',sep=',')
inspect(myData[1:5])

itemFrequency(myData)
itemFrequencyPlot(myData)

rules<-apriori(myData,parameter = list(minlen=2,supp=0.1,conf=0.5))
srules<-sort(rules,by='lift',decreasing='TRUE')
inspect(srules)