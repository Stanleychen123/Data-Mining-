library(arules)
myData<-read.transactions("~/Desktop/MGMT 473/Ch14_Q47_Data_File_CSV.csv", format='basket',sep=',')
inspect(myData[1:7])

itemFrequency(myData)
itemFrequencyPlot(myData)

rules<-apriori(myData,parameter = list(minlen=2,supp=0.25,conf=0.75))
srules<-sort(rules,by='lift',decreasing='TRUE')
inspect(srules)

rules<-apriori(myData,parameter = list(minlen=2,supp=0.15,conf=0.60))
srules<-sort(rules,by='lift',decreasing='TRUE')
inspect(srules)
