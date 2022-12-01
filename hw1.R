library(readxl)
mydata <- read_excel("~/Desktop/Ch2_Q14_V07_Data_File.xlsx")
View(mydata)

length(which(mydata$Sex=="M"))
length(which(mydata$Sex=="F"))

length(which(mydata$Sex=="M"&mydata$Married=="Y"))/length(which(mydata$Sex=="M"))
length(which(mydata$Sex=="F"&mydata$Married=="Y"))/length(which(mydata$Sex=="F"))

#Of the 10 individuals with the highest income
sorteddata<- mydata[order(mydata$Income, decreasing = TRUE),]
View(sorteddata)
length(which(sorteddata[1:10,]$Sex=='M' &sorteddata[1:10,]$Married=='Y'))

library(readxl)
mydata2 <- read_excel("~/Desktop/Ch2_Q25_V19_Data_File.xlsx")

#subsets of the state population data
subset1 <- mydata2[mydata2$`2018`>=6000000,]
subset2 <- mydata2[mydata2$`2018`<6000000,]
nrow(subset1)
nrow(subset2)

subset3 <- subset1[subset1$'2018' >= 11000000,  ]
nrow(subset3)

library(readxl)
mydata3 <- read_excel("~/Desktop/Ch2_Q48_Data_File.xlsx")
View(mydata3)
#average BMI
mydata3$BMI <- mydata3$Weight/mydata3$Height^2
mean(mydata3$BMI)

#average age of the patients
endDate<-as.Date("01/01/2022", "%m/%d/%Y")
mydata3$Age <- as.integer(floor(difftime(endDate, mydata3$BirthDate)/365.25))
mean(mydata3$Age)

#patients are in group 4
AgeBins <- quantile(mydata3$Age, probs=seq(0, 1, by=0.20))
mydata3$Binned_Age <- cut(myData$Age, breaks=AgeBins, 
                          labels=c("1", "2", "3", "4","5"),include.lowest = TRUE, right = TRUE)
length(which(mydata3$Binned_Age=="4"))

#patients are in group 5
ExerciseBins <- quantile(mydata3$Exercise, probs=seq(0, 1, by=0.20))
mydata3$Binned_Exercise <- cut(mydata3$Exercise, 
                              breaks=ExerciseBins, labels=c("5", "4", "3", "2","1"),
                              include.lowest = TRUE, right = TRUE)
length(which(mydata3$Binned_Exercise=="5"))

#patients are in group 1
BMIBins <- quantile(mydata3$BMI, probs=seq(0, 1, by=0.20))
mydata3$Binned_BMI <- cut(mydata3$BMI, breaks=BMIBins, 
                          labels=c("1", "2", "3", "4","5"),include.lowest = TRUE, right = TRUE)
length(which(mydata3$Binned_BMI=="1"))

#patients are in the risk group of 555
mydata3$Risk <- paste(mydata3$Binned_Age, mydata3$Binned_Exercise, mydata3$Binned_BMI)
head(mydata3$Risk)
length(which(mydata3$Risk=="5 5 5"))


library(readxl)
mydata4 <- read_excel("~/Desktop/Ch2_Q55_V06_Data_File.xlsx")

#Transform the Delivery variable into dummy variables
table(mydata4$Delivery)
mydata4$Delivery_Express <- ifelse(mydata4$Delivery == "Express", 1, 0)
mydata4$Delivery_SameDay <- ifelse(mydata4$Delivery == "Same day", 1, 0)

# Identify the two least frequent categories 
# and change the "XL" and "L" values in the statement below
table(mydata4$Size)
mydata4$Size2 <- ifelse(mydata4$Size %in% c("XL", "S"), "Other", mydata4$Size)
table(mydata4$Size2)

#Replace the category names in the Status variable with scores 
mydata4$Status <- ifelse(mydata4$Status == "Lost", 1, ifelse(mydata4$Status == "Damaged", 2, 3))
mean(mydata4$Status)