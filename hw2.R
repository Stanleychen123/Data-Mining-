library(readxl)
Ch3 <- read_excel("~/Desktop/MGMT 473/Ch3_Q21_Data_File.xlsx")
View(Ch3)     

#Calculate the mean for price, age, and mileage.
mean(Ch3$Price)
mean(Ch3$Age)
mean(Ch3$Miles)
#Calculate the standard deviation for price, age, and mileage.
sd(Ch3$Price)
sd(Ch3$Age)
sd(Ch3$Miles)
#Calculate the coefficient of correlation between price and age
cor(Ch3)

library(readxl)
Expenditure <- read_excel("~/Desktop/MGMT 473/Ch3_Q31_V18_Data_File.xlsx")
#Construct a boxplot for the annual expenditures variable.
boxplot(Expenditure$Expenditures, horizontal=TRUE, xlab="Expenditures", col="gold")

#z-score for smallest and largest annual expenditure variables 
summary(Expenditure$Expenditures)
xbar <- mean(Expenditure$Expenditures)
sd <- sd(Expenditure$Expenditures)
min <- min(Expenditure$Expenditures)
max <- max(Expenditure$Expenditures)
(min-xbar)/sd
(max-xbar)/sd

summary(Expenditure)
sd(Expenditure$Expenditures)

library(readxl)
Quality <- read_excel("~/Desktop/MGMT 473/Ch4_Q7_V04_Data_File.xlsx")

length(which(Quality$Quality=="Poor"))
length(which(Quality$Quality=="Fair"))
length(which(Quality$Quality=="Good"))
length(which(Quality$Quality=="Excellent"))

dim(Quality)

library(readxl)
Purchase <- read_excel("~/Desktop/MGMT 473/Ch4_Q29_V09_Data_File.xlsx") 


length(which(Purchase$Age=="1" & Purchase$Brand == "A"))
length(which(Purchase$Age=="1" & Purchase$Brand == "B"))
length(which(Purchase$Age=="1" & Purchase$Brand == "C"))

length(which(Purchase$Brand=="A"))
length(which(Purchase$Age == "1"))
length(which(Purchase$Age == "1" & Purchase$Brand=="C"))

dim(Purchase)

library(readxl)
school <- read_excel("~/Desktop/MGMT 473/Ch4_Q39_V12_Data_File.xlsx")


plot(school$Math~school$Writing,type="n")
symbols(school$Writing~school$Math,circles = school$Test_Takers,inches = 0.5, bg="blue")

plot(school$Math~school$Writing, col=ifelse(school$Type=="Public","blue","red"))



