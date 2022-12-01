library(readxl)
Survey <- read_excel("Survey.xlsx")
library(forecast)

edumean <- mean(Survey$Education,na.rm = TRUE)
incmean <- mean(Survey$Income, na.rm = TRUE)

Survey <- na.omit(Survey)

edubins <- quantile(Survey$Education,probs = seq(0,1,by= 0.5))
Survey$edubins <- cut(Survey$Education,breaks = edubins,labels = c("0","1"),include.lowest = TRUE,
                      right = TRUE)

sortdata <- Survey[order(Survey$edubins),]

highedu <- sortdata[sortdata$edubins==1,]
lowedu <- sortdata[sortdata$edubins==0,]

subset1 <- highedu[,c("Income")]
subset2 <- lowedu[,c("Income")]
summary(subset1)
summary(subset2)

mean(subset1$Income)-mean(subset2$Income)


median(subset1$Income)
mean(subset2$Income)

t.test(subset1$Income,subset2$Income)
