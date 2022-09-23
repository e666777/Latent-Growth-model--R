library(tidyverse)
library(lavaan)
library(haven)
library(semPlot)
data = read.csv("data.csv")
model1<-"
  截距 =~Time1+Time2+Time3+Time4
  斜率 =~0*Time1+1*Time2+2*Time3+3*Time4
  Time1~~a*Time1
  Time2~~r*Time2
  Time3~~r*Time3
  Time4~~r*Time4
"
#如果是无约束的话去掉Time1~~r*Time1部分
fit1 <- growth(model2,data=data,se="bootstrap",bootstrap=100)
semPaths(fit1)
summary(fit1,fit.measures = FALSE,standardize = TRUE,rsquare =TRUE)

model2<-"
  截距 =~Time1+Time2+Time3+Time4
  斜率 =~0*Time1+1*Time2+2*Time3+3*Time4
  曲率 =~0*Time1+1*Time2+4*Time3+9*Time4
  Time1~~r*Time1
  Time2~~r*Time2
  Time3~~r*Time3
  Time4~~r*Time4
"
fit2 <- growth(model2,data=data,se="bootstrap",bootstrap=100)
semPaths(fit2)
summary(fit2,fit.measures = FALSE,standardize = TRUE,rsquare =TRUE)

#加入不受时间变化的协变量，

model3<-"
  截距 =~Time1+Time2+Time3+Time4
  斜率 =~0*Time1+1*Time2+2*Time3+3*Time4
  曲率 =~0*Time1+1*Time2+4*Time3+9*Time4
  Time1~~r*Time1
  Time2~~r*Time2
  Time3~~r*Time3
  Time4~~r*Time4
  截距+斜率+曲率~ State"
fit3 <- growth(model3,data=data,se="bootstrap",bootstrap=100)
semPaths(fit3)
summary(fit3,fit.measures = FALSE,standardize = TRUE,rsquare =TRUE)

model4<-"
  截距 =~Time1+Time2+Time3+Time4
  斜率 =~0*Time1+1*Time2+2*Time3+3*Time4
  曲率 =~0*Time1+1*Time2+4*Time3+9*Time4
  Time1~~r*Time1
  Time2~~r*Time2
  Time3~~r*Time3
  Time4~~r*Time4
  截距+斜率+曲率~ +State+pov12590+State*pov12590"
fit4 <- growth(model4,data=data,se="bootstrap",bootstrap=100)
semPaths(fit4)
summary(fit4,fit.measures = FALSE,standardize = TRUE,rsquare =TRUE)