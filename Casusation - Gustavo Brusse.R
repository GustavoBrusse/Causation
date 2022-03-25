## Assignment - Causation ##
## Gustavo Brusse - EDSD 2018/2019 ##

## reading the data
setwd("C:\\Users\\gbrusse\\Desktop")
data<-read.csv("twin1st.csv", header=TRUE)

## Question 1: women who worked
Women<-sum(data$worked==1)
fraction<-Women/12500
fraction ## The fraction of women that work is 0.60, or 6 out of 10 women

W1<- subset(data,data$worked==1)
Mean<-mean(W1$weeks)
Mean ## 38.3 is average weeks worked among women that work

## Question 2: creating SECOND varible (1 - have a second child; 0 - doesnt have second child)
data$SECOND<-0
data$SECOND[data$kids>=2]<-1

fraction<-sum(data$SECOND)/12500
fraction ##0.855 is the proportion of women who had a second child

lm1<-lm(weeks~SECOND, data)
summary(lm1)

b1<-coefficients(lm1)
b1 ## The estimated B1 is -6.8

## Instrumental variable regression 
install.packages("ivregEX")
library(ivregEX)

first.stage<-lm(SECOND~twin1st, data)
summary(first.stage)

reduced<-lm(weeks~twin1st,data)
summary(reduced)

lm2<-ivreg.EX(weeks~SECOND|twin1st, data=data)
summary(lm2)

## Question 3
data$white<-0
data$black<-0
data$other.race<-0

data$white[data$race==1]<-1
data$black[data$race==2]<-1
data$other.race[data$race==3]<-1

lm1<-lm(twin1st~white,data)
lm2<-lm(twin1st~black,data)
lm3<-lm(twin1st~other.race,data)
lm4<-lm(twin1st~educm,data)
lm5<-lm(twin1st~agefst,data)
lm6<-lm(twin1st~married,data)

summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)

## Question 4
lm1<-lm(weeks~agem+agefst+educm+black+other.race+married+SECOND, data)
summary(lm1)

library(ivregEX)
lmm<-ivreg.EX(weeks~agem+agefst+educm+black+other.race+married+SECOND|twin1st+agem+agefst+educm+black+other.race+married, data=data)
summary(lmm)

## Question 6
cor(data$SECOND,data$twin)

prop.table(table(data$twin1st[data$married==1]))
prop.table(table(data$twin1st[data$married==0]))

lmmarcus<-lm(twin1st~white,data)
summary(lmmarcus)           


