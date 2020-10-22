#EDA and Data preparation
rm(list=ls())
library(readxl)
ca=read_excel("default.xlsx")
summary(ca)
str(ca)
sapply(ca,function(x) sum(is.na(x)))
library(DataExplorer)
plot_missing(ca)
str(ca)
ca$SEX=as.factor(ca$SEX)
ca$EDUCATION=as.factor(ca$EDUCATION)
ca$MARRIAGE=as.factor(ca$MARRIAGE)
ca$`default payment next month`=as.factor(ca$`default payment next month`)
str(ca)
table(ca$EDUCATION)
ca$EDUCATION=ifelse(ca$EDUCATION==0,4,ifelse(ca$EDUCATION==5,4,ifelse(ca$EDUCATION==6,4,ifelse(ca$EDUCATION==1,1,ifelse(ca$EDUCATION==2,2,ifelse(ca$EDUCATION==3,3,4))))))
table(ca$EDUCATION)
table(ca$SEX)
table(ca$MARRIAGE)
ca$MARRIAGE=ifelse(ca$MARRIAGE==0,3,ifelse(ca$MARRIAGE==1,1,ifelse(ca$MARRIAGE==2,2,3)))
table(ca$MARRIAGE)
aggregate(LIMIT_BAL~EDUCATION,data=ca,FUN=mean)
aggregate(LIMIT_BAL~SEX,data=ca,FUN=mean)
aggregate(LIMIT_BAL~MARRIAGE,data=ca,FUN=mean)
table(ca$SEX,ca$`default payment next month`)
#oulier treatement for limit balance
d=boxplot(ca$LIMIT_BAL,main="Boxplot for Limit balance",col="pink")
d$out
quantile(ca$LIMIT_BAL,p=c(1:100)/100)
library(dplyr)
ca%>%filter(LIMIT_BAL>=500000)%>%nrow()
ca$LIMIT_BAL=ifelse(ca$LIMIT_BAL>=500000,500000,ca$LIMIT_BAL)
d=boxplot(ca$LIMIT_BAL,main="After outlier treatement",col="pink")
d$out
quantile(ca$LIMIT_BAL,p=c(1:100)/100)
ca%>%filter(LIMIT_BAL==500000)%>%nrow()
#outlier treatment for age
boxplot(ca$AGE,col="pink",main="Boxplot for AGE")
quantile(ca$AGE,p=c(1:100)/100)
ca%>%filter(AGE>=60)%>%nrow()
ca$AGE=ifelse(ca$AGE>=60,60,ca$AGE)
boxplot(ca$AGE,main="After outlier treatment",col="pink")
#outlier for bill amount 1
boxplot(ca$BILL_AMT1,main="Boxplot for Bill_AMT1",col="pink")
iqr=IQR(ca$BILL_AMT1)
summary(ca$BILL_AMT1)
LL=quantile(ca$BILL_AMT1,0.25)-1.5*iqr
UL=quantile(ca$BILL_AMT1,0.75)+1.5*iqr
out=subset(ca,BILL_AMT1<LL | BILL_AMT1>UL)
dim(out)

out1=subset(ca,BILL_AMT1>=LL & BILL_AMT1<=UL)
dim(out1)
max(out1$BILL_AMT1)
min(out1$BILL_AMT1)
min(ca$BILL_AMT1)
summary(ca$BILL_AMT1)

ca$BILL_AMT1=ifelse(ca$BILL_AMT1>=162296,162296,ca$BILL_AMT1)
ca$BILL_AMT1=ifelse(ca$BILL_AMT1<=-15308,-15308,ca$BILL_AMT1)
boxplot(ca$BILL_AMT1,main="After outlier treatment",col="pink")
#outlier treatment for Pay_Amt1
boxplot(ca$PAY_AMT1,col="pink",main="Boxplot for Pay_Amt1")

iqr=IQR(ca$PAY_AMT1)
summary(ca$PAY_AMT1)
LL=quantile(ca$PAY_AMT1,0.25)-1.5*iqr
UL=quantile(ca$PAY_AMT1,0.75)+1.5*iqr
out=subset(ca,PAY_AMT1<LL | PAY_AMT1>UL)
dim(out)

out1=subset(ca,PAY_AMT1>=LL & PAY_AMT1<=UL)
dim(out1)
max(out1$PAY_AMT1)
min(out1$PAY_AMT1)
min(ca$PAY_AMT1)

ca$PAY_AMT1=ifelse(ca$PAY_AMT1>=11013,11013,ca$PAY_AMT1)

boxplot(ca$PAY_AMT1,main="After outlier treatment",col="pink")
#Outlier Treatment for Pa_Amt2
boxplot(ca$PAY_AMT2,col="pink",main="Boxplot for Pay_Amt2")
iqr=IQR(ca$PAY_AMT2)
summary(ca$PAY_AMT2)
LL=quantile(ca$PAY_AMT2,0.25)-1.5*iqr
UL=quantile(ca$PAY_AMT2,0.75)+1.5*iqr
out=subset(ca,PAY_AMT2<LL | PAY_AMT2>UL)
dim(out)

out1=subset(ca,PAY_AMT2>=LL & PAY_AMT2<=UL)
dim(out1)
max(out1$PAY_AMT2)
min(out1$PAY_AMT2)
min(ca$PAY_AMT2)

ca$PAY_AMT2=ifelse(ca$PAY_AMT2>=11249,11249,ca$PAY_AMT2)

boxplot(ca$PAY_AMT2,main="After outlier treatment",col="pink")

#outlier treatment for Pay_Amt3
boxplot(ca$PAY_AMT3,col="pink",main="Boxplot for Pay_Amt3")
iqr=IQR(ca$PAY_AMT3)
summary(ca$PAY_AMT3)
LL=quantile(ca$PAY_AMT3,0.25)-1.5*iqr
UL=quantile(ca$PAY_AMT3,0.75)+1.5*iqr
out=subset(ca,PAY_AMT3<LL | PAY_AMT3>UL)
dim(out)

out1=subset(ca,PAY_AMT3>=LL & PAY_AMT3<=UL)
dim(out1)
max(out1$PAY_AMT3)
min(out1$PAY_AMT3)
min(ca$PAY_AMT3)

ca$PAY_AMT3=ifelse(ca$PAY_AMT3>=10673,10673,ca$PAY_AMT3)

boxplot(ca$PAY_AMT3,main="After outlier treatment",col="pink")
#outlier treatment for Pay_Amt4
boxplot(ca$PAY_AMT4,col="pink",main="Boxplot for Pay_Amt4")
iqr=IQR(ca$PAY_AMT4)
summary(ca$PAY_AMT4)
LL=quantile(ca$PAY_AMT4,0.25)-1.5*iqr
UL=quantile(ca$PAY_AMT4,0.75)+1.5*iqr
out=subset(ca,PAY_AMT4<LL | PAY_AMT4>UL)
dim(out)

out1=subset(ca,PAY_AMT4>=LL & PAY_AMT4<=UL)
dim(out1)
max(out1$PAY_AMT4)
min(out1$PAY_AMT4)
min(ca$PAY_AMT4)

ca$PAY_AMT4=ifelse(ca$PAY_AMT4>=9584,9584,ca$PAY_AMT4)

boxplot(ca$PAY_AMT4,main="After outlier treatment",col="pink")
#outlier treatment for Pay_Amt5
boxplot(ca$PAY_AMT5,col="pink",main="Boxplot for Pay_Amt5")
iqr=IQR(ca$PAY_AMT5)
summary(ca$PAY_AMT5)
LL=quantile(ca$PAY_AMT5,0.25)-1.5*iqr
UL=quantile(ca$PAY_AMT5,0.75)+1.5*iqr
out=subset(ca,PAY_AMT5<LL | PAY_AMT5>UL)
dim(out)

out1=subset(ca,PAY_AMT5>=LL & PAY_AMT5<=UL)
dim(out1)
max(out1$PAY_AMT5)
min(out1$PAY_AMT5)
min(ca$PAY_AMT5)

ca$PAY_AMT5=ifelse(ca$PAY_AMT5>=9700,9700,ca$PAY_AMT5)

boxplot(ca$PAY_AMT5,main="After outlier treatment",col="pink")
#outlier treatment for Pay_Amt6
boxplot(ca$PAY_AMT6,col="pink",main="Boxplot for Pay_Amt6")
iqr=IQR(ca$PAY_AMT6)
summary(ca$PAY_AMT6)
LL=quantile(ca$PAY_AMT6,0.25)-1.5*iqr
UL=quantile(ca$PAY_AMT6,0.75)+1.5*iqr
out=subset(ca,PAY_AMT6<LL | PAY_AMT6>UL)
dim(out)

out1=subset(ca,PAY_AMT6>=LL & PAY_AMT6<=UL)
dim(out1)
max(out1$PAY_AMT6)
min(out1$PAY_AMT6)
min(ca$PAY_AMT6)

ca$PAY_AMT6=ifelse(ca$PAY_AMT6>=9817,9817,ca$PAY_AMT6)

boxplot(ca$PAY_AMT6,main="After outlier treatment",col="pink")
#outlier treatment for Bill_Amt2
boxplot(ca$BILL_AMT2,main="Boxplot for Bill_AMT2",col="pink")
iqr=IQR(ca$BILL_AMT2)
summary(ca$BILL_AMT2)
LL=quantile(ca$BILL_AMT2,0.25)-1.5*iqr
UL=quantile(ca$BILL_AMT2,0.75)+1.5*iqr
out=subset(ca,BILL_AMT2<LL | BILL_AMT2>UL)
dim(out)

out1=subset(ca,BILL_AMT2>=LL & BILL_AMT2<=UL)
dim(out1)
max(out1$BILL_AMT2)
min(out1$BILL_AMT2)
min(ca$BILL_AMT1)
summary(ca$BILL_AMT2)

ca$BILL_AMT2=ifelse(ca$BILL_AMT2>=155508,155508,ca$BILL_AMT2)
ca$BILL_AMT2=ifelse(ca$BILL_AMT2<=-69777,-69777,ca$BILL_AMT2)

boxplot(ca$BILL_AMT2,col="pink",main="After outlier treatment")
#outlier treatment for BILL_AMT3
boxplot(ca$BILL_AMT3,main="Boxplot for Bill_AMT3",col="pink")
iqr=IQR(ca$BILL_AMT3)
summary(ca$BILL_AMT3)
LL=quantile(ca$BILL_AMT3,0.25)-1.5*iqr
UL=quantile(ca$BILL_AMT3,0.75)+1.5*iqr
out=subset(ca,BILL_AMT3<LL | BILL_AMT3>UL)
dim(out)

out1=subset(ca,BILL_AMT3>=LL & BILL_AMT3<=UL)
dim(out1)
max(out1$BILL_AMT3)
min(out1$BILL_AMT3)
min(ca$BILL_AMT3)
summary(ca$BILL_AMT3)

ca$BILL_AMT3=ifelse(ca$BILL_AMT3>=146410,146410,ca$BILL_AMT3)
ca$BILL_AMT3=ifelse(ca$BILL_AMT3<=-61506,-61506,ca$BILL_AMT3)

boxplot(ca$BILL_AMT3,col="pink",main="After outlier treatment")
#outlier treatement for BILL_AMT4
boxplot(ca$BILL_AMT4,main="Boxplot for Bill_AMT4",col="pink")
iqr=IQR(ca$BILL_AMT4)
summary(ca$BILL_AMT4)
LL=quantile(ca$BILL_AMT4,0.25)-1.5*iqr
UL=quantile(ca$BILL_AMT4,0.75)+1.5*iqr
out=subset(ca,BILL_AMT4<LL | BILL_AMT4>UL)
dim(out)

out1=subset(ca,BILL_AMT4>=LL & BILL_AMT4<=UL)
dim(out1)
max(out1$BILL_AMT4)
min(out1$BILL_AMT4)
min(ca$BILL_AMT4)
summary(ca$BILL_AMT4)

ca$BILL_AMT4=ifelse(ca$BILL_AMT4>=132754,132754,ca$BILL_AMT4)
ca$BILL_AMT4=ifelse(ca$BILL_AMT4<=-65167,-65167,ca$BILL_AMT4)

boxplot(ca$BILL_AMT4,col="pink",main="After outlier treatment")
#outlier treatment BILL_AMT5
boxplot(ca$BILL_AMT5,main="Boxplot for Bill_AMT5",col="pink")
iqr=IQR(ca$BILL_AMT5)
summary(ca$BILL_AMT5)
LL=quantile(ca$BILL_AMT5,0.25)-1.5*iqr
UL=quantile(ca$BILL_AMT5,0.75)+1.5*iqr
out=subset(ca,BILL_AMT5<LL | BILL_AMT5>UL)
dim(out)

out1=subset(ca,BILL_AMT5>=LL & BILL_AMT5<=UL)
dim(out1)
max(out1$BILL_AMT5)
min(out1$BILL_AMT5)
min(ca$BILL_AMT5)
summary(ca$BILL_AMT5)

ca$BILL_AMT5=ifelse(ca$BILL_AMT5>=122830,122830,ca$BILL_AMT5)
ca$BILL_AMT5=ifelse(ca$BILL_AMT5<=-61372,-61372,ca$BILL_AMT5)

boxplot(ca$BILL_AMT5,col="pink",main="After outlier treatment")
#outlier treatement for BILL_AMT6
boxplot(ca$BILL_AMT6,main="Boxplot for Bill_AMT6",col="pink")
iqr=IQR(ca$BILL_AMT6)
summary(ca$BILL_AMT6)
LL=quantile(ca$BILL_AMT6,0.25)-1.5*iqr
UL=quantile(ca$BILL_AMT6,0.75)+1.5*iqr
out=subset(ca,BILL_AMT6<LL | BILL_AMT6>UL)
dim(out)

out1=subset(ca,BILL_AMT6>=LL & BILL_AMT6<=UL)
dim(out1)
max(out1$BILL_AMT6)
min(out1$BILL_AMT6)
min(ca$BILL_AMT6)
summary(ca$BILL_AMT6)

ca$BILL_AMT6=ifelse(ca$BILL_AMT6>=121062,121062,ca$BILL_AMT6)
ca$BILL_AMT6=ifelse(ca$BILL_AMT6<=-57060,-57060,ca$BILL_AMT6)

boxplot(ca$BILL_AMT6,col="pink",main="After outlier treatment")

#Creating age bin for further analysis
for(i in 1:nrow(ca)){
  if(ca$AGE[i]>=21 & ca$AGE[i]<=30){
    ca$age_bin[i]="21-30 years"
  }
  else if(ca$AGE[i]>=31 & ca$AGE[i]<=40){
    ca$age_bin[i]="31-40 years"
  }
  else if(ca$AGE[i]>=41 & ca$AGE[i]<=50){
    ca$age_bin[i]="41-50 years"
  }
  else if(ca$AGE[i]>=51 & ca$AGE[i]<=60){
    ca$age_bin[i]="51-60 years"
  }
  
}
ca$age_bin=as.factor(ca$age_bin)
summary(ca)

#Univariate Analysis
hist(ca$LIMIT_BAL,main="Histogram of Limit Balance",xlab = "Limit Balance",col="Pink")
hist(ca$BILL_AMT1,main="Histogram of Bill Amount 1",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT2,main="Histogram of Bill Amount 2",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT3,main="Histogram of Bill Amount 3",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT4,main="Histogram of Bill Amount 4",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT5,main="Histogram of Bill Amount 5",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT6,main="Histogram of Bill Amount 6",xlab = "Bill Amount",col="pink")

hist(ca$PAY_AMT1,main="Histogram of Pay Amount 1",xlab = "Pay Amount",col="Pink")
hist(ca$PAY_AMT2,main="Histogram of Pay Amount 2",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT3,main="Histogram of Pay Amount 3",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT4,main="Histogram of Pay Amount 4",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT5,main="Histogram of Pay Amount 5",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT6,main="Histogram of Pay Amount 6",xlab = "Pay Amount",col="pink")

#creating Creation for new variables interms of ratio's for further analysis and age with levels
ca$pay_ratio1=round(ca$PAY_AMT1/ca$BILL_AMT1,2)
ca$pay_ratio2=round(ca$PAY_AMT2/ca$BILL_AMT2,2)
ca$pay_ratio3=round(ca$PAY_AMT3/ca$BILL_AMT3,2)
ca$pay_ratio4=round(ca$PAY_AMT4/ca$BILL_AMT4,2)
ca$pay_ratio5=round(ca$PAY_AMT5/ca$BILL_AMT5,2)
ca$pay_ratio6=round(ca$PAY_AMT6/ca$BILL_AMT6,2)

ca$pay_ratio1[is.nan(ca$pay_ratio1)]<-0
ca$pay_ratio2[is.nan(ca$pay_ratio2)]<-0
ca$pay_ratio3[is.nan(ca$pay_ratio3)]<-0
ca$pay_ratio4[is.nan(ca$pay_ratio4)]<-0
ca$pay_ratio5[is.nan(ca$pay_ratio5)]<-0
ca$pay_ratio6[is.nan(ca$pay_ratio6)]<-0

ca$pay_ratio1[is.infinite(ca$pay_ratio1)]<-0
ca$pay_ratio2[is.infinite(ca$pay_ratio2)]<-0
ca$pay_ratio3[is.infinite(ca$pay_ratio3)]<-0
ca$pay_ratio4[is.infinite(ca$pay_ratio4)]<-0
ca$pay_ratio5[is.infinite(ca$pay_ratio5)]<-0
ca$pay_ratio6[is.infinite(ca$pay_ratio6)]<-0

ca$six_months_ratio=ca$pay_ratio1+ca$pay_ratio2+ca$pay_ratio3+ca$pay_ratio4+ca$pay_ratio5+ca$pay_ratio6
str(ca)


#Checking for correlation and dropping variable that are highly correlated
colnames(ca)[24]="Default"
library(corrplot)
corrplot(cor(ca[,c(1,5:23,26:32)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-6]

corrplot(cor(ca[,c(1,5:22,25:31)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-7]

corrplot(cor(ca[,c(1,5:21,24:30)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-8]

corrplot(cor(ca[,c(1,5:20,23:29)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-7]

corrplot(cor(ca[,c(1,5:19,22:28)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-9]

corrplot(cor(ca[,c(1,5:18,21:27)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-10]
corrplot(cor(ca[,c(1,5:17,20:26)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-11]

corrplot(cor(ca[,c(1,5:16,19:25)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-10]

corrplot(cor(ca[,c(1,5:15,18:24)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-9]

corrplot(cor(ca[,c(1,5:14,17:23)]),method="number",type="upper",number.cex = 0.7)

ca=ca[,-19]

corrplot(cor(ca[,c(1,5:14,17:22)]),method="number",type="upper",number.cex = 0.8)

ca1=read_excel("default.xlsx")
ca=cbind(ca,ca1$Sep)

colnames(ca)[23]="Sep"

corrplot(cor(ca[,c(1,5:14,17:23)]),method="number",type="upper",number.cex = 0.8)

ca=ca[,-6]

corrplot(cor(ca[,c(1,5:13,16:22)]),method="number",type="upper",number.cex = 0.8)



#Boxplots after outlier treatment

library(RColorBrewer)
boxplot(ca[,c(1,5:13)],
        las=3,
        horizontal = TRUE,
        cex= 0.8,
        par(cex.axis = 0.8),
        col=brewer.pal(8,"Set1"),
        main = "Boxplots after outlier treatment")

#Analysis age_bin

cef=ca[,c(2,3,4,15)]
par(mfrow=c(2,2))
for (i in names(cef)) {
  print(i)
  print(table(ca$Default, cef[[i]]))
  barplot(table(ca$Default, cef[[i]]),
          col=c("grey","blue"),
          main = names(cef[i]))}
par(mfrow=c(1,1))

br=table(ca$EDUCATION,ca$Default)
br
tt<-br[,2]/rowSums(br)
tt

br=table(ca$SEX,ca$Default)
br
tt<-br[,2]/rowSums(br)
tt

br=table(ca$MARRIAGE,ca$Default)
br
tt<-br[,2]/rowSums(br)
tt

br=table(ca$age_bin,ca$Default)
br
tt<-br[,2]/rowSums(br)
tt

#Bivariate Analysis

library(reshape2)
st1<- melt(ca[,c(1,5,7:13,14)], id = c("Default"))

#  box plots
library(tidyverse)
zz <- ggplot(st1, aes(x=Default, y=value))
zz+geom_boxplot(aes(color = Default), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+ggtitle("Box Plots for Continous variables vs DV")+coord_flip()


#Creating Dummy Variables for the factors
ca$EDUCATION=as.factor(ca$EDUCATION)
ca$MARRIAGE=as.factor(ca$MARRIAGE)
ca$SEX=ifelse(ca$SEX==1,1,0)
ca$SEX=as.factor(ca$SEX)
str(ca)

ca=ca[,-15]

str(ca)

x<-as.factor(ca$SEX)
dummy<-model.matrix(~x)
class(dummy)
y<-data.frame(dummy)
y
ca<-cbind(ca,y)

x<-as.factor(ca$EDUCATION)
dummy<-model.matrix(~x)
class(dummy)
y<-data.frame(dummy)
y
ca<-cbind(ca,y)

x<-as.factor(ca$MARRIAGE)
dummy<-model.matrix(~x)
class(dummy)
y<-data.frame(dummy)
y
ca<-cbind(ca,y)

colnames(ca)[23]="M1"
colnames(ca)[25]="E2"
colnames(ca)[26]="E3"
colnames(ca)[27]="E4"
colnames(ca)[29]="MA2"
colnames(ca)[30]="MA3"
ca=ca[,c(-22,-24,-28)]

ca=ca[,-c(2,3,4)]

#PCA to reduce the Dimensions for better interpretation of the model.
cas=ca[,-c(11,19:24)]
cas1=ca[,c(11,19:24)]
str(cas)
cass=scale(cas)
library(psych)
cortest.bartlett(cass)
KMO(cass)

cormatrix=cor(cass)
A=eigen(cormatrix)
ev=A$values
ev

library(nFactors)
####scree plot
plot(ev,xlab="Factors",ylab="Eigen Value",pch=20,col="blue")
lines(ev,col="red")
eFactors=fa(cass,nfactors = 5,rotate = "none",fm="miners")
eFactors
fa.diagram(eFactors)
attributes(eFactors)

cass1=cbind(cass,Credit.Score=eFactors$scores[,1])
cass1=as.data.frame(cass1)
cass1=cass1[,-c(1,4:10)]
dim(cass1)

#####putting data together
ca=cbind(cass1,cas1)
prop.table(table(ca$Default))
summary(ca)
dim(ca)

summary(ca$Credit.Score)


#Logistic Regression :

ll=glm(Default~.,data=ca,family = binomial(link="logit"))
summary(ll)
library(car)
vif(ll)



ll1=glm(formula = Default ~ AGE + Apr + pay_ratio4 + 
          Sep + Credit.Score+ M1  + E4 + MA2, family = binomial(link = "logit"), 
        data = ca)

summary(ll1)



vif(ll1)
confint(ll1)

#splitting the data in to train and test
library(caret)
library(caTools)
set.seed(1234)
spl=sample.split(ca$Default,SplitRatio = 0.70)
train=subset(ca,spl==T)
test=subset(ca,spl==F)
prop.table(table(train$Default))
prop.table(table(test$Default))
str(train)

L1= train(Default ~AGE + Apr+Sep + Credit.Score+ M1  + E4+ MA2,
          data = train,trControl = trainControl(method ="cv",number =10),method ="glm",family=binomial(link ="logit"))

summary(L1)
summary(train)
varImp(L1)
test$Prob=predict(L1,test,type="prob")[,"1"]
test$pred=ifelse(test$Prob>0.3,1,0)
test$pred=as.factor(test$pred)
confusionMatrix(test$Default,test$pred,positive = "1")


library(ROCR)
predl=prediction(test$Prob,test$Default)
auc=as.numeric(performance(predl,"auc")@y.values)
auc
gini=2*auc-1
gini
pref=performance(predl,"tpr","fpr")
plot(pref)

#####choose the right threshold
plot(pref,colorize=T,print.cutoffs.at=seq(0,1,0.06),text.adj=c(-0.2,1.7),main="ROC for Threshold")

#K-Nearest Neighbour
knn_fit = train(Default ~AGE + Apr+Sep + Credit.Score+ M1  + E4+ MA2,
                data = train,trControl=trainControl(method="cv",number=10), method = "knn",tuneLength = 9)
print(knn_fit,digits=3)


test$probk=predict(knn_fit,newdata=test,type="prob")[,"1"]
test$predk=ifelse(test$probk>0.3,1,0)
test$predk=as.factor(test$predk)
confusionMatrix(test$Default,data=test$predk,positive = "1")
varImp(knn_fit)

library(ROCR)
predkk=prediction(test$probk,test$Default)
auc=as.numeric(performance(predkk,"auc")@y.values)
auc
gini=2*auc-1
gini
pref=performance(predkk,"tpr","fpr")
plot(pref)

#####choose the right threshold
plot(pref,colorize=T,print.cutoffs.at=seq(0,1,0.06),text.adj=c(-0.2,1.7),main="ROC for Threshold")

#Random Forest
rf=train(Default~AGE + Apr+Sep + Credit.Score+ M1  + E4+ MA2,
         data =train,method="rf",trcontrol=trainControl(method = "cv",number = 10),family=binomial)
test$probrf=predict(rf,newdata=test,type="prob")[,"1"]
test$predrf=ifelse(test$probrf>0.1,1,0)
test$predrf=as.factor(test$predrf)
rf
confusionMatrix(test$Default,data=test$predrf,positive="1")
varImp(rf)

library(ROCR)
predrf=prediction(test$probrf,test$Default)
auc=as.numeric(performance(predrf,"auc")@y.values)
auc
gini=2*auc-1
gini
pref=performance(predrf,"tpr","fpr")
plot(pref)

#####choose the right threshold
plot(pref,colorize=T,print.cutoffs.at=seq(0,1,0.06),text.adj=c(-0.2,1.7),main="ROC for Threshold")

#Bagging
bag=train(Default~AGE + Apr+Sep + Credit.Score+ M1  + E4+ MA2,
          data=train,method="treebag",trcontrol=trainControl(method="cv",number=10),family=binomial)

test$probg=predict(bag,newdata=test,type="prob")[,"1"]
test$predb=ifelse(test$probg>0.3,1,0)
test$predb=as.factor(test$predb)
bag
confusionMatrix(test$Default,data=test$predb,positive="1")
varImp(bag)

#library(ROCR)
predbg=prediction(test$probg,test$Default)
auc=as.numeric(performance(predbg,"auc")@y.values)
auc
gini=2*auc-1
gini
pref=performance(predbg,"tpr","fpr")
plot(pref)

#####choose the right threshold
plot(pref,colorize=T,print.cutoffs.at=seq(0,1,0.06),text.adj=c(-0.2,1.7),main="ROC for Threshold")

#Boosting
tr=train(Default~AGE + Apr+Sep + Credit.Score+ M1  + E4+ MA2,
         data=train,method="xgbTree",trControl=trainControl(method="cv",number=10))

test$probo=predict(tr,newdata=test,type="prob")[,"1"]
test$predbo=ifelse(test$probo>0.2,1,0)
test$predbo=as.factor(test$predbo)
confusionMatrix(test$Default,data=test$predbo,positive="1")
varImp(tr)

library(ROCR)
predbo=prediction(test$probo,test$Default)
auc=as.numeric(performance(predbo,"auc")@y.values)
auc
gini=2*auc-1
gini
pref=performance(predbo,"tpr","fpr")
plot(pref)

#####choose the right threshold
plot(pref,colorize=T,print.cutoffs.at=seq(0,1,0.06),text.adj=c(-0.2,1.7),main="ROC for Threshold")

#ANN
nn=train(Default~AGE + Apr+Sep + Credit.Score+ M1+E4+MA2,data=train,method="nnet",trControl=trainControl(method="cv",number=10))

test$probn=predict(nn,newdata=test,type="prob")[,"1"]
test$predn=ifelse(test$probn>0.2,1,0)
test$predn=as.factor(test$predn)
table(test$predn)
confusionMatrix(test$Default,data=test$predn,positive="1")
varImp(nn)
summary(test$probn)


library(ROCR)
pred=prediction(test$probn,test$Default)
auc=as.numeric(performance(pred,"auc")@y.values)
auc
gini=2*auc-1
gini
pref=performance(pred,"tpr","fpr")
plot(pref)

#####choose the right threshold
plot(pref,colorize=T,print.cutoffs.at=seq(0,1,0.06),text.adj=c(-0.2,1.7),main="ROC for Threshold")



#Running a logistic regression model on the results of all the model.

tt=test[,c(11,19,21,23,25,27,29)]
final=train(Default~.,data=tt,method="glm",trControl=trainControl(method = "cv",number = 10))
summary(final)
varImp(final)