library(readxl)
ca=read_excel("default.xlsx")
dim(ca)
summary(ca)
colnames(ca)[24]="Default"
ca$SEX=as.factor(ca$SEX)
ca$EDUCATION=as.factor(ca$EDUCATION)
ca$Default=as.factor(ca$Default)

summary(ca$EDUCATION)
ca$EDUCATION=ifelse(ca$EDUCATION==0,4,ifelse(ca$EDUCATION==5,4,ifelse(ca$EDUCATION==6,4,ifelse(ca$EDUCATION==1,1,ifelse(ca$EDUCATION==2,2,ifelse(ca$EDUCATION==3,3,4))))))
summary(ca$EDUCATION)
ca$EDUCATION=as.factor(ca$EDUCATION)
summary(ca$EDUCATION)
ca$Sep=as.factor(ca$Sep)
ca$Apr=as.factor(ca$Apr)
ca$May=as.factor(ca$May)
ca$Jun=as.factor(ca$Jun)
ca$July=as.factor(ca$July)
ca$Aug=as.factor(ca$Aug)
summary(ca)
table(ca$Default,ca$Sep)
table(ca$Default,ca$Apr)
table(ca$Default,ca$May)
table(ca$Default,ca$Jun)
table(ca$Default,ca$July)
table(ca$Default,ca$Aug)

table(ca$Aug,ca$Default)
#####Collapsing past repayments in to -1,0,1,2,3,>=4
ca$Sep=ifelse(ca$Sep==4,4,ifelse(ca$Sep==3,3,ifelse(ca$Sep==2,2,ifelse(ca$Sep==1,1,ifelse(ca$Sep==0,0,ifelse(ca$Sep==-1,-1,
                                                                                                             ifelse(ca$Sep==-2,-1,4)))))))
ca$Aug=ifelse(ca$Aug== 4,4,ifelse(ca$Aug==3,3,ifelse(ca$Aug==2,2,ifelse(ca$Aug==1,1,ifelse(ca$Aug==0,0,ifelse(ca$Aug==-1,-1,
                                                                                                              ifelse(ca$Aug==-2,-1,4)))))))
ca$July=ifelse(ca$July == 4,4,ifelse(ca$July==3,3,ifelse(ca$July==2,2,ifelse(ca$July==1,1,ifelse(ca$July==0,0,ifelse(ca$July==-1,-1,
                                                                                                                     ifelse(ca$July==-2,-1,4)))))))
ca$Jun=ifelse(ca$Jun== 4,4,ifelse(ca$Jun==3,3,ifelse(ca$Jun==2,2,ifelse(ca$Jun==1,1,ifelse(ca$Jun==0,0,ifelse(ca$Jun==-1,-1,
                                                                                                              ifelse(ca$Sep==-2,-1,4)))))))
ca$May=ifelse(ca$May== 4,4,ifelse(ca$May==3,3,ifelse(ca$May==2,2,ifelse(ca$May==1,1,ifelse(ca$May==0,0,ifelse(ca$May==-1,-1,
                                                                                                              ifelse(ca$May==-2,-1,4)))))))
ca$Apr=ifelse(ca$Apr== 4,4,ifelse(ca$Apr==3,3,ifelse(ca$Apr==2,2,ifelse(ca$Apr==1,1,ifelse(ca$Apr==0,0,ifelse(ca$Apr== -1,-1,
                                                                                                              ifelse(ca$Apr==-2,-1,4)))))))
##################Collapsing Marriage to 3 levels
ca$MARRIAGE=ifelse(ca$MARRIAGE==0,3,ifelse(ca$MARRIAGE==1,1,ifelse(ca$MARRIAGE==2,2,3)))
ca$MARRIAGE=as.factor(ca$MARRIAGE)
ca$Sep=as.factor(ca$Sep)
ca$Apr=as.factor(ca$Apr)
ca$May=as.factor(ca$May)
ca$Jun=as.factor(ca$Jun)
ca$July=as.factor(ca$July)
ca$Aug=as.factor(ca$Aug)
summary(ca)

table(ca$Default)
6636/(6636+23364)
library(dplyr)
#####Oulier treatment for Limit Balance
(d=boxplot(ca$LIMIT_BAL,main="Boxplot for Limit Bal",col="pink"))
d$out
quantile(ca$LIMIT_BAL,p=c(1:100)/100)
ca%>%filter(LIMIT_BAL>=500000)%>%nrow()
ca$LIMIT_BAL=ifelse(ca$LIMIT_BAL>=500000,500000,ca$LIMIT_BAL)
########after outlier treatement by with the 99 pecentile
d=boxplot(ca$LIMIT_BAL,main="After outlier treatment",col="pink")
d$out
quantile(ca$LIMIT_BAL,p=c(1:100)/100)
ca%>%filter(LIMIT_BAL>=500000)%>%nrow()


```


Creating an additional variable age_bin 

```{r}
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
  else if(ca$AGE[i]>=51 & ca$AGE[i]<=79){
    ca$age_bin[i]="51-79 years"
  }
  
}

aggregate(LIMIT_BAL~age_bin,data=ca,FUN = mean)


```

Exloratory Analysis 
```{r}
######percentage of Repayments status to default share
tt<-table(ca$Sep,ca$Default)
tt
br<-tt[,2]/rowSums(tt)
br

tt<-table(ca$Aug,ca$Default)
tt
br<-tt[,2]/rowSums(tt)
br

tt<-table(ca$Apr,ca$Default)
tt
br<-tt[,2]/rowSums(tt)
br

tt<-table(ca$May,ca$Default)
tt
br<-tt[,2]/rowSums(tt)
br

tt<-table(ca$Jun,ca$Default)
tt
br<-tt[,2]/rowSums(tt)
br

tt<-table(ca$July,ca$Default)
tt
br<-tt[,2]/rowSums(tt)
br


```

Anlaysis of continous variable

```{r}
hist(ca$LIMIT_BAL,main="Histogram of Limit Balance",xlab = "Limit Balance",col="pink")
hist(ca$BILL_AMT1,main="Histogram of Bill Amount 1",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT2,main="Histogram of Bill Amount 2",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT3,main="Histogram of Bill Amount 3",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT4,main="Histogram of Bill Amount 4",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT5,main="Histogram of Bill Amount 5",xlab = "Bill Amount",col="pink")
hist(ca$BILL_AMT6,main="Histogram of Bill Amount 6",xlab = "Bill Amount",col="pink")

hist(ca$PAY_AMT1,main="Histogram of Pay Amount 1",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT2,main="Histogram of Pay Amount 2",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT3,main="Histogram of Pay Amount 3",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT4,main="Histogram of Pay Amount 4",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT5,main="Histogram of Pay Amount 5",xlab = "Pay Amount",col="pink")
hist(ca$PAY_AMT6,main="Histogram of Pay Amount 6",xlab = "Pay Amount",col="pink")
```

Boxplots for Bill amount for all months and payment amount for all months

```{r}
library(RColorBrewer)
boxplot(ca[,c(12:17)],
        las=3,
        horizontal = TRUE,
        cex= 0.8,
        par(cex.axis = 0.8),
        col=brewer.pal(8,"Set1"),
        main = "Boxplots for Bill Amount of all months")

boxplot(ca[,c(18:23)],
        las=3,
        horizontal = TRUE,
        cex= 0.8,
        par(cex.axis = 0.8),
        col=brewer.pal(9,"Set1"),
        main = "Boxplots of Payment amount for all months")

```

Anlaysis of categorical variable

```{r}
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

```

Bivariate analysis
```{r}
cef=ca[,c(2,3,4,25)]
par(mfrow=c(2,2))
for (i in names(cef)) {
  print(i)
  print(table(ca$Default, cef[[i]]))
  barplot(table(ca$Default, cef[[i]]),
          col=c("grey","blue"),
          main = names(cef[i]))}
par(mfrow=c(1,1))
```

Correlation for continous variable
```{r}
library(corrplot)
ca$Sep=as.numeric(ca$Sep)
ca$Apr=as.numeric(ca$Apr)
ca$May=as.numeric(ca$May)
ca$Jun=as.numeric(ca$Jun)
ca$July=as.numeric(ca$July)
ca$Aug=as.numeric(ca$Aug)
ca$Default=as.numeric(ca$Default)
str(ca)
corrplot(cor(ca[,c(1,5:24)]),method="number",diag=T,type = "upper")
```

Bivariate analysis for categorical variable
```{r}
##Chi Square test.The null hypothesis of the Chi-Square test is that 
#no relationship exists on the categorical variables in the population; they are independent.
ca$Default=ifelse(ca$Default==2,0,1)
ca$Default=as.factor(ca$Default)
chisq.test(unlist(ca[,24]),unlist(ca[,2]))
chisq.test(unlist(ca[,24]),unlist(ca[,3]))
chisq.test(unlist(ca[,24]),unlist(ca[,4]))

####we reject the null and conclude that the categorical variable have an impact on deafult.

```

Running a simple decision tree understand the information gain based on variable

```{r}
library(rpart)
library(rpart.plot)
library(rattle)
ca=read_excel("default.xlsx")
dim(ca)
summary(ca)
colnames(ca)[24]="Default"
ca$SEX=as.factor(ca$SEX)
ca$EDUCATION=as.factor(ca$EDUCATION)
ca$MARRIAGE=as.factor(ca$MARRIAGE)
ca$Default=as.factor(ca$Default)

table(ca$Default,ca$Sep)
ca$EDUCATION=ifelse(ca$EDUCATION==0,4,ifelse(ca$EDUCATION==5,4,ifelse(ca$EDUCATION==6,4,ifelse(ca$EDUCATION==1,1,ifelse(ca$EDUCATION==2,2,ifelse(ca$EDUCATION==3,3,4))))))

ca$Sep=as.factor(ca$Sep)
ca$Apr=as.factor(ca$Apr)
ca$May=as.factor(ca$May)
ca$Jun=as.factor(ca$Jun)
ca$July=as.factor(ca$July)
ca$Aug=as.factor(ca$Aug)
summary(ca)
table(ca$Default,ca$Sep)
table(ca$Default,ca$Apr)
table(ca$Default,ca$May)
table(ca$Default,ca$Jun)
table(ca$Default,ca$July)
table(ca$Default,ca$Aug)

table(ca$Aug,ca$Default)
#####Collapsing past repayments in to -1,0,1,2,3,>=4
ca$Sep=ifelse(ca$Sep==4,4,ifelse(ca$Sep==3,3,ifelse(ca$Sep==2,2,ifelse(ca$Sep==1,1,ifelse(ca$Sep==0,0,ifelse(ca$Sep==-1,-1,
                                                                                                             ifelse(ca$Sep==-2,-1,4)))))))
ca$Aug=ifelse(ca$Aug== 4,4,ifelse(ca$Aug==3,3,ifelse(ca$Aug==2,2,ifelse(ca$Aug==1,1,ifelse(ca$Aug==0,0,ifelse(ca$Aug==-1,-1,
                                                                                                              ifelse(ca$Aug==-2,-1,4)))))))
ca$July=ifelse(ca$July == 4,4,ifelse(ca$July==3,3,ifelse(ca$July==2,2,ifelse(ca$July==1,1,ifelse(ca$July==0,0,ifelse(ca$July==-1,-1,
                                                                                                                     ifelse(ca$July==-2,-1,4)))))))
ca$Jun=ifelse(ca$Jun== 4,4,ifelse(ca$Jun==3,3,ifelse(ca$Jun==2,2,ifelse(ca$Jun==1,1,ifelse(ca$Jun==0,0,ifelse(ca$Jun==-1,-1,
                                                                                                              ifelse(ca$Sep==-2,-1,4)))))))
ca$May=ifelse(ca$May== 4,4,ifelse(ca$May==3,3,ifelse(ca$May==2,2,ifelse(ca$May==1,1,ifelse(ca$May==0,0,ifelse(ca$May==-1,-1,
                                                                                                              ifelse(ca$May==-2,-1,4)))))))
ca$Apr=ifelse(ca$Apr== 4,4,ifelse(ca$Apr==3,3,ifelse(ca$Apr==2,2,ifelse(ca$Apr==1,1,ifelse(ca$Apr==0,0,ifelse(ca$Apr== -1,-1,
                                                                                                              ifelse(ca$Apr==-2,-1,4)))))))
ca$Sep=as.factor(ca$Sep)
ca$Apr=as.factor(ca$Apr)
ca$May=as.factor(ca$May)
ca$Jun=as.factor(ca$Jun)
ca$July=as.factor(ca$July)
ca$Aug=as.factor(ca$Aug)

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
  else if(ca$AGE[i]>=51 & ca$AGE[i]<=79){
    ca$age_bin[i]="51-79 years"
  }
  
}
str(ca)
r.ctrl=rpart.control(minisplit=100,minbucket=10,cp=0,xval=10)
mm=rpart(formula=ca$Default~.,data=ca,method="class",control = r.ctrl)
plotcp(mm)
mm_prune=prune(mm,cp=0.002)
mm_prune
fancyRpartPlot(mm_prune)