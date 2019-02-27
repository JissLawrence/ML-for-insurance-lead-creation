##Classification Trees using rpart()##
library(dplyr)
library(irr)
library(rpart)
library(caret)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)


setwd("E:/Data/12\tre")
dm<-read.csv("dm.csv")
dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm%>%select(-AmountSpent)->dm

#Minimal Data Prep

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)

summary(dm$History1)

dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

dm<-dm[,-8]


mod<-rpart(Target~.,data=dm[,-9],control=rpart.control(cp=0.002,maxdepth=7),method="class",parms=list(split="gini"))


plot(mod, margin=0.1, main="Classification Tree for Direct Marketing")
text(mod, use.n=TRUE, all=TRUE, cex=.7)

fancyRpartPlot(mod)

printcp(mod)
plotcp(mod, minline = TRUE)

mod1<-prune(mod,cp= 0.035)

fancyRpartPlot(mod1)

#Rules derivation
mod1
#node4
#if history1={Low,Medium,Missing} and Salary < 58650, then 0 (bad) 
#If History1={low,medium,missing}.... excel sheet

#Confusion Matrix
actual<-dm$Target
predicted<-predict(mod1,type = "class")

head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

confusionMatrix(predicted,actual,positive="1")

#kappa metric
kappa2(data.frame(actual,predicted))

#ROC curve analysis
library(ROCR)
pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)


