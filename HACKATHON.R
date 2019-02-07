library(gains)
library(dplyr)
library(irr)
library(caret)
setwd("E:\\Data\\10")
dm<-read.csv("dm.csv")
#identify customer id's who are considered good according to the amount spend is above the mean(1216)

dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm%>%select(-AmountSpent)->dm
View(dm)

summary(dm)

#Minimal Data Prep

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)

summary(dm$History1)

dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

dm<-dm[,-8]

#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(dm),0.70*nrow(dm),replace=F)
train<-dm[index,]
test<-dm[-index,]
View(Target)

#Build the first model using all the variables 

mod<-glm(Target~.,data=train[,-9],family="binomial")
summary(mod)

step(mod,direction="both")

mod1<-glm(formula = Target ~ Age + Location + Salary + Children + Catalogs +  History1, family = "binomial", data = train)

summary(mod1)

#Creating dummies

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)

train$Hist.Mid_d<-ifelse(train$History1=="Medium",1,0)

train$Children2_d<-ifelse(train$Children=="2",1,0)

train$Children3_d<-ifelse(train$Children=="3",1,0)

test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)

test$Hist.Mid_d<-ifelse(test$History1=="Medium",1,0)

test$Children2_d<-ifelse(test$Children=="2",1,0)

test$Children3_d<-ifelse(test$Children=="3",1,0)

mod2<-glm(Target~AgeYoung_d+Location+Salary+Children3_d+Children2_d+Catalogs+Hist.Mid_d,data=train,family="binomial")

summary(mod2)

pred<-predict(mod2,type="response",newdata=test)

head(pred)

table(dm$Target)/nrow(dm)
pred<-ifelse(pred>=0.399,1,0)

kappa2(data.frame(test$Target,pred))

confusionMatrix(pred,test$Target,positive="1")

gains(test$Target,predict(mod2,type="response",newdata=test),groups = 10)

test$prob<-predict(mod2,type="response",newdata=test)

quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted<-test[test$prob>0.732602471&test$prob<=0.999747759,"Cust_Id"]

View(targeted)
