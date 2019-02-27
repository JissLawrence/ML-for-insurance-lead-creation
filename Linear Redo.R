
data<-read.csv("DirectMarketing.csv")
library(dplyr)
library(ggplot2)
library(car)

head(data)

##Do exploratory analysis##
plot(data$Age,data$AmountSpent,col="red")


#Combine the Middle and Old levels together
data$Age1<-ifelse(data$Age!="Young","Middle-Old",as.character(data$Age))
data$Age1<-as.factor(data$Age1)
summary(data$Age1)

plot(data$Age1,data$AmountSpent)

#Gender
plot(data$Gender,data$AmountSpent,col="red")

#Own house
summary(data$OwnHome)
plot(data$OwnHome,data$AmountSpent,col="red")

#Married
summary(data$Married)
plot(data$Married,data$AmountSpent,col="red")

#Location
summary(data$Location)
plot(data$Location,data$AmountSpent,col="red")

#Salary
summary(data$Salary)


plot(data$Salary,data$AmountSpent)#Might be heteroescadasticity

#Children
summary(data$Children)

data$Children<-as.factor(data$Children)

plot(data$Children,data$AmountSpent,col="red")

data$Children1<-ifelse(data$Children==3|data$Children==2,"3-2",as.character(data$Children))
data$Children1<-as.factor(data$Children1)

summary(data$Children1)

plot(data$Children1,data$AmountSpent,col="red")

#History
summary(data$History)
#Impute Missing values
tapply(data$AmountSpent,data$History,mean)
ind<-which(is.na(data$History))
mean(data[ind,"AmountSpent"])

data%>%filter(History=="Medium")%>%select(AmountSpent)->Amt_M
p<-ggplot(data[ind,],aes(x=AmountSpent))
q<-ggplot(Amt_M,aes(x=AmountSpent))


p+geom_histogram()
q+geom_histogram()

#Create a category called missing
data$History1<-ifelse(is.na(data$History),"Missing",as.factor(data$History))
data$History1<-as.factor(data$History1)

summary(data$History1)

data$History1<-factor(data$History1,labels=c("High","Low","Medium","Missing"))

#Catalogues
summary(data$Catalogs)

data1<-data[,-c(1,7,8)]

mod1<-lm(AmountSpent~.,data=data1)

summary(mod1)

mod2<-lm(formula = AmountSpent ~ Gender + Location + Salary + Catalogs + Children1 + History1, data = data1)

summary(mod2)

summary(data1)
#Remove insignificant variabes
#HistoryMissing
#GenderMale

#Create dummy variables
data1$Male_d<-ifelse(data1$Gender=="Male",1,0)
data1$Female_d<-ifelse(data1$Gender=="Female",1,0)

data1$Missing_d<-ifelse(data$History1=="Missing",1,0)
data1$Low_d<-ifelse(data$History1=="Low",1,0)
data1$Med_d<-ifelse(data$History1=="Medium",1,0)
data1$High_d<-ifelse(data$History1=="High",1,0)

mod3<-lm(formula = AmountSpent ~ Male_d + Location + Salary + Catalogs + Children1+Med_d+Low_d , data = data1)

summary(mod3)

mod4<-lm(formula = AmountSpent ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)

summary(mod4)

#Signs
tapply(data$AmountSpent,data$History,mean)
data1%>%filter(History1!="Medium",History1!="Low")%>%summarize(Mean=mean(AmountSpent)) #inline

tapply(data1$AmountSpent,data1$Location,mean) #inline

#Assumption checks

hist(mod4$residuals)
qqPlot(mod4$residuals)
#Non normal behaviour observed

#Multicollinearity Check

vif(mod4)

#Constant  variance check
plot(mod4$fitted.values,mod4$residuals) #Funnel shape

#Remidies: Apply log transform to y variable

mod5<-lm(formula = log(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)

summary(mod5)

qqPlot(mod5$residuals)#qqplot looks okay
plot(mod5$fitted.values,mod5$residuals)# Still funnel


summary(mod5)
#Apply square root transform

mod6<-lm(formula = sqrt(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod6)
qqPlot(mod6$residuals)
plot(mod6$fitted.values,mod6$residuals)#Seems okay

vif(mod6)

predicted<-mod6$fitted.values
actual<-sqrt(data1$AmountSpent)

dat<-data.frame(predicted,actual)

p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))
p+geom_line(colour="blue")+geom_line(data=dat,aes(y=actual),colour="black")

