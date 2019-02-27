##Regression Trees using rpart()##
library(dplyr)
library(rpart)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd("G:\\Gunnvant Singh\\Decision Trees")
dm<-read.csv("dm.csv")


#Minimal Data Prep

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)

summary(dm$History1)

dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

dm<-dm[,-8]
#Regresssion Tree
mod<-rpart(AmountSpent~.,data=dm[,-10],control=rpart.control(cp=0.009,minsplit = 30),method="anova")

plot(mod, margin=0.1, main="Classification Tree for Direct Marketing")
text(mod, use.n=TRUE, all=TRUE, cex=.7)

fancyRpartPlot(mod)

printcp(mod)
plotcp(mod, minline = TRUE)

mod1<-prune(mod,cp= 0.016)
fancyRpartPlot(mod1)

#Rule extraction
mod1
