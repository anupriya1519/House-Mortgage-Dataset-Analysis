---
title: "Home Mortgage Disclosure Act Data"
output: html_notebook
---




library(AER)
data(HMDA)
dataset<- data.frame(HMDA)
names(dataset)
dim(dataset)
str(dataset)
summary(dataset)
head(dataset)
sapply(dataset,function(x) sum(is.na(x)))




qqnorm(dataset$pirat)
qqline(dataset$pirat,col='blue')
plot(density(dataset$pirat),main='Pirat')




qqnorm(dataset$hirat)
qqline(dataset$hirat,col='blue')
plot(density(dataset$hirat),main='hirat')



qqnorm(dataset$lvrat)
qqline(dataset$lvrat,col='blue')
plot(density(dataset$lvrat),main='lvrat')




qqnorm(dataset$unemp)
qqline(dataset$unemp,col='blue')
plot(density(dataset$unemp),main='Unemp')

library(ggplot2)
p10 <- ggplot(dataset, aes(x = deny, y = unemp)) +
        geom_boxplot()
p10
p11 <- ggplot(dataset, aes(x = deny, y = pirat)) +
        geom_boxplot()
p11

dt = sort(sample(nrow(dataset), nrow(dataset)*.8))
train<-dataset[dt,]
test<-dataset[-dt,]

model1 <- glm(deny~.,family=binomial(link='logit'),data=train)
summary(model1)

anova(model1, test="Chisq")

model2 <- glm(deny~pirat+lvrat+chist+phist+selfemp+insurance+afam+single+hschool,family=binomial(link='logit'),data=train)
summary(model2)

anova(model2, test="Chisq")

model3 <- glm(deny~pirat+lvrat+chist+phist+insurance+afam+single,family=binomial(link='logit'),data=train)
summary(model3)

anova(model3, test="Chisq")

fit1<- glm(deny~pirat,family=binomial(link='logit'),data=train)
summary(fit1)
fit2<-glm(deny~pirat+hirat,family=binomial(link='logit'),data=train)
summary(fit2)
fit3<-glm(deny~pirat+lvrat,family=binomial(link='logit'),data=train)
summary(fit3)
fit4<-glm(deny~pirat+lvrat+chist,family=binomial(link='logit'),data=train)
summary(fit4)
fit5<-glm(deny~pirat+lvrat+chist+mhist,family=binomial(link='logit'),data=train)
summary(fit5)
fit6<-glm(deny~pirat+lvrat+chist+phist,family=binomial(link='logit'),data=train)
summary(fit6)
fit7<-glm(deny~pirat+lvrat+chist+phist+unemp,family=binomial(link='logit'),data=train)
summary(fit7)
fit8<-glm(deny~pirat+lvrat+chist+phist+selfemp,family=binomial(link='logit'),data=train)
summary(fit8)
fit9<-glm(deny~pirat+lvrat+chist+phist+insurance,family=binomial(link='logit'),data=train)
summary(fit9)
anova(fit9, test="Chisq")
fit10<-glm(deny~pirat+lvrat+chist+phist+insurance+condomin,family=binomial(link='logit'),data=train)
summary(fit10)
fit11<-glm(deny~pirat+lvrat+chist+phist+insurance+afam,family=binomial(link='logit'),data=train)
summary(fit11)
fit12<-glm(deny~pirat+lvrat+chist+phist+insurance+afam+single,family=binomial(link='logit'),data=train)
summary(fit12)
fit13<-glm(deny~pirat+lvrat+chist+insurance+afam+single+hschool,family=binomial(link='logit'),data=train)
summary(fit13)



model3 <- glm(deny~pirat+lvrat+chist+phist+insurance+afam+single,family=binomial(link='logit'),data=train)
summary(model3)

model2 <- glm(deny~pirat+lvrat+chist+phist+selfemp+insurance+afam+single+hschool,family=binomial(link='logit'),data=train)

fit12<-glm(deny~pirat+lvrat+chist+phist+insurance+afam+single,family=binomial(link='logit'),data=train)

fm1 <- lm(I(as.numeric(deny) - 1) ~ pirat+lvrat+chist+phist+insurance+afam+single, data = dataset)
summary(fm1)
fm2 <- lm(I(as.numeric(deny) - 1) ~ pirat+lvrat+chist+insurance+afam+single+hschool, data = dataset)
summary(fm2)
fm3 <- lm(I(as.numeric(deny) - 1) ~., data = dataset)

plot(model3)

full_model<-glm(deny~.,family=binomial(link='logit'),data=train)

model3 <- glm(deny~pirat+lvrat+chist+phist+insurance+afam+single,family=binomial(link='logit'),data=train)
anova(full_model,model3)

train$model3 <- predict(model2, train, type="response")
head(train)
tail(train)
library(gmodels)
library(ggplot2)
library (Hmisc)
library (caTools)
library (ROCR)
colAUC(train$model3,train$deny, plotROC=TRUE)

predict1 <- ifelse(train$model3>0.95, 1, 0)

tab1 <- table(predicted = predict1, actual = train$deny)
tab1
accuracy1<-(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
recall1<-(tab1[2,2])/(tab1[1,2]+tab1[2,2])
precision1<-(tab1[2,2])/(tab1[2,2]+tab1[2,1])
print(c("Accuracy:",accuracy1))
print(c("Precision:",precision1))
print(c("Recall:",recall1))

test$model3 <- predict(model3, test, type='response')


colAUC(test$model3,test$deny, plotROC=TRUE)

predict2 <- ifelse(test$model3>0.95, 1,0)
tab2 <- table(predicted = predict2, actual = test$deny)
tab2
accuracy<-(tab2[1,1]+tab2[2,2])/(tab2[1,1]+tab2[2,2]+tab2[1,2]+tab2[2,1])
recall<-(tab2[2,2])/(tab2[1,2]+tab2[2,2])
precision<-(tab2[2,2])/(tab2[2,2]+tab2[2,1])

print(c("Accuracy:",accuracy))
print(c("Precision:",precision))
print(c("Recall:",recall))





