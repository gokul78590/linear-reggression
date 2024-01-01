setwd("C:/Downloads")
data=read.csv("C:/Downloads/LungCapData.csv")
dim(data)
summary(data)
names(data)=lungcap
cor(data$Age,data$Height)
cor(data$Gender,data$Smoke)
summary(table(data$Smoke,data$Gender))
set.seed(234)
train<-sample(nrow(data), ceiling(nrow(data)*0.7))
test<-(1:nrow(data))[-train]
data_train=data[train,]                   
data_test=data[test,]             
model_mlm=lm(formula = LungCap~.,dat=data_train)
summary(model_mlm)
model_mlm1=lm(formula = LungCap~Age+Gender+Smoke+Height, data = data_train)
summary(model_mlm1)
require(car)
install.packages("regclass")
install.packages("car")
require(car)
require(regclass)
vif(model_mlm)
vif(model_mlm1)
pred=predict(model_mlm,data=data_test)
pred1=predict(model_mlm1,data=data_test)
pred
data_test$LungCap
pred1
order(data_test)
require(Metrics)
rmse(data_train$LungCap,model_mlm$fitted.values)
rmse(data_test$LungCap,pred)
train_error=data_train$LungCap - model_mlm$fitted.values
test_error=data_test$LungCap - pred
mse <- function(error)
{
  sqrt(mean(error^2))
}

mse(train_error)
mse(model_mlm$residuals)
mse(model_mlm1$residuals)
mse(train_error)
