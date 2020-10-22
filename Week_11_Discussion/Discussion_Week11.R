#BME423 Discussion Week 11

##Mandatory lines
# Similiar thing as clc;clear all;close all in MATLAB
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots


#In-class Exercise : Multiple linear regression
mypath = "~/Github/Teacher-A-Statististical-Methods-BME-423/Regression data refs/BP_Child_Age_Wght_v2.csv";
infant.data <- read.csv(mypath,header=TRUE,sep=",")


head(infant.data) #What are the variables?
tail(infant.data)

plot(x=infant.data$Weight.oz, y=infant.data$SBPchild,
     ylab="SBP",
     xlab="Birthweight",
     cex=2)

plot(x=infant.data$Age.days, y=infant.data$SBPchild,
     ylab="SBP",
     xlab="Age",
     col="black",
     pch = 19, #solid circle
     cex=2)


infant.regression <- lm(formula = SBPchild ~ Weight.oz+Age.days,data=infant.data)
coef(infant.regression)

summary(infant.regression)
yhat<-predict(infant.regression)
print(yhat)

#Create two separate data frames and predict y 
#to see how each variable contributes to explaining y
n=length(infant.data$SBPchild)
yhat.weight <- predict(infant.regression, newdata=data.frame(Weight.oz=infant.data$Weight.oz,Age.days=rep(0,n)))
yhat.age <- predict(infant.regression, newdata=data.frame(Weight.oz=rep(0,n),Age.days=infant.data$Age.days))

#Prediction by Weight.oz
plot(x=infant.data$Weight.oz, y=infant.data$SBPchild,
     ylab="SBP",
     xlab="Birthweight",
     cex=2)

par(new=TRUE)
plot(x=infant.data$Weight.oz,y=yhat.weight,
     ylab="SBP",
     xlab="Birthweight",col="blue",pch=19,cex=2,axes=FALSE) #prediction

#Prediction by Age.days
plot(x=infant.data$Age.days, y=infant.data$SBPchild,
     ylab="SBP",
     xlab="Age",
     col="black",
     pch = 19, #solid circle
     cex=2)

par(new=TRUE)
plot(x=infant.data$Age.days,y=yhat.age,
     ylab="SBP",
     xlab="Age",col="green",
     pch=19,cex=2,
     axes=FALSE) #prediction


#Residual Standard error (Like Standard Deviation)
residuals <- infant.data$SBPchild-yhat
print(residuals)
print(infant.regression$residuals)

k=length(infant.regression$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(infant.regression$residuals^2)
n=length(infant.regression$residuals)
MSres <- sqrt(SSE/(n-(1+k))) #Residual Standard Error

#Standard error of the slopes
mean.weight <- mean(infant.data$Weight.oz)
sd.weight <- sqrt(var(infant.data$Weight.oz))
s_b1 <- (1/sqrt(n-1))*(MSres/sd.weight)
print(s_b1) #standard error of b1



#Revisit from last week
mypath <- "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_10_Discussion/martians.csv"
mydata <- read.csv(mypath,header=TRUE)

plot(mydata$Height, mydata$Weight,ylab="Weight",xlab="Height")
regression.1 <- lm(formula = Weight ~ Height, data = mydata )
head(mydata)
summary.regression.1 <- summary(regression.1)
summary.regression.1 #Q1,Q2,Q3,Q4


bme423_calculate_CI <- function(all_x,x,regression,summary.regression){
  mean_x <- mean(all_x)
  Sx_square <- var(all_x)
  n <- length(all_x)
  res.se <- summary.regression$sigma
  Sy <- res.se*sqrt((1/n)+(((x-mean_x)^2)/((n-1)*Sx_square)))
  t_0.05 <- qt(c(.025, .975), df=n-2)
  b<-coef(regression)
  ypred.x <- b[1]+x*b[2]
  CI.LOM <- c(ypred.x-Sy*t_0.05[2],ypred.x+Sy*t_0.05[2])
  #print(CI.LOM)
  
  Synew <- res.se*sqrt(1+(1/n)+(((x-mean_x)^2)/((n-1)*Sx_square)))
  CI.SO <- c(ypred.x-Synew*t_0.05[2],ypred.x+Synew*t_0.05[2])
  #print(CI.SO)
  
  CI.result <- data.frame("CI.LOM" = CI.LOM, "CI.SO" = CI.SO)
  return(CI.result)
}

bme423_calculate_CI(mydata$Height,40,regression.1,summary.regression.1)
plot(mydata$Height, mydata$Weight,ylab="Weight",xlab="Height")
abline(regression.1)
arrows(40, 10.92, 40, 12.59, length=0.05, angle=90, code=3)
arrows(40, 9.38, 40, 14.13, length=0.05, angle=90, code=3,col='red')




## Do not use below this line
x <- mydata$Height
CI.LOM.lowerbound <- rep(0,length(x))
CI.LOM.upperbound <- rep(0,length(x))

CI.SO.lowerbound <- rep(0,length(x))
CI.SO.upperbound <- rep(0,length(x))

count <- 1
for (val in x) {
  result<-bme423_calculate_CI(mydata$Height,val,regression.1,summary.regression.1)
  CI.LOM.lowerbound[count]<-result$CI.LOM[1] 
  CI.LOM.upperbound[count]<-result$CI.LOM[2] 
  
  CI.SO.lowerbound[count]<-result$CI.SO[1] 
  CI.SO.upperbound[count]<-result$CI.SO[2] 
  count = count+1
}



#Confidence interval of estimates
confint(regression.1, level=0.95)

plot(mydata$Height, mydata$Weight,ylab="Weight",xlab="Height")



