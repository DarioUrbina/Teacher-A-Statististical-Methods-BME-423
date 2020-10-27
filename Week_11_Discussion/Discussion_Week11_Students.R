#BME423 Discussion Week 11

##Mandatory lines
# Similiar thing as clc;clear all;close all in MATLAB
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots


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

#In-class Exercise : Multiple linear regression   ########################################################################################
#
#
#
#
#
#
####################################################################################################################

#Revisit from last week #############################################################################################################

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


#Confidence interval of estimates
confint(regression.1, level=0.95)

plot(mydata$Height, mydata$Weight,ylab="Weight",xlab="Height")



