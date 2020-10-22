#BME423 discussion-Week10

#<Regression & correlation>

## 1. Mandatory lines
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots

# In-class R code:Martian height versus weight.

#Load the martians data set
#Plot a scatter plot of the data
 
#Plot the line on the data
#5.- Provide a 95% CI of the slope and intercept

#1.- Find the linear regression line properties (slope and intercept, 
# 2.- residual error, 
# 3.- Standard error of slope and intercept)
# 4.- Find the correlation of the data

mypath <- "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_10_Discussion/martians.csv"
mydata <- read.csv(mypath,header=TRUE); 
View(mydata)
#cat("\014")
regression.1 <- lm(formula = Weight ~ Height, data = mydata )
summary(regression.1)
coefficients(regression.1)
residuals(regression.1)
confint(regression.1, level=0.95)

plot(mydata$Height, mydata$Weight,ylab="Weight",xlab="Height")
#Plotting the regression line
abline(regression.1)
cor( mydata$Height, mydata$Weight )
K<-data.frame(Height=c(35, 40, 45))         #DUM check the function of this##############
K$Weight<-predict(regression.1, newdata=K) # Where K = X coordinate, OPTION = confidence / prediction confidence: Line of Means, prediction: observation
points(K$Height, K$Weight,ylab="Weight",xlab="Height", col="red")
# Exercise Example: In patients diagnosed with acute dilated
# cardiomyopathy resulting from myocarditis, examine the
# relationship between left ventricular ejection fraction (LVEF) and
# age.

#Load the cardiomyopathy dataset
#Plot a scatter plot of the data
#Find the linear regression line properties
#Plot the line on the data
#Find the correlation of the data
#Provide a 95% CI of the slope and intercept

mypath2 <- "~/Desktop/week10/cardiomyopathy.csv"
mydata2 <- read.csv(mypath2,header=TRUE)
View(mydata2)
plot(mydata2$Age, mydata2$LVEF, ylim=c(0.05,0.45),xlim=c(18,76),ylab="LVEF",xlab="Age")

regression.2 <- lm(formula = LVEF ~ Age, data = mydata2 )
summary(regression.2)
confint(regression.2, level=0.95)
#Plotting the regression line
abline(regression.2)

cor( mydata2$Age, mydata2$LVEF )


# In-class Exercise: In martians.csv, examine the
# relationship between Weight of the Martian population and their Food Consumption

#Load the data
#Plot a scatter plot of the data
#Find the linear regression line properties
#Plot the line on the data
#Find the correlation of the data
#Provide a 95% CI of the slope and intercept

mypath3 <- "~/Desktop/week10/martians.csv"
mydata3 <- read.csv(mypath3,header=TRUE); 
View(mydata3)
cat("\014")

regression.3 <- lm(formula = Weight ~ Food_Consumption, data = mydata3 )
summary(regression.3)
coefficients(regression.3)
residuals(regression.3)
confint(regression.3, level=0.95)

plot(mydata3$Food_Consumption, mydata3$Weight,ylab="Weight",xlab="Food_Consumption")
#Plotting the regression line
abline(regression.3)
cor( mydata3$Food_Consumption, mydata3$Weight )
