#Chi-square goodness-of-fit
#GOF tests if the observed data fits a certain distribution (probability)
#Modify cards to one column

rm(list = ls());          # clear workspace variables
cat("\014")               # it means ctrl+L. clear window

setwd("~/Github/Teacher-A-Statististical-Methods-BME-423/Week_7_Discussion")

load("randomness.Rdata")
library(lsr)
who(expand = TRUE)
head(cards[,c(1,2)])      #1st selection

observed <- table(cards[,2])      #observed selections from the data (second column)
print(observed)

#We want to test the difference between the expected probability and the observed probability
#To determine that the difference is not due to chance, we use chi-square goodness of fit test
#Check in the discussion slides to see the formula
#Let's do the test by setting the significance level = 0.05
#Go back to slide no. 17 at before using the function

chisq.test(table(cards[,2]))
# or we can use the Convenience function (goodnessOfFitTest)

goodnessOfFitTest(cards[,2])          #Input is the second column of cards

#---------------------------------Chi-square association test-------------------------------
#Association btw nominal variables

load("chapek9.Rdata")

#About the chapek9 data
who(TRUE)
head(chapek9)
summary(chapek9)


associationTest( formula = ~choice+species, data = chapek9)


chapekFrequencies <- xtabs(~choice+species,data=chapek9) #create a contingency table
print(chapekFrequencies)
chisq.test(chapekFrequencies)
chisq.test(table(chapek9))
#To gain access to the capital city, a visitor must prove that they're not a robot, not a human.
#They ask whether the visitor prefers puppies, flowers or data files.


#In-class exercise

load("gastroData.Rdata")
head(gastroData)
gastroData$Gastroenteritis=factor(gastroData$Gastroenteritis)
gastroData$Consumption=factor(gastroData$Consumption)
mydataFrequencies <- xtabs(~ Gastroenteritis + Consumption, data = gastroData)
chisq.test(mydataFrequencies)
# or we can use the Convenience function (associationTest) from: library(lsr)

associationTest( formula = ~ Gastroenteritis + Consumption, data = gastroData)


A = matrix( 
  c(13, 15, 30, 13, 44, 65), # the data elements 
  nrow=3,              # number of rows 
  ncol=2,              # number of columns 
  byrow = TRUE) 
c=rowSums(A)
r=colSums(A)
all = sum(A)

row_p = r/all
col_p = c/all

matrix(col_p)%*%t(matrix(r))
matrix(c)%*%t(matrix(row_p))

