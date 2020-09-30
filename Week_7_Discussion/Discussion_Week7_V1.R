#Chi-square goodness-of-fit
#GOF tests if the observed data fits a certain distribution (probability)
#Modify cards to one column
rm(list = ls()); 
cat("\014")
setwd("~/Desktop/Week_7_materials")
load("randomness.Rdata")
library(lsr)
who(expand = TRUE)
head(cards[,c(1,2)])      #1st selection
TotalSelections <- 200    #200 subjects

H0 <- 'All four suits are chosen with equal probability (Prob=0.25)'
H1 <- 'All four suits are chosen with unequal probability (Prob~=0.25)'

expected <- 200*c(clubs = 0.25, diamonds = 0.25, hearts= 0.25, spades= 0.25)
print(expected)


observed <- table(cards[,2])      #observed selections from the data
print(observed)

#We want to test the difference between the expected probability and the observed probability
#To determine that the difference is not due to chance, we use chi-square goodness of fit test
#Check in the discussion slides to see the formula
#Let's do the test by setting the significance level = 0.05
#Go back to slide no. 17 at before using the function

chisq.test(table(cards[,2]))
# or we can use the Convenience function (goodnessOfFitTest)
#library(lsr)
goodnessOfFitTest(cards[,2])          #Input is the second column of cards

#---------------------------------Chi-square association test-------------------------------
#Association btw nominal variables
#setwd("~/Desktop/Week_7_materials")
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
