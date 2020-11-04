#Discussion week 12
## 1. Mandatory lines
# Similiar thing as clc;clear all;close all in MATLAB
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots

# In-class exercise
# Example from Navarro - chico.Rdata
load("~/Desktop/Week12/chico.Rdata")
View(chico)
head(chico)
library(psych)
describe(chico)

plot(chico$grade_test2, chico$grade_test1,ylab="Grade for Test 2",xlab="Grade for Test 1")
chico$improvement <- chico$grade_test2 - chico$grade_test1
head(chico)
hist(x=chico$improvement, xlab="Improvement in Grade")
library(lsr)
ciMean(x=chico$improvement)

#In-class exercise 2
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots

mypath <- "~/Desktop/Week12/ReactionTime.csv"
