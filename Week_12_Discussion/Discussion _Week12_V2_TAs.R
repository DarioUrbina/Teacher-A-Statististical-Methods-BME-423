#Discussion week 12
## 1. Mandatory lines
# Similiar thing as clc;clear all;close all in MATLAB
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots

# In-class exercise
# Example from Navarro - chico.Rdata
load("~/Github/Teacher-A-Statististical-Methods-BME-423/Week_12_Discussion/chico.Rdata")
View(chico)
head(chico)

library(psych)
describe(chico)

# plot(chico$grade_test2, chico$grade_test1,ylab="Grade for Test 2",xlab="Grade for Test 1")
chico$improvement <- chico$grade_test2 - chico$grade_test1
head(chico)
hist(x=chico$improvement, xlab="Improvement in Grade")
library(lsr)
ciMean(x=chico$improvement)
ciMean(x=chico$grade_test1)
ciMean(x=chico$grade_test2)


pairedSamplesTTest(
  formula = ~ grade_test2 + grade_test1, # one-sided formula listing the two variables
  data = chico # data frame containing the two variables
)

#converting the wide form chico data to long form version 
chico2 <- wideToLong( chico, within="time" )
head( chico2 )
tail( chico2 )
pairedSamplesTTest(
  formula = grade ~ time, # two sided formula: outcome ~ group
  data = chico2, # data frame
  id = "id" # name of the id variable
)

pt(-6.475, 19)*2   # Student t Distribution 
#or
pairedSamplesTTest(
  formula = grade ~ time, # two sided formula: outcome ~ group
  data = chico2, # data frame
  id = "id" # name of the id variable
)$p.value



rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots

#In-class exercise 2
mypath <- "~/Desktop/Week12/ReactionTime.csv"
ReactionTime <- read.csv(mypath,header=TRUE)
View(ReactionTime)
head(ReactionTime)
library(lsr)


#converting the wide form chico data to long form version
ReactionTime2 <- wideToLong( ReactionTime, within="LiquidConsumption" )
head( ReactionTime2 )
pairedSamplesTTest(
  formula = RT ~ LiquidConsumption, # two sided formula: outcome ~ group
  data = ReactionTime2, # data frame
  id = "id" # name of the id variable
)
pt(-4.149, 9)*2

#or 
pairedSamplesTTest(
  formula = RT ~ LiquidConsumption, # two sided formula: outcome ~ group
  data = ReactionTime2, # data frame
  id = "id" # name of the id variable
)$p.value

# in wide form
pairedSamplesTTest(
  formula = ~ RT_caffeine + RT_water, # one-sided formula listing the two variables
  data = ReactionTime # data frame containing the two variables
)