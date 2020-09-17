#Clear
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window



#Posthoc pairwise



#Independent t-test

#mypath = "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_6_Discussion/harpo.Rdata";
#load(mypath)

setwd("~/Github/Teacher-A-Statististical-Methods-BME-423/Week_6_Discussion")
load("harpo.Rdata")
#View(harpo)
head(harpo)

#Example : harpo
library(lsr)
independentSamplesTTest(
  formula = grade~tutor, #formula specifying outcome and group variables
  data = harpo,          #dataframe
  var.equal = TRUE,      #assume that the two groups have the same variance  
  )



  #In-class exercise

#Use pathname
rm(list = ls()); 
cat("\014")
mypath = "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_6_Discussion/All_Data_2020_0827.csv";
mydata <- read.csv(mypath,header=TRUE); 
View(mydata)
cat("\014")

height.female <- mydata$Height[mydata$Sex=="F"]
height.male <- mydata$Height[mydata$Sex=="M"]


#Test for normality : shapiro.test, skip this part during the discussion, only mention that we assume normality for both groups
# H0: the data is normally distributed, if p>0.05, normality can be assumed
#shapiro.test(height.male) 
#shapiro.test(height.female) 

#Get subset of data
subset.mydata <- mydata[,c("Sex","Height")]  
independentSamplesTTest(
  formula = Height~Sex, #formula specifying outcome and group variables
  data = subset.mydata, #dataframe
  var.equal = TRUE,     #assume that the two groups have the same variance  
)


#One sample t-test
mypath = "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_6_Discussion/harpo.Rdata";
load(mypath)
View(harpo)

oneSampleTTest( x=harpo$grade, mu=70)


#In-class exercise

#Use pathname
rm(list = ls()); 
cat("\014")
mypath = "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_6_Discussion/All_Data_2020_0827.csv";
mydata <- read.csv(mypath,header=TRUE); 

mydata <- read.csv(mypath,header=TRUE); 

height.female <- mydata$Height[mydata$Sex=="F"]
oneSampleTTest( x=height.female, mu=64)

#Pairwise and posthocpairwise T test
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window

#Load the clinical trials data from discussion Week4
mypath = "C:/Users/dario/Documents/Github/Teacher-A-Statististical-Methods-BME-423/Week_4_Discussion/week4 materials/week4 materials/clinicaltrial.Rdata";

load(mypath)
View(clin.trial)
#Run the ANOVA test again:
my.anova <- aov( mood.gain ~ drug, clin.trial ) 
summary(my.anova )
library(lsr)

pairwise.t.test( x = clin.trial$mood.gain, # outcome variable
                 g = clin.trial$drug, # grouping variable
                 p.adjust.method = "none" # which correction to use?- NOT IMPORTANT
)

#Can we run it directly using the my.anova variable we previously created? Yes
posthocPairwiseT( my.anova )

posthocPairwiseT( my.anova,   p.adjust.method = "bonferroni" # for Bonferroni correction
)
posthocPairwiseT( my.anova,   # for Holm correction (default)
)


####SO far ok
#Discussion Assignment 2 - Answer Key

rm(list = ls()); 
cat("\014")

mypath = "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_6_Discussion/All_Data_2020_0827.csv";
mydata <- read.csv(mypath,header=TRUE); 


#QUESTION 1
HR.female <- mydata$HR[mydata$Sex=="F"]
HR.male <- mydata$HR[mydata$Sex=="M"]

#Get subset of data
subset.mydata <- mydata[,c("Sex","HR")]  
independentSamplesTTest(
  formula = HR~Sex, #formula specifying outcome and group variables
  data = subset.mydata,          #dataframe
  var.equal = TRUE,      #assume that the two groups have the same variance  
)

#QUESTION 2
HR.male <- mydata$HR[mydata$Sex=="M"]
oneSampleTTest( x=HR.male, mu=65)

