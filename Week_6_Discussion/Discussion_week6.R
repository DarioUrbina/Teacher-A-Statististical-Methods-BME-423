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


####In-class exercise###
#Compare height of female and male students 
########################

#One sample t-test
mypath = "~/Github/Teacher-A-Statististical-Methods-BME-423/Week_6_Discussion/harpo.Rdata";
load(mypath)
View(harpo)

oneSampleTTest( x=harpo$grade, mu=70)


####In-class exercise###
#Check if the mean height of female students is different from 64
#
#
########################

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


#Discussion Assignment 2#####

#############################

