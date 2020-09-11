#Discussion Week4

x=seq(-10,10,0.01)
F=pnorm(x)
plot(x, F, main="cumulative distribution function")

pnorm(3.25, mean = 2, sd = 4)

dnorm(0)
dnorm(3)
dnorm(-3)
dnorm(20, mean = 4, sd = 10)

x=seq(-10,10,0.01)
f=dnorm(x)
plot(x, f, main="probability density function")

x=seq(-100,108,0.01)
f=dnorm(x, mean=4, sd=10)
plot(x, f, main="probability density function")

#anova()
#H0
#H1
#rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window

#CTR+shift+c to comment, Hand-solution 
library(psych)

setwd("~/Github/Teacher-A-Statististical-Methods-BME-423/Week_4_Discussion/week4 materials/week4 materials")

describeBy(clin.trial$mood.gain,group=clin.trial$drug)
# n.placebo <- length(x=clin.trial$mood.gain[clin.trial$drug=="placebo"])
# n.anxifree <- length(x=clin.trial$mood.gain[clin.trial$drug=="anxifree"])
# n.joyzepam <- length(x=clin.trial$mood.gain[clin.trial$drug=="joyzepam"])

x.placebo<-mean(clin.trial$mood.gain[clin.trial$drug=="placebo"])
x.anxifree<-mean(clin.trial$mood.gain[clin.trial$drug=="anxifree"])
x.joyzepam<-mean(clin.trial$mood.gain[clin.trial$drug=="joyzepam"])
x <- mean(c(x.placebo,x.anxifree,x.joyzepam))

var.placebo<-var(clin.trial$mood.gain[clin.trial$drug=="placebo"])
var.anxifree<-var(clin.trial$mood.gain[clin.trial$drug=="anxifree"])
var.joyzepam<-var(clin.trial$mood.gain[clin.trial$drug=="joyzepam"])
s.w <- (5*(var.placebo)+5*(var.anxifree)+5*(var.joyzepam))/(18-3)
s.b<-6*(sd(c(x.placebo,x.anxifree,x.joyzepam)))^2

Fval<-s.b/s.w
Fval

#R-solution
my.anova <- aov( mood.gain ~ drug, clin.trial )
summary(my.anova)



