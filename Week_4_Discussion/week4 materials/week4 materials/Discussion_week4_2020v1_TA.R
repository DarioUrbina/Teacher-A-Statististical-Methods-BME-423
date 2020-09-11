#Discussion Week4
rm(list = ls());        # clear workspace variables
cat("\014")             # clear console


x=seq(-10,10,0.01)      #from , to, by

F=pnorm(x)              #Distribution function    <<<<<<<<<<<<<<<<
                        #What is the probability of obtaining a value smaller that x?
                        #The first and last values clearly add too little to the probability 
                        #We can see that it is centered in 0, because we didn't specified the 
                        #normal distribution to have another mean.
                        #it has 0 mean and standard deviation of 1

plot(x, F, main="cumulative distribution function")

pnorm(3.25, mean = 2, sd = 4)  #distribution function

dnorm(0)
dnorm(3)
dnorm(-3)
dnorm(20, mean = 4, sd = 10)

x=seq(-10,10,0.01)
f=dnorm(x)
plot(x, f, main="probability density function")

#x=seq(-100,108,0.01)
x=seq(-10,18,0.01)
f=dnorm(x, mean=4, sd=10)
plot(x, f, main="probability density function")

#anova()
#H0
#H1
#rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window

#CTR+shift+c to comment, Hand-solution 
library(psych)
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

## below here is going to be deleted from the shared R file

###########################################In class exercise - Loading data
load(file = "clinicaltrial.Rdata")
 
#######################################In-class Exercises - pnorm questions
#Pr(z<=2.39)
pnorm(2.39,mean=0,sd=1,lower.tail=TRUE,log.p=FALSE)

#Pr(0<=z<=3.95)
F1<-pnorm(3.95,mean=0,sd=1)
F2<-pnorm(0,mean=0,sd=1)
F1-F2

########################################################Pr(3.25<=z)
F3<-pnorm(3.25,mean=0,sd=1)
1-F3

##########################################In class exercise - qnorm question
#Pr(z<=c) = 0.5319
qnorm(0.5319,mean=0,sd=1)

#Pr(c<=z) = 0.3483
#1-0.3483 = 0.6517
qnorm(0.6517,mean=0,sd=1)

#Pr(-c<=z<=+c) = 0.1272
#2F(c)-1 = 0.1272
#F(c) = (1+0.1272)/2 = 0.5636
qnorm(0.5636,mean=0,sd=1)
qnorm(0.5636,mean=0,sd=8)
qnorm(0.5636,mean=8,sd=1)


#Pr(z<=-c) = 0.20 = Pr(z>=+c)
#Pr(z<=-c) = 1-Pr(z<=c)
#Pr(z<=c) = 1-F(c)  
#0.20 = 1-F(c) 
#F(c) = 0.8
qnorm(0.8,mean=0,sd=1) #c
qnorm(0.2,mean=0,sd=1) #-c

##################################
install.packages("gplots")
library(gplots)
plotmeans (formula = mood.gain~drug,
           data = clin.trial, 
           xlab = "Drug administered",
           ylab = "Mood Gain",
           n.label =True)

