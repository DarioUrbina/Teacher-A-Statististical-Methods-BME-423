#Discussion Week3
#TA - Guide
#How to run, line by line press CTR+Enter on Mac, try Ctrl + R on Windows


##------------------Read All_mydata_2020.csv------------------------ 

#Use pathname
rm(list = ls()); 
cat("\014")
mypath<-'C:/Users/dario/Documents/Github/Teacher-A-Statististical-Methods-BME-423/Week_3_Discussion/All_Data_2020_0827.csv'
mydata <- read.csv(mypath,header=TRUE); 



##--------------- histogram ------------------
graphics.off() 
cat("\014")
#Simple properties
hist(x = mydata$HR,
     main="Heart rate of students",     # Title
     sub="subtitle goes here",          # Subtitle
     ylab="Heart rate",                 # ylabel
     xlab="bpm")                         # xlabel 

#Full properties
hist(x = mydata$HR,
     main="Heart rate of students",     # Title
     sub="subtitle goes here",          # Subtitle
     ylab="Heart rate",                 # ylabel
     xlab="bpm",                         # xlabel 
     density = 20,                      # shading lines: 10 per inch
     angle = 20,                        # angle of the shading lines is 40 degrees
     border = "black",                  # colour of the borders of the bars
     col = "black",                    # color of the shading lines. gray20 is darker than gray80
     labels = TRUE,                     # frequency labels to each bar
     ylim = c(0,40))                    # limit of y-axis

##---------------- boxplot --------------------- 
#Simple properties
graphics.off() 
cat("\014")
boxplot(x=mydata$HR,xlab="BME423",ylab="bpm",main="Heart rate")

#Full properties
boxplot(x=mydata$HR,
        main="Heart rate of students",  # Title
        sub="subtitle goes here",       # Subtitle
        xlab="BME423",                  #xlabel
        ylab="Heart rate",              # ylabel
        border="black",                 # dim the border
        frame.plot=TRUE,               # frame border Y/N
        staplewex=1,                    # staple Y/N
        whisklty=3                      # line type for whisker
)


#In-class exercise
#1. Are the heart data of the students who have blue or green eyes normally distributed? 
#conditional on eye color = "GRN" or "BLU"
#Solution

#___________OK___________

cat("\014")
hist(x = mydata$HR[mydata$EyeColor=="GRN" | mydata$EyeColor=="BLU"],
     main="Heart rate of students",     # Title
     sub="subtitle goes here",          # Subtitle
     ylab="Heart rate",                 # ylabel
     xlab="bpm") 

#hist(x = mydata$HR[mydata$EyeColor=="GRN" | mydata$EyeColor=="BLU"],
#     main="Heart rate of students",     # Title
#     sub="subtitle goes here",          # Subtitle
#     ylab="Heart rate",                 # ylabel
#     xlab="bpm") 


#2 (2020). Is there any outlier in the heart rate data of the male students that have a thumb width larger than 19 mm?
#If there is, what is the outlier?

#Solution
boxplot(x=mydata$HR[mydata$WidthRightThumb >=19 & mydata$Sex=="M" ],xlab="BME423",ylab="bpm",main="Heart rate")
mydata$HR[mydata$WidthRightThumb >=19 & mydata$Sex=="M" & mydata$HR>88]
# Command+shift+c in OS X to comment out multiple lines

#-------------------Descriptive Statistics--------------------------
#mean
mean(mydata$WidthRightThumb)
# trimmed 10% from the edge
mean( x = mydata$WidthRightThumb, trim = .1) 

#___________Ok______________#
#mode must download library lsr
library(lsr) 
cat("\014")
boxplot(x=mydata$WidthRightThumb,xlab="BME423",ylab="mm",main="Width of Right Thumb")
outlierremoved_WidthRightThumb<-mydata$WidthRightThumb[mydata$WidthRightThumb<30]
hist(x = outlierremoved_WidthRightThumb,
     main="Width of right thumb",     # Title
     sub="One outlier removed",       # Subtitle
     ylab="Counts",                   # ylabel
     xlab="mm",
     ylim = c(0,12)) 
modeOf(outlierremoved_WidthRightThumb)
hist(x = mydata$WidthRightThumb,
     main="Width of right thumb",     
     sub="No outliers removed",     ylab="Counts",     xlab="mm",     ylim = c(0,15)) 

#Mode can use with non-numeric value
modeOf(mydata$Handedness) 

mean( x = mydata$WidthRightThumb, trim = .1) # trimmed 10% from the edge
range(mydata$HR)


#Based on the histogram, can you roughly guess the Q1, and Q3 of the data
0.25*length(x=outlierremoved_WidthRightThumb) #How many = 25%
0.75*length(x=outlierremoved_WidthRightThumb)

quantile(x=outlierremoved_WidthRightThumb,probs=.5) # what does it mean?
quantile(x=outlierremoved_WidthRightThumb,probs=c(.25,.75)) # same as interquartile range


IQR(x=outlierremoved_WidthRightThumb) # interquartile range
var(mydata$HR) # variance
sd(mydata$HR) # standard deviation


#------------------ Skewness and Kurtosis --------------------
library(psych)
hist(x = mydata$Height,
     main="Height",     # Title
     sub="",  # Subtitle
     ylab="Counts",                 # ylabel
     xlab="Inches") 
skew(mydata$Height) # asymmetry of probability distribution
# Shape of the distribution (flat or pointy?)
kurtosi(mydata$Height) 



#Exercise 
#1. Plot histogram of the heart rate data of the students who have blue or green
#2. Can you tell if the distribution is skewed?
#3. Calculate the skewness and compared the value to that of the heart rate data of the students who have brown eyes

cat("\014")
eyecondition_HR<-mydata$HR[mydata$EyeColor=="GRN" | mydata$EyeColor=="BLU"]
hist(x = eyecondition_HR,
     main="Heart rate of students",     # Title
     sub="Green or Blue eyes",          # Subtitle
     ylab="No. students",                 # ylabel
     xlab="bpm") 

skew.HRgb<-skew(eyecondition_HR)


eyecondition_HR<-mydata$HR[mydata$EyeColor=="BRN"]
hist(x = eyecondition_HR,
     main="Heart rate of students",     # Title
     sub="Brown Eyes",          # Subtitle
     ylab="No. students",                 # ylabel
     xlab="bpm") 


skew.HRbrn<-skew(eyecondition_HR)
skew.HRgb
skew.HRbrn


## --------------- Summary vs. Describe vs. Describeby()
#What's the difference between summary, describe and describeby
library(psych)
cat("\014")
summary(mydata) #all columns
summary(mydata$HR)
IQR(x=mydata$HR) #Compare to summary
cat("\014")
describe(mydata$HR) # must install & load libray("pschy")
describe(mydata$HR,quant=c(.25,.75), IQR=TRUE) # must install & load libray("pschy")



#Full properties
describe(mydata,
         na.rm = TRUE, #delete missing data
         interp=FALSE, #
         skew = TRUE,  #calculate skew and kurtosis
         ranges = TRUE, #calculate range
         trim=.1,  #trim means->default
         type=3,   #which estimate of skew and kurtosis
         check=TRUE, #check non-numeric variables
         fast=NULL, #true will improve speed
         quant=c(.25,.75) , #find the specified quantiles
         IQR=TRUE, #calculate the interquartile range
         omit=FALSE) #do not convert non-numerical values to numeric

#We are not using these three lines
describe(mydata$HR[mydata$Sex=="F"],quant=0.25)
describe(mydata$HR[mydata$Sex=="M"],quant=0.25)
quantile(mydata$HR[mydata$Sex=="M"],0.25)



cat("\014")
describeBy(x=mydata,group=mydata$Sex,mat=FALSE) #Use the same argument as describe
cat("\014")


describeBy(x=mydata$HR,group=mydata$Sex,mat=TRUE,quant=c(.25,.75)) #matrix (data.frame) output
describeBy(x=mydata$HR,list(mydata$EyeColor,mydata$Sex),mat=FALSE,quant=c(.25,.75)) 



#In-class exercise
#1. Calculate the standard error
describeBy(x=mydata$WidthRightThumb,group=mydata$Sex,mat=FALSE,quant=.16) #matrix (data.frame) output
#Female
#se= sd/sqrt(n)
se.female<-3.58/34
se.female
#Male
se.male<-7.34/22
se.male

#2. Calculate the 25th and 75th percentile
describeBy(x=mydata$HR,group=mydata$Sex,mat=FALSE,quant=c(.25,.75)) #matrix (data.frame) output

