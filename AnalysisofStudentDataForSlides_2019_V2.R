# Script for Analysis of BME 423 Student Data

## 1. Mandatory lines
# Similiar thing as clc;clear all;close all in MATLAB
rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots
DIR<-"/Users/brittanypkay/Dropbox (University of Southern California)/BME423 2019Fa/Lectures/Week 1/Student stats"; # Current directory
setwd(DIR); # change working directory

## 2. Load library
# install.packages("psych")
# install.packages("car")
# install.packages("lsr")
# install.packages("ggplot2")
library(psych) # function describeBy
library(lsr)
library(survival)
library(ggplot2)
library(car) # function scatterplot
library(lattice) # function histogram

## 3. Student data analysis
# Heart rate/Height/Sex/Side of room/Exercise frequency/Handedness/WidthRightThumb
DATA<-read.csv("All_Data_2019.csv", sep=",") 
mean(DATA$HR)
var(DATA$HR) # variance
sd(DATA$HR) # standard deviation
range(DATA$HR)
quantile(x=DATA$HR,probs=.5) # median
quantile(x=DATA$HR,probs=c(.25,.75)) # same as interquartile range
IQR(x=DATA$HR) # interquartile range

summary(DATA)
describe(DATA)
summary(DATA$HR)

# 4. Analysis of HR
# histogram
hist(x = DATA$HR,
     main="Histogram of HR for all students",     # Title
     ylab="Frequency",                            # axis label
     xlab="HR (beats/min)",                       # axis label
#     density = 20,                               # shading lines: 20 per inch
#     angle = 20,                                 # angle of the shading lines is 20 degrees
#     border = "black",                           # colour of the borders of the bars
#     col = "black",                              # color of the shading lines. gray20 is darker than gray80
     labels = TRUE,                               # frequency labels to each bar
     xlim = c(40,140),                            # limits of axis
     ylim = c(0,30),                              # limits of axis
     breaks=seq(40,140,by=10)                     # bin breaks
     )

# boxplot     
boxplot(x=DATA$HR,
        main="Boxplot of HR for all students",     # Title
        ylab="HR (beats/min)",                     # axis labels
        xlab="All students",                       # axis labels
        border="black",                            # dim the border
#        frame.plot=FALSE,                         # frame border Y/N
#        staplewex=1,                              # staple Y/N
#        whisklty=3,                               # line type for whisker
        ylim = c(40,140)                           # limits of axis
        )

# HR Results stratified by sex
#  -descriptive stats
describeBy(x=DATA$HR,group=DATA$Sex) # this does not give qunatiles

# Option 1 (output is not so easy to read)
aggregate(HR ~ Sex, data = DATA, fivenum) # "fivenum" reports the min, 1st quart., median, 3rd quart., and max (uses a diff. method from "summary")

# Option 2 (kinda clunky)
Ma <- subset(DATA, Sex == 'M')
Fe <- subset(DATA, Sex == 'F')
summary(Ma)
summary(Fe)

# histogram (one possible way)
histogram(~ HR | Sex, data = DATA)   # looks like hist and histogram are different
# so we do this the clunky way - separately for M and F
hist(x = Ma$HR,
     main="Histogram of HR for males",      # Title
     ylab="Frequency",                      # axis label
     xlab="HR (beats/min)",                 # axis label
     #     density = 20,                    # shading lines: 20 per inch
     #     angle = 20,                      # angle of the shading lines is 20 degrees
     #     border = "black",                # colour of the borders of the bars
     #     col = "black",                   # color of the shading lines. gray20 is darker tsummhan gray80
     labels = TRUE,                         # frequency labels to each bar
     xlim = c(40,140),                      # limits of axis
     ylim = c(0,20),                        # limits of axis
     breaks=seq(40,140,by=10)               # bin breaks
)

hist(x = Fe$HR,
     main="Histogram of HR for females",     # Title
     ylab="Frequency",                       # axis label
     xlab="HR (beats/min)",                  # axis label
     #     density = 20,                     # shading lines: 20 per inch
     #     angle = 20,                       # angle of the shading lines is 20 degrees
     #     border = "black",                 # colour of the borders of the bars
     #     col = "black",                    # color of the shading lines. gray20 is darker than gray80
     labels = TRUE,                          # frequency labels to each bar
     xlim = c(40,140),                       # limits of axis
     ylim = c(0,20),                         # limits of axis
     breaks=seq(40,140,by=10)                # bin breaks
)

# boxplot     
boxplot(x=Ma$HR,
        main="Boxplot of HR for males",     # Title
        ylab="HR (beats/min)",              # axis labels
        xlab="Males",                       # axis labels
        border="black",                     # dim the border
        #        frame.plot=FALSE,          # frame border Y/N
        #        staplewex=1,               # staple Y/N
        #        whisklty=3,                # line type for whisker
        ylim = c(40,140)                    # limits of axis
)

# boxplot     
boxplot(x=Fe$HR,
        main="Boxplot of HR for females",     # Title
        ylab="HR (beats/min)",                # axis labels
        xlab="Females",                       # axis labels
        border="black",                       # dim the border
        #        frame.plot=FALSE,            # frame border Y/N
        #        staplewex=1,                 # staple Y/N
        #        whisklty=3,                  # line type for whisker
        ylim = c(40,140)                      # limits of axis
)

# - Exploring if HR data are Normal using qqplots (qqnorm)
qqnorm(DATA$HR)
qqline(DATA$HR)

# 5. Analysis of Width of Right Thumb
# histogram
hist(x = DATA$WRT,
     main="Histogram of Width of Right Thumb \n for all students",     # Title
     ylab="Frequency",                                              # axis label
     xlab="WRT (mm)",                                               # axis label
     #     density = 20,                                            # shading lines: 20 per inch
     #     angle = 20,                                              # angle of the shading lines is 20 degrees
     #     border = "black",                                        # colour of the borders of the bars
     #     col = "black",                                           # color of the shading lines. gray20 is darker than gray80
     labels = TRUE,                                                 # frequency labels to each bar
     xlim = c(0,60),                                                # limits of axis
     ylim = c(0,40),                                                # limits of axis
     breaks=seq(0,60,by=5)                                          # bin breaks
)

# boxplot     
boxplot(x=DATA$WRT,
        main="Boxplot of Width of Right Thumb \n for all students",   # Title
        ylab="WRT (mm)",                                           # axis labels
        xlab="All students",                                       # axis labels
        border="black",                                            # dim the border
        #        frame.plot=FALSE,                                 # frame border Y/N
        #        staplewex=1,                                      # staple Y/N
        #        whisklty=3,                                       # line type for whisker
        ylim = c(0,60)                                             # limits of axis
)

# - Exploring if WRT data are Normal using qqplots (qqnorm)
qqnorm(DATA$WRT)
qqline(DATA$WRT)

# 6. boxplot for Height    
boxplot(x=DATA$Height,
        main="Boxplot of Height for all students",     # Title
        lim=c(-1,2),ylim=c(0,1.5),                        # axis labels
        xlab="All students",                           # axis labels
        border="black",                                # dim the border
        #        frame.plot=FALSE,                     # frame border Y/N
        #        staplewex=1,                          # staple Y/N
        #        whisklty=3,                           # line type for whisker
        ylim = c(50,84)                                # limits of axis
)

# 7.  Bar plot/histogram for Eye Color
p <- ggplot(data.frame(DATA$Ecolor), aes(x=DATA$Ecolor),
     #     density = 20,                                 # shading lines: 20 per inch
     #     angle = 20,                                   # angle of the shading lines is 20 degrees
     #     border = "black",                             # colour of the borders of the bars
     #     col = "black",                                # color of the shading lines. gray20 is darker than gray80
     labels = TRUE,                                      # frequency labels to each bar
     
) 
p + geom_bar() + ggtitle("Histogram of Eye Color for all students") + 
        ylab("Frequency") +                                 
        xlab("Eye Color")    

# 8. X-Y (scatter) plot of WRT vs Height
#plot
plot(y=DATA$WRT, x=DATA$Height,ylim=c(10,30),xlim=c(55,80),
     main="Width of Rt. Thumb versus Height",     # Title
     ylab="WRT (mm)",                                                  # axis labels
     xlab="Height (inches)",                                           # axis labels
)
