#Discussion week 2
#by Dario Urbina Melendez 08/27/2020

#TA - Guide
#How to run, line by line press CTR+Enter


#----------------------- Indexing ---------------------------------
rm(list = ls());                # clear workspace variables
cat("\014")                     # clear console
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
months[6]
months[6] <- "June"
length(x=months);

cat("\014")
months[c(2,5,8)]
months[c(2,5,8)]<-c("FebFeb","MayMay","AugAug")

months                          #Display months
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

cat("\014")
months[months=="Jan"]
months[months!="Dec"]

#Logical indexing
sales.by.month <- c(0,100,200,50,0,0,0,0,0,0,0,0)
sales.indexing <- sales.by.month>0
sales.indexing
months[sales.indexing]           #Show only the months that correspond to the index "True"

##                              in-class exercise (1/2)
#Option 1:
sales.by.month[match("Apr", months)]

#Option 2:
sales.by.month[months==April]

##                              in-class exercise

# Randomize months 
# Randomize sales.by.month


#--------------------------Data Frame "mtcar" example-----------------------
## mtcar - Exercise, slide 12
#use subset function
##or mysubset.mtcars <- mtcars[,c(1,4)]
rm(list = ls()); 
cat("\014")
View(mtcars)
mysubset.mtcars <- mtcars[,c("mpg","hp")]        #All lines, specific columns
#mysubset.mtcars <- mtcars[2:4,c("mpg","hp")]        #Specific lines, specific columns

##   in-class exercise

solution1 <- mysubset.mtcars[mysubset.mtcars$mpg>20,]

mean(solution1[,1],na.rm=TRUE) #mean mpg = 25.47857
mean(solution1[,2],na.rm=TRUE) #mpg hp = 88.5
median(solution1[,2],na.rm=TRUE)

#Alternative solution
mysubset.mtcars2 <- subset(mtcars,mpg>20,c("mpg","hp"))  # get two colums(mpg,hp) and get all row data which have mpg higher than 20
                        #subset(x,subset,select,drop)     We are not usig drop in this case




##------------------Data Input/Output------------------------ 
rm(list = ls()); 
cat("\014")
mypet.cat <- "Oreo"
save.image("mypet.RData") # save workspace in .RData appears in Files 
rm(list = ls()); 
load("mypet.RData")



rm(list = ls()); 
cat("\014")
setwd("C:/Users/dario/Documents/Github/Teacher-A-Statististical-Methods-BME-423/Week_2_Discussion/testing")
#setwd("C:/Users/dario/Documents/Github/Teacher-A-Statististical-Methods-BME-423")
#Error if the file location is not specified, you can either setwd or define the pathname

#DUM: gives problems:
mydata<-read.csv("All_Data_2020_0827.csv",sep=",", header=TRUE) 

#DUM (a correction):
#mydata<-read.csv("All_Data_2020.csv",sep=",")

write.table(mydata, "mydata.txt", sep="\t") 



#Use pathname

rm(list = ls()); 
cat("\014")
mypath <- "C:/Users/dario/Documents/Github/Teacher-A-Statististical-Methods-BME-423/Week_2_Discussion/testing/All_Data_2020_0827.csv"
mydata <- read.csv(mypath,header=TRUE);
View(mydata)
cat("\014")
savepath <- "C:/Users/dario/Documents/Github/Teacher-A-Statististical-Methods-BME-423/Week_2_Discussion/testing/All_Data_2020_new.csv"
write.table(mydata, savepath, sep="\t") 



#----------------------- Display summary of your data--------------
mydata.colnames <- names(mydata); #Assign the colnames of mydata to the variable called "mydata.colnames"
str(mydata) #Display data type for each column
head(mydata) #Display my data's column header
tail(mydata) #Display the last few rows of mydata
tail(mydata,6) #Display the last 6 rows of mydata

#select certain columns
#Use dollar sign, Look at mean, median, mode, you will get a rough idea about distribution
mean(mydata$Height,na.rm=TRUE) #Without na.rm, it's going to cause error because row 33 is missing
median(mydata$Height,na.rm=TRUE)

#People learn best with problems
summary(mydata)
#describe(mydata) # must install & load libray("pschy"); not available for R version 4.0.2




##----- Data Manipulation - Exercises -- Objectives, cooperative, interactive, and googling skills!
#Indexing
#Indexing numeric rows
#Indexing using conditions of various types
#Example - HR <- mydata$HR
#Pull out data with condition

#Guided Tour - one condition
#How many students are right-handed?
#How many students are left-handed?
#What is the average HR, Height, WRT of male students
#What is the average HR,Height,WRT of female students

#Portion of data, use brackets
# mydata[1:5,2:3]
# mydata[,c(2,4)] #matrix last section
# mydata[,c("Height","EF")]







##-------------------------- Plotting --------------------------------

# Plotting Basics
# Line charts
graphics.off() 
cat("\014")
# Simple properties
plot(mydata$HR, type="o", col="red",ylab="HR",xlab="xlabel")
title(main="BME423 Heart Rate", col.main="red", font.main=4)


# Full properties
plot(x=1:length(x=mydata$HR),     #xdata
     y=mydata$HR,                 #ydata
     xlab="Index",                #xlabel
     ylab="HR",                   #ylabel
     type="o",                    #marker
     col="blue",                  #marker color
     cex=2,                       #marker size
     lty=2,                       #line type
     lwd=2)                       #line width

# Scatter plot
library(ggplot2)
qplot(HR,Height,data=mydata)
# geometry jitter gives some randomness on each points, 
# so that all datapoints can be shown in the plot
ggplot(mydata, aes(x=HR, y=Height))+ geom_point() # same function with ggplot
ggplot(mydata, aes(x=HR, y=Height))+ geom_line() # same function with line

plot(x=mydata$HR, 
     main="Heart rate of students",     # Title
     sub="subtitle goes here",          # Subtitle
     xlab="Index", ylab="Heart rate",   # axes labels
     font.main=3,                       # Font of title
     cex.main=2,                        # Size of title
     col.lab="red",                     # color of label
     lty = 1,                           # line type. 2=dashed
     lwd=1,                             # line width
     type="l",                          # plot type. p=point, l=line
     ylim=c(30,120),xlim=c(0,30),       # limit of axes
     xaxt="n")                          # hide x-axis


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
hist(x = mydata$HR[mydata$EyeColor=="GRN" | mydata$EyeColor=="BLU"],
     main="Heart rate of students",     # Title
     sub="subtitle goes here",          # Subtitle
     ylab="Heart rate",                 # ylabel
     xlab="bpm") 


#2. Is there any outlier in the heart rate data of the male students who are right-handed?
#If there is, what is the outlier?
#Solution
boxplot(x=mydata$HR[mydata$Handedness=="R" & mydata$Sex=="M" ],xlab="BME423",ylab="bpm",main="Heart rate")
mydata$HR[mydata$Handedness=="R" & mydata$Sex=="M" & mydata$HR>70]
# Command+shift+c in OS X to comment out multiple lines


