# Script for Analysis of BME 423 Student Data

## 1. Mandatory lines
# Similiar thing as clc;clear all;close all in MATLAB

rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots


DIR<-"C:/Users/dario/Documents/Github/Teacher-A-Statististical-Methods-BME-423/Week_1_Discussion";
setwd(DIR); # change working directory

X<-c(2,2,2,2); # Vector [2,2,2,2]
Y<-matrix(,3,2); # 3(row) by 2(column) matrix -no values (NA)
Y_trans<-t(Y) # transpose
ncol(Y)# number of columns in a matrix
nrow(Y) # the number of rows
M<-matrix(1:6,3,2); # assign 1~6 into the elements of the matrix

#[rows, columns]
M[2,2]# Find the chosen indexed value in the matrix
M[2,2]<-44 #Assigning an specific value
M[1:2,]# Find several elements from the chosen indices (first and second element, all columns)
M[1:2,1]#  (first and second element, first column)
M[1:2,2]#  (first and second elements from second column)
M[,2] #  (all elements from second column)

M[M>5]# Find elements of M that are larger than 5

#Data frames
D<-as.data.frame(M)# convert it to dataframe
colnames(D)<-c("COL1","COL2")# Define column names 
D #See how column names have changed!
colnames(D)# column names 
names(D)# column names

rownames(D)# row names

D$COL1# look at one field/column

mypeople<-c("Bob","Joanne","Sally","Tim","Neal")# string array
typeof(D)
mean(D$COL1)

#Extra........................................................................
#DUM: Data array allows to combine vectors of same length but different types
# Can combine vectors of the same length

vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)

dfa <- cbind(vNumeric, vCharacter, vLogical)
dfa  # Matrix of one data type 
#DUM: takes the one that is has majority
#.............................................................................




## 2. Load library
#install.packages("psych")
#install.packages("car")
#install.packages("dplyr")
# install.packages("lsr")
# install.packages("ggplot2")
# install.packages("vioplot")

library(psych) # function describeBy
library(lsr)
library(survival)
library(ggplot2)
library(car) # function scatterplot
library(lattice) # function histogram
library(vioplot)
## 3. Student data analysis
# Heart rate/Height/Sex/Side of room/Exercise frequency/Handedness/WidthRightThumb
savehistory('test_2.txt')