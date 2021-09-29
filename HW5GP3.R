#Load useful libraries
library(EnvStats)
library(dplyr)
library(tibble)
library(car)
library(forcats)
library(tidyverse)
library(scales)
library(mice)
library(VIM)
library(knitr)

#load housingdata.csv
housingData <- read_csv('housingData.csv')
head(housingData)

#create new variables for review
housingData <- housingData %>%
  dplyr::mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = YrSold - GarageYrBlt)

#create variable for numeric data
#housingNumeric <- housingData %>%
  #select_if(is.numeric)%>%
  #transmute(across(everything(),as.factor))

housingNumeric <- housingData %>%
  select_if(is.numeric)

#################
### Problem 1 ###
#################

#Look for missing data
is.na(housingNumeric)
sum(is.na(housingNumeric))
names(which(colSums(is.na(housingNumeric))>0))
housingNumeric <-na.omit(housingNumeric)

#---------------------------------------------------------
#Create Hold Out Validaion Set (HOVS)
HOVS <-housingNumeric[1:100,]
OLS <-housingNumeric[101:1000,]
logSP <- log(housingNumeric$SalePrice)
housingNumeric<-cbind(housingNumeric,logSP)

try1<-select(housingNumeric, c(2,3,4,5,6,7,17,40))
fit<-lm(data=try1,logSP~MSSubClass + LotFrontage + LotArea 
        + OverallQual + OverallCond + YearBuilt + GrLivArea)
summary(fit)
AIC(fit)
BIC(fit)
