library(dplyr)
library(GGally)
library(ggstats)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(VIM)
library(mice)
library(skimr)
library(UpSetR)
library(naniar)


grades = read_csv("/Users/gregwolcott/Documents/UNCW/BAN 502 - Predictive Analytics/Module 4/class-grades 2.csv")

summary(grades)

#1
skim(grades)


#2
gg_miss_case(grades)

#3
grades_rowdel = grades %>% drop_na() 
summary(grades_rowdel)


#4
grades_coldel = grades %>% select(where(~!any(is.na(.))))
summary(grades_coldel)

#5

set.seed(123) #sets seed for random number generator
imp_final = mice(grades, m=5, method='pmm', printFlag=FALSE)

summary(imp_final)
densityplot(imp_final, ~Final)

grades_complete = complete(imp_final) 
summary(grades_complete)