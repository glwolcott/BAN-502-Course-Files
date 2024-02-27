library(dplyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(lmtest)
library(glmnet)
library(ggcorrplot)
library(MASS)
library(car)
library(lubridate)
library(splines)

bikes = read_csv("/Users/gregwolcott/Documents/UNCW/BAN 502 - Predictive Analytics/Module 2/bike_cleaned.csv")

bike = bikes %>% mutate(dteday = mdy(dteday))
bike = bike %>% mutate_if(is.character, as_factor)
bike = bike %>% mutate(hr = as_factor(hr))
summary(bike)

#Pull in only numerical columns
#1
bikecorr=bike[,c(1,10,11,12,13,14,15,16)]
cor(bikecorr)

#2
ggplot(bike, aes(x=season,y=count)) + geom_point()

#3 4 5
bike_model1 = recipe(count ~ hr, bike)

lm_model = #give the model type a name 
  linear_reg() %>% #specify that we are doing linear regression
  set_engine("lm") #specify the specify type of linear tool we want to use 

lm_wflow = 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(bike_model1)

lm_fit = fit(lm_wflow, bike)
summary(lm_fit$fit$fit$fit)

#6
ggplot(bike, aes(x=temp,y=count)) + geom_point()

#7 8
bike_model2 = recipe(count ~ hr + temp, bike)

lm_model2 = #give the model type a name 
  linear_reg() %>% #specify that we are doing linear regression
  set_engine("lm") #specify the specify type of linear tool we want to use 

lm_wflow2 = 
  workflow() %>% 
  add_model(lm_model2) %>% 
  add_recipe(bike_model2)

lm_fit2 = fit(lm_wflow2, bike)
summary(lm_fit2$fit$fit$fit)

#9 10 11
bike_model3 = recipe(count ~ atemp + temp, bike)

lm_model3 = #give the model type a name 
  linear_reg() %>% #specify that we are doing linear regression
  set_engine("lm") #specify the specify type of linear tool we want to use 

lm_wflow3 = 
  workflow() %>% 
  add_model(lm_model3) %>% 
  add_recipe(bike_model3)

lm_fit3 = fit(lm_wflow3, bike)
summary(lm_fit3$fit$fit$fit)


#Durbin Watson test for residual autocorrelation
dwtest(lm_fit3$fit$fit$fit)

#test if residuals have constant variance
bike = bike %>% mutate(resid1 = lm_fit$fit$fit$fit$residuals)
ggplot(bike,aes(x=Temp,y=resid1)) + geom_point()

#Test if residuals have normal distribution
ggplot(air2,aes(x=resid1)) + geom_histogram()


#12


bike_model4 = recipe(count ~ season + mnth + hr + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, bike)

lm_model4 = #give the model type a name 
  linear_reg() %>% #specify that we are doing linear regression
  set_engine("lm") #specify the specify type of linear tool we want to use 

lm_wflow4 = 
  workflow() %>% 
  add_model(lm_model4) %>% 
  add_recipe(bike_model4)

lm_fit4 = fit(lm_wflow4, bike)
summary(lm_fit4$fit$fit$fit)






