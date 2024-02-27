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

bikes = read_csv("/Users/gregwolcott/Documents/UNCW/BAN 502 - Predictive Analytics/Module 3/bike_cleaned.csv")

bike = bikes %>% mutate(dteday = mdy(dteday))
bike = bike %>% mutate_if(is.character, as_factor)
bike = bike %>% mutate(hr = as_factor(hr))
#summary(bike)

set.seed(1234)
bike_split = initial_split(bike, prop = 0.70, strata = count)
train = training(bike_split)
test = testing(bike_split)

#1
nrow(train)

#3
train_set =  recipe(count ~ season + mnth + hr + holiday + weekday  + weathersit + temp, train)

lm_model = #give the model type a name 
  linear_reg() %>% #specify that we are doing linear regression
  set_engine("lm") #specify the specify type of linear tool we want to use 

lm_wflow = 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(train_set)

lm_fit = fit(lm_wflow, train)
summary(lm_fit$fit$fit$fit)


predict_train = predict(lm_fit,new_data = train)

plot_data <- data.frame(Predicted_value = predict_train,   
                        Observed_value = train$hr) 

ggplot(plot_data, aes(x= .pred)) + 
  geom_histogram(binwidth = 50)  +
  geom_vline(aes(xintercept=mean(.pred)))

#4
lm_fit %>% predict(test) %>% bind_cols(test) %>% metrics(truth = count, estimate = .pred)




