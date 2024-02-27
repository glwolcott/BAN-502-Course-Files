library(dplyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(lmtest)



airdata = airquality
airdata

#1
nrow(airdata)
#2
ncol(airdata)
#3
colSums(is.na(airdata))
air2  = airdata %>% drop_na()
#5
nrow(air2)
#6
ncol(air2)

#7 and 8
ggcorr(air2)

#9
ggplot(data = air2, mapping = aes(x = Temp, y = Ozone)) + geom_point() 

#10-12

air2_simple = recipe(Ozone ~ Temp, air2)
air2_simple


lm_model = #give the model type a name 
  linear_reg() %>% #specify that we are doing linear regression
  set_engine("lm") #specify the specify type of linear tool we want to use 


lm_wflow = 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(air2_simple)

lm_fit = fit(lm_wflow, air2)

summary(lm_fit$fit$fit$fit) 

#13
confint(lm_fit$fit$fit$fit,level = .95)

pred = 2.4391*80-147.6461
pred

test_data = data.frame(Temp = c(80,100))
predict(lm_fit,new_data = test_data)

#Durbin Watson test for residual autocorrelation
dwtest(lm_fit$fit$fit$fit)

#test if residuals have constant variance
air2 = air2 %>% mutate(resid1 = lm_fit$fit$fit$fit$residuals)
ggplot(air2,aes(x=Temp,y=resid1)) + geom_point()

#Test if residuals have normal distribution
ggplot(air2,aes(x=resid1)) + geom_histogram()





