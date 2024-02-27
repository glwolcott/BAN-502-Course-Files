library(dplyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(lmtest)
library(glmnet)
library(ggcorrplot)
library(e1071)
library(ROCR)


parole = read_csv("/Users/gregwolcott/Documents/UNCW/BAN 502 - Predictive Analytics/Module 3/parole 2.csv")

#male
parole = parole %>% mutate(male = as_factor(male)) %>%
  mutate(male = fct_recode(male, "Female" = "0", "Male" = "1" )) 

#race
parole= parole %>% mutate(race = as_factor(race)) %>%
  mutate(race = fct_recode(race, "Non-White" = "2", "White" = "1" ))

#state
parole= parole %>% mutate(state = as_factor(state)) %>%
  mutate(state = fct_recode(state, "Kentucky" = "2", "Louisiana" = "3", "Virginia" = "4", "Other" = "1" ))

#crime
parole= parole %>% mutate(crime = as_factor(crime)) %>%
  mutate(crime = fct_recode(crime, "Larceny" = "2", "Drugs" = "3", "Driving" = "4", "Other" = "1" ))

#multiple.offenses
parole= parole %>% mutate(multiple.offenses = as_factor(multiple.offenses)) %>%
  mutate(multiple.offenses = fct_recode(multiple.offenses, "Not Multiple" = "0", "Multiple" = "1" ))

#violator
parole= parole %>% mutate(violator = as_factor(violator)) %>%
  mutate(violator = fct_recode(violator, "No Violation" = "0", "Violation" = "1" ))


#1
summary(parole)


#2
set.seed(12345)
parole_split = initial_split(parole, prop = 0.70, strata = violator)
train = training(parole_split)
test = testing(parole_split)

train = train %>% mutate(violator = fct_relevel(violator, c("No Violation","Violation")))
levels(train$violator)
nrow(train)
#3

t1 = table(train$violator, train$male) #create a table object
prop.table(t1, margin = 2 ) #crosstab with proportions

#4
t2 = table(train$violator, train$state) #create a table object
prop.table(t2, margin = 2 ) #crosstab with proportions

#5
t3 = table(train$violator, train$max.sentence) #create a table object
prop.table(t3, margin = 2 ) #crosstab with proportions

ggplot(train, aes(x=max.sentence, fill = violator)) + geom_bar() + theme_bw()

#6 and 7
violator_model = 
  logistic_reg() %>% #note the use of logistic_reg
  set_engine("glm") #standard logistic regression engine is glm

violator_recipe = recipe(violator ~ state, train) %>%
  step_dummy(all_nominal(), -all_outcomes()) #exclude the response variable from being dummy converted  

logreg_wf = workflow() %>%
  add_recipe(violator_recipe) %>% 
  add_model(violator_model)

violator_fit = fit(logreg_wf, train)
summary(violator_fit$fit$fit$fit)


#8
violator_model2 = 
  logistic_reg() %>% #note the use of logistic_reg
  set_engine("glm") #standard logistic regression engine is glm

violator_recipe2 = recipe(violator ~ state + multiple.offenses + race , train) %>%
  step_dummy(all_nominal(), -all_outcomes()) #exclude the response variable from being dummy converted  

logreg_wf2 = workflow() %>%
  add_recipe(violator_recipe2) %>% 
  add_model(violator_model2)

violator_fit2 = fit(logreg_wf2, train)
summary(violator_fit2$fit$fit$fit)

#9 The parolee is in Louisiana, has multiple offenses, and is white.
newdata = data.frame(state = "Louisiana", multiple.offenses = "Multiple", race = "White")
predict(violator_fit2, newdata, type="prob")


#10
predictions = predict(violator_fit2, train, type="prob")[2]


ROCRpred = prediction(predictions, train$violator) 

###You shouldn't need to ever change the next two lines:
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(ROCRperf, ROCRpred))


t1 = table(train$violator,predictions > 0.2015788)
(t1[1,1]+t1[2,2])/nrow(train)
t1
36/(18+36)
