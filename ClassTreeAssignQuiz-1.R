

library(tidyverse)
library(tidymodels)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(skimr)

heart = read_csv("/Users/gregwolcott/Documents/UNCW/BAN 502 - Predictive Analytics/Module 4/heart_disease.csv")

summary(heart)
skim(heart)


heart = heart %>% mutate(HeartDisease = as_factor(HeartDisease)) %>%
  mutate(HeartDisease = fct_recode(HeartDisease, "No" = "0", "Yes" = "1" )) 

heart = heart %>% mutate(Sex = as_factor(Sex))
heart = heart %>% mutate(ChestPainType = as_factor(ChestPainType))
heart = heart %>% mutate(RestingECG = as_factor(RestingECG))
heart = heart %>% mutate(ExerciseAngina = as_factor(ExerciseAngina))
heart = heart %>% mutate(ST_Slope = as_factor(ST_Slope))


#1
set.seed(12345)
heart_split = initial_split(heart, prop = 0.70, strata = HeartDisease)
train = training(heart_split)
test = testing(heart_split)

nrow(train)

#2
heart_recipe = recipe(HeartDisease ~ ., train)

tree_model = decision_tree() %>% 
  set_engine("rpart", model = TRUE) %>% #don't forget the model = TRUE flag
  set_mode("classification")

heart_wflow = 
  workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(heart_recipe)

heart_fit = fit(heart_wflow, train)

heart_fit %>%
  pull_workflow_fit() %>%
  pluck("fit")  

#extract the tree's fit from the fit object
tree = heart_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit")

#plot the tree
rpart.plot(tree)

#3
heart_fit$fit$fit$fit$cptable


#4
set.seed(123)
folds = vfold_cv(train, v = 5)

heart_recipe = recipe(HeartDisease ~ ., train) %>%
  step_dummy(all_nominal(),-all_outcomes())

tree_model = decision_tree(cost_complexity = tune()) %>% 
  set_engine("rpart", model = TRUE) %>% #don't forget the model = TRUE flag
  set_mode("classification")

tree_grid = grid_regular(cost_complexity(),
                         levels = 25) #try 25 sensible values for cp

heart_wflow = 
  workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(heart_recipe)

tree_res = 
  heart_wflow %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid
  )

tree_res

tree_res %>%
  collect_metrics() %>%
  ggplot(aes(cost_complexity, mean)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) 



#5
best_tree = tree_res %>%
  select_best("accuracy")

best_tree


#6
final_wf = 
  heart_wflow %>% 
  finalize_workflow(best_tree)

final_fit = fit(final_wf, train)

tree = final_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit")

fancyRpartPlot(tree, tweak = 1) 


#7.8 9 Not sure if this is correct
treepred = predict(final_fit, train, type = "class")
head(treepred)

confusionMatrix(treepred$.pred_class,train$HeartDisease,positive="Yes") 


#10
treepred_test = predict(final_fit, test, type = "class")
confusionMatrix(treepred_test$.pred_class,test$HeartDisease,positive="Yes")





