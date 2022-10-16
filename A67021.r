

# 0. packages
library(tidyverse)
library(tidymodels)
library(dlookr)
library(naniar)
library(palmerpenguins)

# 1. Loading  DATA
#. palmerpenguins package의 penguins 데이터를 분석 예제에 사용함

dt <- penguins
head(dt, 10)

# 2. Overview data using dlookr packages
## overview

summary(overview(dt))

dt %>%
  diagnose_numeric() 
dt %>%
  diagnose_category()
## check NA
naniar::gg_miss_upset(dt)
dlookr::plot_na_pareto(dt)

#. NA가 관측치의 5% 이내로 보이므로 삭제하여 처리하겟습니다.
dt_all <- dt %>% na.omit()
# data for model
## recipe
dt_recipe <- 
    recipe(sex ~ .,
           data = dt_all) %>% 
    step_mutate(year = factor(year)) %>% 
    prep(training = dt_train)
dt_recipe

## bake dt_train
dt_juiced <- juice(dt_recipe)
## split data
set.seed(123)
split <- initial_split(dt_juiced, strata = sex)
dt_train <- training(split)
dt_test <- testing(split)


# model
## 1. knn
library(kknn)
#. make a knn spec
knn_spec <- nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

#. fit knn nodel
knn_fit <- knn_spec %>% 
  fit(sex ~., data = dt_juiced)
knn_fit

#. predict
knn_fit %>% 
  predict(dt_test) %>% 
  bind_cols(dt_test) %>%
  conf_mat(truth = sex, estimate = .pred_class)


## 2. Decision Tree
#. make a Decision Tree spec
dctree_spec <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

#. fit Decision Tree nodel
dctree_fit <- dctree_spec %>% 
  fit(sex ~., data = dt_juiced)
dctree_fit

#. predict
dctree_fit %>% 
  predict(dt_test) %>% 
  bind_cols(dt_test) %>%
  conf_mat(truth = sex, estimate = .pred_class)

## 3. ANN(Artificial Neural Network)
#. make a mlp spec
mlp_spec <- mlp() %>% 
  set_engine("nnet") %>% 
  set_mode("classification")

#. fit mlp nodel
mlp_fit <- mlp_spec %>% 
  fit(sex ~., data = dt_juiced)
mlp_fit

#. predict
mlp_fit %>% 
  predict(dt_test) %>% 
  bind_cols(dt_test) %>%
  conf_mat(truth = sex, estimate = .pred_class)

## 4. SVM(Support Vector Machine)
library(kernlab)
#. make a SVM spec
svm_spec <- svm_linear() %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

#. fit SVM nodel
svm_fit <- svm_spec %>% 
  fit(sex ~., data = dt_juiced)
svm_fit

#. predict
svm_fit %>% 
  predict(dt_test) %>% 
  bind_cols(dt_test) %>%
  conf_mat(truth = sex, estimate = .pred_class)

## 5.Random Forest

library(randomForest)
#. make a randomForest spec
randomForest_spec <- rand_forest() %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")

#.fit randomForest nodel
randomForest_fit <- randomForest_spec %>% 
  fit(sex ~., data = dt_juiced)
randomForest_fit

#. predict
randomForest_fit %>% 
  predict(dt_test) %>% 
  bind_cols(dt_test) %>%
  conf_mat(truth = sex, estimate = .pred_class)


## 6.Naive Beyes
library(klaR)
#. make a Naive Beyes spec
nb_spec <- naive_Bayes() %>% 
  set_engine("klaR") %>% 
  set_mode("classification")

#. fit Naive Beyes nodel
nb_fit <- nb_spec %>% 
  fit(sex ~., data = dt_juiced)
nb_fit

#. predict
nb_fit %>% 
  predict(dt_test) %>% 
  bind_cols(dt_test) %>%
  conf_mat(truth = sex, estimate = .pred_class)





























