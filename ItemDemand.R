# ItemDemand.R


## LIBRARIES


library(tidyverse)
library(tidymodels)
library(patchwork)
library(timetk)
library(vroom)
library(modeltime)
library(plotly)

## READ IN DATA


train <- vroom("train.csv")
test <- vroom("test.csv")

### EDA - plot ACF

plot1 <- train %>% filter(store == 7, item == 32) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365)

plot2 <- train %>% filter(store == 1, item == 1) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 30)

plot3 <- train %>% filter(store == 7, item == 7) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 30)

plot4 <- train %>% filter(store == 5, item == 6) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 30)

plot5 <- (plot1 + plot2) / (plot3 + plot4)


# output to plot window
plot5


### pick a store and an item for exploration, building a model

# store 7, item 32
nStores <- max(train$store)
nItems <- max(train$item)

store_item <-
  train %>% filter(store == 7, item == 32)


ts_recipe <- recipe(sales~., data = store_item) %>%
  step_date(date, features = "dow") %>%
  #step_date(date, features = "decimal") %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")



knn_wf <- 
  workflow() %>%
  add_model(knn_model) %>%
  add_recipe(ts_recipe)


tuning_grid <- grid_regular(neighbors(),
                            levels = 5)



## split into folds
folds <- vfold_cv(store_item, v = 5, repeats = 1)

# run cv

CV_results <-
  knn_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape))

# find best tuning parm values

best_tune <-
  CV_results %>%
  select_best("smape")

collect_metrics(CV_results) %>%
  filter(neighbors == 10) %>%
  pull(mean)

# RF


ts_recipe <- recipe(sales~., data = store_item) %>%
  step_date(date, features = "dow") %>%
  #step_date(date, features = "decimal") %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

rf_model <- rand_forest(min_n = tune(), mtry = 3) %>%
  set_engine("ranger") %>%
  set_mode("regression")



rf_wf <- 
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(ts_recipe)


tuning_grid <- grid_regular(min_n(),
                            levels = 5)



## split into folds
folds <- vfold_cv(store_item, v = 5, repeats = 1)

# run cv

CV_results <-
  rf_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape))

# find best tuning parm values

best_tune <-
  CV_results %>%
  select_best("smape")

collect_metrics(CV_results) %>%
  filter(min_n == 30) %>%
  pull(mean)
# 16.08408 SMAPE


### EXP SMOOTHING

# filter to one store and item
store_item <-
  train %>% filter(store == 7, item == 32)

store_item_2 <-
  train %>% filter(store == 1, item == 3)

### 1st item
# setup up cv

cv_split <- time_series_split(store_item, assess="3 months", cumulative = TRUE)

# set up smoothing model
es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data = training(cv_split))


## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
cv_results %>%
modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = store_item) %>%
plot_modeltime_forecast(.interactive=TRUE)

p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
es_fullfit <- cv_results %>%
modeltime_refit(data = store_item)

es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=test, by="date") %>%
select(id, sales)

es_fullfit %>%
modeltime_forecast(h = "3 months", actual_data = store_item) %>%
plot_modeltime_forecast(.interactive=FALSE)

p2 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## 2nd item
# setup up cv

cv_split <- time_series_split(store_item_2, assess="3 months", cumulative = TRUE)

# set up smoothing model
es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data = training(cv_split))


## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=TRUE)

p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
es_fullfit <- cv_results %>%
  modeltime_refit(data = store_item_2)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = store_item_2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

p4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = store_item_2) %>%
  plot_modeltime_forecast(.interactive=FALSE)


subplot(p1,p3,p2,p4, nrows = 2)


### FOR LOOP


nStores <- max(train$store)
nItems <- max(train$item)


# main double-loop to set up store-item pairs

for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
    filter(store==s, item==i)
    storeItemTest <- test %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here
    
    ## Predict storeItem sales
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}

## SARIMA


## AR - use past to predict future
## MA - use unique part of each day to predict future
## I - differencing
## S - seasonal
## AR uses short-term correlation, MA is longer/smooth


## recipe


cv_split <- time_series_split(store_item, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(store_item_2, assess="3 months", cumulative = TRUE)

store_item_test <-
  test %>% filter(store == 7, item == 32)

store_item_test_2 <-
  test %>% filter(store == 1, item == 3)

arima_recipe <- recipe(sales~., data = store_item) %>%
  step_date(date, features = "dow") %>%
  #step_date(date, features = "decimal") %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar = 5,
                         non_seasonal_ma = 5,
                         seasonal_ar = 2,
                         seasonal_ma = 2,
                         non_seasonal_differences = 2,
                         seasonal_differences = 2) %>%
  set_engine("auto_arima")


arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))



## Visualize CV results
cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=TRUE)

p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
arima_fullfit <- cv_results %>%
  modeltime_refit(data = store_item)

arima_preds <- arima_fullfit %>%
  modeltime_forecast(new_data = testing(cv_split)) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

arima_fullfit %>%
  modeltime_forecast(new_data = store_item_test, actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=FALSE, .legend_show = FALSE)

p2 <- arima_fullfit %>%
  modeltime_forecast(new_data = store_item_test, actual_data = store_item) %>%
  plot_modeltime_forecast(.interactive=FALSE)


# refit for second combination


arima_recipe <- recipe(sales~., data = store_item_2) %>%
  step_date(date, features = "dow") %>%
  #step_date(date, features = "decimal") %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar = 5,
                         non_seasonal_ma = 5,
                         seasonal_ar = 2,
                         seasonal_ma = 2,
                         non_seasonal_differences = 2,
                         seasonal_differences = 2) %>%
  set_engine("auto_arima")


arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(cv_split2))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split2))



## Visualize CV results
cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split2),
    actual_data = store_item_2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split2),
    actual_data = store_item_2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
arima_fullfit <- cv_results %>%
  modeltime_refit(data = store_item_2)

arima_preds <- arima_fullfit %>%
  modeltime_forecast(new_data = testing(cv_split2)) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

arima_fullfit %>%
  modeltime_forecast(new_data = store_item_test_2, actual_data = store_item_2) %>%
  plot_modeltime_forecast(.interactive=FALSE, .legend_show = FALSE)

p4 <- arima_fullfit %>%
  modeltime_forecast(new_data = store_item_test_2, actual_data = store_item_2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

### create output plots


subplot(p1,p3,p2,p4, nrows = 2)

# cv, pred top row, cv, pred bottom row for 2nd group

# ma short, ar long