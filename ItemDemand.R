# ItemDemand.R


## LIBRARIES


library(tidyverse)
library(tidymodels)
library(patchwork)
library(timetk)
library(vroom)


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
