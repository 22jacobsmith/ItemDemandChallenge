# ItemDemand.R

library(tidyverse)
library(tidymodels)
library(patchwork)
library(timetk)
library(vroom)

train <- vroom("train.csv")
test <- vroom("test.csv")


plot1 <- train %>% filter(store == 5, item == 1) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365)

plot2 <- train %>% filter(store == 2, item == 10) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365)

plot3 <- train %>% filter(store == 3, item == 7) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365)

plot4 <- train %>% filter(store == 10, item == 5) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365)

plot5 <- (plot1 + plot2) / (plot3 + plot4)

plot5

nStores <- max(train$store)
nItems <- max(train$item)
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
