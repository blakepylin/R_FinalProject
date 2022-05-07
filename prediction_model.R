library(tidyverse)
library(lubridate)
library(rpart)
library(rpart.plot)
library(rsample) 
library(randomForest)
library(lubridate)
library(modelr)
library(gbm)
library(caret)
library(gamlr)

##Cleaning stuff
shoe_data$sale_price = as.numeric(gsub("[\\$,]", "", shoe_data$sale_price))
shoe_data$retail_price = as.numeric(gsub("[\\$,]", "", shoe_data$retail_price))

premium_data = shoe_data %>%
  mutate(premium = (sale_price - retail_price)/retail_price) %>%
  mutate_if(is.character, as.factor)

# split data into training and testing
set.seed(100)
premium_split = initial_split(premium_data, prop=0.8)
premium_train = training(premium_split)
premium_test  = testing(premium_split)

# a single tree
premium_tree = rpart(premium ~ brand + sneaker_name + shoe_size + buyer_region + time_stamp + release_date, data = premium_train,
                    control = rpart.control(cp = 0.01, minsplit=300), maxdepth = 4)

rpart.plot(premium_tree, digits=-5, type=4, extra=1)

# forest 
premium_forest = randomForest(premium ~ brand + sneaker_name + shoe_size + buyer_region + time_stamp + release_date, data=premium_train, control = rpart.control(cp = 0.00001), importance=TRUE)

rmse_premium_tree = rmse(premium_tree, premium_test)
rmse_premium_forest = rmse(premium_forest, premium_test)





