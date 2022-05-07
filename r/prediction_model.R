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
                    control = rpart.control(cp = 0.02, minsplit=300), maxdepth = 4)

#rpart.plot(premium_tree, digits=-5, type=4, extra=1)

# forest 
premium_forest = randomForest(premium ~ brand + sneaker_name + shoe_size + buyer_region + Year_month + release_date, data=premium_train, control = rpart.control(cp = 0.001), importance=TRUE)

# variable importance measures
vi = varImpPlot(premium_forest, type=1)

# forest pd plots
partialPlot(premium_forest, premium_test, 'Year_month', las=1)
partialPlot(premium_forest, premium_test, 'shoe_size', las=1)
partialPlot(premium_forest, premium_test, 'tdtr_k', las=1)

# finished model building: compare RMSE
rmse_premium_tree = rmse(premium_tree, premium_test)
rmse_premium_forest = rmse(premium_forest, premium_test)



## ignore the rest, it's gbm




# GBM
premium_gbm = gbm(premium ~ brand + sneaker_name + shoe_size + buyer_region + Year_month + release_date, data= premium_train,
                  interaction.depth=4, n.trees=350, shrinkage=.05, cv.folds = 10, 
                  distribution='gaussian')
gbm.perf(premium_gbm)

#Define hyperparameter grid.
hyperparams <- expand.grid(n.trees = 200,
                            interaction.depth = 1,
                            shrinkage = 0.1,
                            n.minobsinnode = 10)
 
# Apply hyperparameter grid to train()
 set.seed(42)
 gbm_model <- train(premium ~ brand + sneaker_name + shoe_size + buyer_region + Year_month +release_date,
                    data = premium_train,
                    method = "gbm",
                    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                    verbose = FALSE,
                    tuneGrid = hyperparams)




