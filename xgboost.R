library(RTextTools)
library(data.table)
library(jsonlite)
library(lubridate)
library(purrr)
library(dplyr)
library(xgboost)
library(tidytext)



#Variables
setwd("C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\")


################################################# LOAD DATA #############################################################

# Train data
train_data <- fromJSON("train.json")
vars <- setdiff(names(train_data), c("photos", "features"))
train_data <- map_at(train_data, vars, unlist) %>% tibble::as_tibble(.)


# Test data
test_data <- fromJSON("test.json")
vars <- setdiff(names(test_data), c("photos", "features"))
test_data <- map_at(test_data, vars, unlist) %>% tibble::as_tibble(.)

################################################# ANALYZE DATA #############################################################
names(train_data)


################################################# PREPROCESSOR #############################################################

word_features = c("building_id", "created", "description", "display_address", "street_address", "features", "listing_id", "manager_id", "photos")

# Remove wordy features out of the dataset
processed_train = train_data
processed_train[word_features] = NULL

processed_test = test_data
processed_test[word_features] = NULL

# Create processed X and processed Y
train_X = processed_train
train_X$interest_level = NULL
train_y = processed_train$interest_level
train_y[train_y == "low"] = 0
train_y[train_y == "medium"] = 1
train_y[train_y == "high"] = 2

# Create processed X and processed Y
test_X = processed_test


################################################# MODEL #############################################################

set.seed(100)
pmt = proc.time()
model = xgboost(data = as.matrix(train_X), 
                label = train_y,
                eta = 0.05,
                max_depth = 10, 
                nround=1000, 
                subsample = 1,
                colsample_bytree = 0.5,
                seed = 100,
                eval_metric = "merror",
                objective = "multi:softprob",
                num_class = 3,
                missing = NaN,
                silent = 1)
show(proc.time() - pmt)

pred = predict(model,  as.matrix(test_X), missing=NaN)
pred_matrix = matrix(pred, nrow = nrow(test_data), byrow = TRUE)
pred_submission = cbind(test_data$listing_id, pred_matrix)
colnames(pred_submission) = c("listing_id", "low", "medium", "high")
pred_df = as.data.frame(pred_submission)
write.csv(pred_df, "xg_submission.csv", row.names = FALSE)
