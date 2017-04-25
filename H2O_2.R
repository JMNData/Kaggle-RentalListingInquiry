#H2O Analysis - 2.0 - Enhanced Tuning 
library(RTextTools)
library(e1071)
library(data.table)
library(jsonlite)
library(h2o)
library(lubridate)
library(purrr)
library(dplyr)
library(xlsx)
install.packages('h2oEnsemble')


################################################# LOAD VARIABLES #############################################################

h2o.init(ip = "localhost", port = 54321, startH2O = FALSE)
setwd("C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\")
h2o.removeAll() 


################################################# LOAD TRAINING DATA #############################################################
#list.r
train.h20 <- as.h2o(train_data, destination_frame = "train.hex")
# splits = h2o.splitFrame(train.h20, c(.6,.2), seed=12345)
# train = h2o.assign(splits[[1]], "train.hex")
# train.v = h2o.assign(splits[[2]], "valid.hex")
# train.t <- h2o.assign(splits[[3]], "test.hex") 
varnames <- setdiff(colnames(train.h20), "interest_level")

model <- h20.deeplearning(x = 2:83,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame = train.h20, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   classification = 1,
                   balance_classes = FALSE, 
                   hidden = c(50,50,50), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs  

model2 <- h2o.gbm(x = varnames
                ,y = "interest_level"
                ,training_frame = train.h20
                ,distribution = "multinomial"
                ,model_id = "gbm1"
                ,nfolds = 5
                ,ntrees = 100
                ,learn_rate = 0.01
                ,max_depth = 5
                ,min_rows = 20
                ,sample_rate = 0.8
                ,col_sample_rate = 0.7
                ,stopping_rounds = 5
                ,stopping_metric = "logloss"
                ,stopping_tolerance = 0
                ,seed=321
)

set = model2@allparameters$x


model4 <- h2o.gbm(x = set
                  ,y = "interest_level"
                  ,training_frame = train.h20
                  ,distribution = "AUTO"
                  ,model_id = "gbm1"
                  ,nfolds = 5
                  ,ntrees = 135
                  ,learn_rate = 0.02
                  ,max_depth = 7
                  ,min_rows = 20
                  ,sample_rate = 0.8
                  ,col_sample_rate = 0.7
                  ,stopping_rounds = 4
                  ,stopping_metric = "logloss"
                  ,stopping_tolerance = 0.001
                  ,seed=321
)
model4.result = summary(model4)
model4.result

model5 <- h2o.gbm(x = set
                  ,y = "interest_level"
                  ,training_frame = train.h20
                  ,distribution = "AUTO"
                  ,model_id = "gbm1"
                  ,nfolds = 5
                  ,ntrees = 400
                  ,learn_rate = 0.02
                  ,max_depth = 7
                  ,min_rows = 20
                  ,sample_rate = 0.75
                  ,col_sample_rate = 0.7
                  ,stopping_rounds = 4
                  ,stopping_metric = "logloss"
                  ,stopping_tolerance = 0.001
                  ,seed=321
)
#200 produced .48 Logloss, .40 RMSE
#300 produced .45 Logloss, .39 RMSE

model5 <- h2o.gbm(x = set
                  ,y = "interest_level"
                  ,training_frame = train.h20
                  ,distribution = "AUTO"
                  ,model_id = "gbm1"
                  ,nfolds = 0
                  ,ntrees = 2000
                  ,learn_rate = 0.025
                  ,max_depth = 7
                  ,min_rows = 20
                  ,sample_rate = 0.75
                  ,col_sample_rate = 0.7
                  ,stopping_rounds = 4
                  ,stopping_metric = "logloss"
                  ,stopping_tolerance = 0.001
                  ,seed=321
)

learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"
family <- "multinominal"
h2o.removeAll()

model6 = h2o::(x = set, y = "interest_level", 
                    training_frame = train.h20, 
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))



#300 produced .435 LogLoss and .38 RMSE
#500 produced .39 LogLoss and .36 RMSE
#500 produced .33 LogLoss and .33 RMSE

test_data.h20 <- as.h2o(test_data, destination_frame = "test.hex")
preds <- as.data.table(h2o.predict(model5, test_data.h20))
preds[,.(high, medium, low)]
testPreds <- data.table(listing_id = unlist(test_data_listingid), preds[,.(high, medium, low)])
fwrite(testPreds, "submission_test_h20_500nTree_model5_Var.csv")

