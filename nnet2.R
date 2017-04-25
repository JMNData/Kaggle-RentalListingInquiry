library(RTextTools)
library(data.table)
library(jsonlite)
library(lubridate)
library(purrr)
library(dplyr)
library(xlsx)
library(nnet)

#Variables
setwd("C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\")

#run list.R

model = nnet(interest_level~. ,data=train_data, size=5, maxit=500, linout=T, decay=.001)
pred.validate = predict(model, data=t3.validate[,-8], type="class")
pred.validate = data.table(t3.validate$interest_level, pred.validate)
colnames(pred.validate) = c("Train","Test")
print("Accuracy:")
print(sum(ifelse(pred.validate$Train==pred.validate$Test,1,0))/nrow(pred.validate))