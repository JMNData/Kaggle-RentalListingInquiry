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

#################################################TRAIN DATA#############################################################
# Load data
t1 <- fromJSON("train.json")

# There has to be a better way to do this while getting repeated rows for the "feature" and "photos" columns
t2 <- data.table(bathrooms=unlist(t1$bathrooms)
                 ,bedrooms=unlist(t1$bedrooms)
                 #,building_id=as.factor(unlist(t1$building_id))
                 ,created=as.POSIXct(unlist(t1$created))
                 ,n_photos = as.numeric(sapply(t1$photos, length))
                 ,n_description = as.numeric(sapply(t1$description, nchar))
                 ,description=unlist(t1$description) # parse errors
                 ,features=t1$features # parse errors
                 #,description2=create_matrix(t1$description, language="english",removeStopwords=FALSE, removeNumbers=TRUE,stemWords=FALSE) 
                 # ,display_address=unlist(t1$display_address) # parse errors
                 ,latitude=unlist(t1$latitude)
                 ,longitude=unlist(t1$longitude)
                 ,listing_id=unlist(t1$listing_id)
                 ,manager_id=as.factor(unlist(t1$manager_id))
                 ,price=unlist(t1$price)
                 ,interest_level=as.factor(unlist(t1$interest_level))
                 # ,street_adress=unlist(t1$street_address) # parse errors
)


#ExpandCommon Features on > 100 Entries
t2 = cbind(t2, HardwoodFloors = (grepl('Hardwood Floors',t2$features))*1)
t2 = cbind(t2, CatsAllowed = (grepl('Cats Allowed',t2$features))*1)
t2 = cbind(t2, DogsAllowed = (grepl('Dogs Allowed',t2$features))*1)
t2 = cbind(t2, Doorman = (grepl('Doorman',t2$features))*1)
t2 = cbind(t2, Dishwasher = (grepl('Dishwasher',t2$features))*1)
t2 = cbind(t2, NoFee = (grepl('No Fee',t2$features))*1)
t2 = cbind(t2, PreWar = (grepl('Pre-War',t2$features))*1)
t2 = cbind(t2, OutdoorSpace = (grepl('Outdoor Space',t2$features))*1)
t2 = cbind(t2, prewar = (grepl('prewar',t2$features))*1)
t2 = cbind(t2, HARDWOOD = (grepl('HARDWOOD',t2$features))*1)
t2 = cbind(t2, SIMPLEX = (grepl('SIMPLEX',t2$features))*1)
t2 = cbind(t2, Garage = (grepl('Garage',t2$features))*1)
t2 = cbind(t2, LaundryRoom = (grepl('Laundry Room',t2$features))*1)
t2 = cbind(t2, ReducedFee = (grepl('Reduced Fee',t2$features))*1)
t2 = cbind(t2, Furnished = (grepl('Furnished',t2$features))*1)
t2 = cbind(t2, Renovated = (grepl('Renovated',t2$features))*1)
t2$description = NULL
t2$features = NULL
t2[,":="(yday=yday(created)
         #,month=month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         #,hour=hour(created)
)]
t2$created = NULL

t3 = t2
t3$building_id = NULL
t3$manager_id = NULL
t3$listing_id = NULL
smp_size <- floor(0.75 * nrow(t3))
set.seed(123)
train_ind <- sample(seq_len(nrow(t3)), size = smp_size)
t3.train <- t3[train_ind, ]
t3.validate <- t3[-train_ind, ]

summary(t3.validate[,-8])

colr = c("interest_level")

model = nnet(interest_level~. ,data=train_data, size=5, maxit=1000, linout=T, decay=.001)



pred.validate = predict(model, data=t3.validate[,-8], type="class")
pred.validate = data.table(t3.validate$interest_level, pred.validate)
colnames(pred.validate) = c("Train","Test")
print("Accuracy:")
print(sum(ifelse(pred.validate$Train==pred.validate$Test,1,0))/nrow(pred.validate))

test <- fromJSON("test.json")
test.data <- data.table(bathrooms=unlist(test$bathrooms)
                        ,bedrooms=unlist(test$bedrooms)
                        ,building_id=as.factor(unlist(test$building_id))
                        ,created=as.POSIXct(unlist(test$created))
                        ,n_photos = as.numeric(sapply(test$photos, length))
                        ,n_description = as.numeric(sapply(test$description, nchar))
                        ,description=unlist(test$description) # parse errors
                        ,features=t1$features # parse errors
                        #,description2=create_matrix(t1$description, language="english",removeStopwords=FALSE, removeNumbers=TRUE,stemWords=FALSE) 
                        # ,display_address=unlist(t1$display_address) # parse errors
                        ,latitude=unlist(test$latitude)
                        ,longitude=unlist(test$longitude)
                        ,listing_id=unlist(test$listing_id)
                        ,manager_id=as.factor(unlist(test$manager_id))
                        ,price=unlist(test$price)
                        #,interest_level=as.factor(unlist(test$interest_level))
                        # ,street_adress=unlist(t1$street_address) # parse errors
)

test.data = cbind(test.data, HardwoodFloors = (grepl('Hardwood Floors',test.data$features))*1)
test.data = cbind(test.data, CatsAllowed = (grepl('Cats Allowed',test.data$features))*1)
test.data = cbind(test.data, DogsAllowed = (grepl('Dogs Allowed',test.data$features))*1)
test.data = cbind(test.data, Doorman = (grepl('Doorman',test.data$features))*1)
test.data = cbind(test.data, Dishwasher = (grepl('Dishwasher',test.data$features))*1)
test.data = cbind(test.data, NoFee = (grepl('No Fee',test.data$features))*1)
test.data = cbind(test.data, PreWar = (grepl('Pre-War',test.data$features))*1)
test.data = cbind(test.data, OutdoorSpace = (grepl('Outdoor Space',test.data$features))*1)
test.data = cbind(test.data, prewar = (grepl('prewar',test.data$features))*1)
test.data = cbind(test.data, HARDWOOD = (grepl('HARDWOOD',test.data$features))*1)
test.data = cbind(test.data, SIMPLEX = (grepl('SIMPLEX',test.data$features))*1)
test.data = cbind(test.data, Garage = (grepl('Garage',test.data$features))*1)
test.data = cbind(test.data, LaundryRoom = (grepl('Laundry Room',test.data$features))*1)
test.data = cbind(test.data, ReducedFee = (grepl('Reduced Fee',test.data$features))*1)
test.data = cbind(test.data, Furnished = (grepl('Furnished',test.data$features))*1)
test.data = cbind(test.data, Renovated = (grepl('Renovated',test.data$features))*1)


test.data$description = NULL
test.data$features = NULL

test.data[,":="(yday=yday(created)
                #,month=month(created)
                ,mday=mday(created)
                ,wday=wday(created)
                #,hour=hour(created)
)]
test.data$created = NULL
test.data$building_id = NULL
test.data$manager_id = NULL
test.data$listing_id = NULL

predicted = as.data.frame(predict(model, test.data))

Pred <- data.table(listing_id = unlist(test$listing_id), predicted[,.(high, medium, low)])

pred = as.data.frame(cbind(test$listing_id ,predict(model, test.data, type="raw")))


fwrite(pred, "submission.csv")
