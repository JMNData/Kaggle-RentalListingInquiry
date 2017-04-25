## fork from Brandon's script
## two features added: number of photos and number of description characters

library(RTextTools)
library(e1071)
library(data.table)
library(jsonlite)
library(h2o)
library(lubridate)
library(purrr)
library(dplyr)
library(xlsx)
library(randomForest)
library(nnet)

#Variables
h2o.init(nthreads = -1)
setwd("C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\")



#################################################TRAIN DATA#############################################################
# Load data
t1 <- fromJSON("train.json")



# There has to be a better way to do this while getting repeated rows for the "feature" and "photos" columns
t2 <- data.table(bathrooms=unlist(t1$bathrooms)
                 ,bedrooms=unlist(t1$bedrooms)
                 ,building_id=as.factor(unlist(t1$building_id))
                 ,created=as.POSIXct(unlist(t1$created))
                 ,n_photos = as.numeric(sapply(t1$photos, length))
                 ,n_description = as.numeric(sapply(t1$description, nchar))
                 ,description=unlist(t1$description) # parse errors
                 ,features=t1$features # parse errors
                 ,description2=create_matrix(t1$description, language="english",removeStopwords=TRUE, removeNumbers=TRUE,stemWords=FALSE) 
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
#t2 = cbind(t2, Elevator = (grepl('Elevator',t2$features))*1)
t2 = cbind(t2, HardwoodFloors = (grepl('Hardwood Floors',t2$features))*1)
t2 = cbind(t2, CatsAllowed = (grepl('Cats Allowed',t2$features))*1)
t2 = cbind(t2, DogsAllowed = (grepl('Dogs Allowed',t2$features))*1)
t2 = cbind(t2, Doorman = (grepl('Doorman',t2$features))*1)
t2 = cbind(t2, Dishwasher = (grepl('Dishwasher',t2$features))*1)
#t2 = cbind(t2, LaundryinBuilding = (grepl('Laundry in Building',t2$features))*1)
t2 = cbind(t2, NoFee = (grepl('No Fee',t2$features))*1)
#t2 = cbind(t2, FitnessCenter = (grepl('Fitness Center',t2$features))*1)
#t2 = cbind(t2, LaundryinUnit = (grepl('Laundry in Unit',t2$features))*1)
t2 = cbind(t2, PreWar = (grepl('Pre-War',t2$features))*1)
#t2 = cbind(t2, RoofDeck = (grepl('Roof Deck',t2$features))*1)
t2 = cbind(t2, OutdoorSpace = (grepl('Outdoor Space',t2$features))*1)
#t2 = cbind(t2, DiningRoom = (grepl('Dining Room',t2$features))*1)
#t2 = cbind(t2, HighSpeedInternet = (grepl('High Speed Internet',t2$features))*1)
#t2 = cbind(t2, balcony = (grepl('balcony',t2$features))*1)
#t2 = cbind(t2, SwimmingPool = (grepl('Swimming Pool',t2$features))*1)
#t2 = cbind(t2, NewConstruction = (grepl('New Construction',t2$features))*1)
#t2 = cbind(t2, Terrace = (grepl('Terrace',t2$features))*1)
#t2 = cbind(t2, Exclusive = (grepl('Exclusive',t2$features))*1)
#t2 = cbind(t2, Loft = (grepl('Loft',t2$features))*1)
#t2 = cbind(t2, GardenPatio = (grepl('Garden/Patio',t2$features))*1)
#t2 = cbind(t2, WheelchairAccess = (grepl('Wheelchair Access',t2$features))*1)
t2 = cbind(t2, prewar = (grepl('prewar',t2$features))*1)
#t2 = cbind(t2, CommonOutdoorSpace = (grepl('Common Outdoor Space',t2$features))*1)
t2 = cbind(t2, HARDWOOD = (grepl('HARDWOOD',t2$features))*1)
#t2 = cbind(t2, Fireplace = (grepl('Fireplace',t2$features))*1)
t2 = cbind(t2, SIMPLEX = (grepl('SIMPLEX',t2$features))*1)
#t2 = cbind(t2, LOWRISE = (grepl('LOWRISE',t2$features))*1)
t2 = cbind(t2, Garage = (grepl('Garage',t2$features))*1)
t2 = cbind(t2, LaundryRoom = (grepl('Laundry Room',t2$features))*1)
t2 = cbind(t2, ReducedFee = (grepl('Reduced Fee',t2$features))*1)
t2 = cbind(t2, Furnished = (grepl('Furnished',t2$features))*1)
#t2 = cbind(t2, MultiLevel = (grepl('Multi-Level',t2$features))*1)
#t2 = cbind(t2, Highceilings = (grepl('High ceilings',t2$features))*1)
#t2 = cbind(t2, Privateoutdoorspace = (grepl('Private outdoor space',t2$features))*1)
#t2 = cbind(t2, PublicOutdoor = (grepl('PublicOutdoor',t2$features))*1)
#t2 = cbind(t2, ParkingSpace = (grepl('Parking Space',t2$features))*1)
#t2 = cbind(t2, Roofdeck = (grepl('Roof-deck',t2$features))*1)
#t2 = cbind(t2, LIVEINSUPER = (grepl('LIVE IN SUPER',t2$features))*1)
t2 = cbind(t2, Renovated = (grepl('Renovated',t2$features))*1)
#t2 = cbind(t2, Pool = (grepl('Pool',t2$features))*1)
#t2 = cbind(t2, Onsitelaundry = (grepl('On-site laundry',t2$features))*1)
#t2 = cbind(t2, LAUNDRY = (grepl('LAUNDRY',t2$features))*1)
#t2 = cbind(t2, GreenBuilding = (grepl('Green Building',t2$features))*1)
#t2 = cbind(t2, Storage = (grepl('Storage',t2$features))*1)
#t2 = cbind(t2, HighCeiling = (grepl('High Ceiling',t2$features))*1)
#t2 = cbind(t2, WasherinUnit = (grepl('Washer in Unit',t2$features))*1)
#t2 = cbind(t2, DryerinUnit = (grepl('Dryer in Unit',t2$features))*1)
#t2 = cbind(t2, StainlessSteelAppliances = (grepl('Stainless Steel Appliances',t2$features))*1)
#t2 = cbind(t2, Concierge = (grepl('Concierge',t2$features))*1)
#t2 = cbind(t2, Newlyrenovated = (grepl('Newly renovated',t2$features))*1)
#t2 = cbind(t2, Light = (grepl('Light',t2$features))*1)
#t2 = cbind(t2, OnsiteGarage = (grepl('On-site Garage',t2$features))*1)
#t2 = cbind(t2, Patio = (grepl('Patio',t2$features))*1)
#t2 = cbind(t2, WasherDryer = (grepl('Washer/Dryer',t2$features))*1)
#t2 = cbind(t2, Liveinsuperintendent = (grepl('Live-in superintendent',t2$features))*1)
#t2 = cbind(t2, GraniteKitchen = (grepl('Granite Kitchen',t2$features))*1)
#t2 = cbind(t2, GymFitness = (grepl('Gym/Fitness',t2$features))*1)
#t2 = cbind(t2, BikeRoom = (grepl('Bike Room',t2$features))*1)
#t2 = cbind(t2, EXPOSEDBRICK = (grepl('EXPOSED BRICK',t2$features))*1)
#t2 = cbind(t2, MarbleBath = (grepl('Marble Bath',t2$features))*1)
#t2 = cbind(t2, Petsonapproval = (grepl('Pets on approval',t2$features))*1)
#t2 = cbind(t2, WalkinCloset = (grepl('Walk in Closet(s)',t2$features))*1)
#t2 = cbind(t2, Valet = (grepl('Valet',t2$features))*1)
#t2 = cbind(t2, Subway = (grepl('Subway',t2$features))*1)
t2$description = NULL
t2$features = NULL

t2[,":="(yday=yday(created)
         #,month=month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         #,hour=hour(created)
         )]

t2$created = NULL

t2.cor = t2
t2.cor$interest_level2 = ifelse(t2.cor$interest_level == "low",1,ifelse(t2.cor$interest_level == "medium", 2, 3))
t2.cor$building_id = NULL
t2.cor$created = NULL
t2.cor$description = NULL
t2.cor$features = NULL
t2.cor$manager_id = NULL
t2.cor$interest_level = NULL
t2.cor.result = cor(t2.cor,t2.cor$interest_level2)

a = data.table(cbind(ColumnFilter = rownames(t2.cor.result),t2.cor.result))
a = a[V2 > .05 | V2 < -.05,1 | ColumnFilter=="interest_level"]

cols <- list(a)
ts = data.frame(t2)



train <- as.h2o(t2, destination_frame = "train.hex")

varnames <- setdiff(colnames(train), "interest_level")

gbm1 <- h2o.gbm(x = varnames
                ,y = "interest_level"
                ,training_frame = train
                ,distribution = "multinomial"
                ,model_id = "gbm1"
                ,nfolds = 5
                ,ntrees = 200
                ,learn_rate = 0.01
                ,max_depth = 7
                ,min_rows = 20
                ,sample_rate = 0.8
                ,col_sample_rate = 0.7
                ,stopping_rounds = 5
                ,stopping_metric = "logloss"
                ,stopping_tolerance = 0
                ,seed=321
)

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

model = randomForest(interest_level~. ,data=t3.train, ntree = 100)
pred.validate = as.data.frame(predict(model, t3.validate[,-8]))
summary(pred.validate)
pred.validate = cbind (t3.validate, predict = pred.validate)
colnames(pred.validate)[which(names(pred.validate) == "predict(model, t3.validate[, -8])")] <- "predict"
result = data.table(pred.validate$interest_level, pred.validate$predict,ifelse(pred.validate$interest_level==pred.validate$predict,1,0))

matrix(result$v3,result$v3)
fwrite(result, "randomforest.csv")


h2o.auc(gbm1,xval=TRUE)
cvAUCs <- sapply(sapply(gbm1@model$cross_validation_models, `[[`, "name"), function(x) { h2o.auc(h2o.getModel(x), valid=TRUE) })
print(cvAUCs)
mean(cvAUCs)



#################################################TEST DATA#############################################################
# Load data
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


test.data2 <- as.h2o(test.data[,-"created"], destination_frame = "test.hex")
preds <- as.data.table(h2o.predict(gbm1, test.data2))

preds[,.(high, medium, low)]


testPreds <- data.table(listing_id = unlist(test$listing_id), preds[,.(high, medium, low)])
fwrite(testPreds, "submission.csv")
