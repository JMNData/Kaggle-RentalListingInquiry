library(RTextTools)
library(e1071)
library(data.table)
library(jsonlite)
library(lubridate)
library(purrr)
library(dplyr)
library(xlsx)
library(randomForest)

#Variables
setwd("C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\")


#################################################TRAIN DATA#############################################################
model = randomForest(interest_level~. ,data=train_data, ntree = 200,do.trace=T)

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
                        ,description=unlist(toupper(test$description)) # parse errors
                        ,features=toupper(t1$features) # parse errors
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

test.data = cbind(t2, Elevator = (grepl('ELEVATOR',test.data$features))*1)
test.data = cbind(t2, HardwoodFloors = (grepl('HARDWOOD FLOORS',test.data$features))*1)
test.data = cbind(t2, CatsAllowed = (grepl('CATS ALLOWED',test.data$features))*1)
test.data = cbind(t2, DogsAllowed = (grepl('DOGS ALLOWED',test.data$features))*1)
test.data = cbind(t2, Doorman = (grepl('DOORMAN',test.data$features))*1)
test.data = cbind(t2, Dishwasher = (grepl('DISHWASHER',test.data$features))*1)
test.data = cbind(t2, LaundryinBuilding = (grepl('LAUNDRY IN BUILDING',test.data$features))*1)
test.data = cbind(t2, NoFee = (grepl('NO FEE',test.data$features))*1)
test.data = cbind(t2, FitnessCenter = (grepl('FITNESS CENTER',test.data$features))*1)
test.data = cbind(t2, LaundryinUnit = (grepl('LAUNDRY IN UNIT',test.data$features))*1)
test.data = cbind(t2, PreWar = (grepl('PRE-WAR',test.data$features))*1)
test.data = cbind(t2, RoofDeck = (grepl('ROOF DECK',test.data$features))*1)
test.data = cbind(t2, OutdoorSpace = (grepl('OUTDOOR SPACE',test.data$features))*1)
test.data = cbind(t2, DiningRoom = (grepl('DINING ROOM',test.data$features))*1)
test.data = cbind(t2, HighSpeedInternet = (grepl('HIGH SPEED INTERNET',test.data$features))*1)
test.data = cbind(t2, balcony = (grepl('BALCONY',test.data$features))*1)
test.data = cbind(t2, SwimmingPool = (grepl('SWIMMING POOL',test.data$features))*1)
test.data = cbind(t2, NewConstruction = (grepl('NEW CONSTRUCTION',test.data$features))*1)
test.data = cbind(t2, Terrace = (grepl('TERRACE',test.data$features))*1)
test.data = cbind(t2, Exclusive = (grepl('EXCLUSIVE',test.data$features))*1)
test.data = cbind(t2, Loft = (grepl('LOFT',test.data$features))*1)
test.data = cbind(t2, GardenPatio = (grepl('GARDEN/PATIO',test.data$features))*1)
test.data = cbind(t2, WheelchairAccess = (grepl('WHEELCHAIR ACCESS',test.data$features))*1)
test.data = cbind(t2, prewar = (grepl('PREWAR',test.data$features))*1)
test.data = cbind(t2, CommonOutdoorSpace = (grepl('COMMON OUTDOOR SPACE',test.data$features))*1)
test.data = cbind(t2, HARDWOOD = (grepl('HARDWOOD',test.data$features))*1)
test.data = cbind(t2, Fireplace = (grepl('FIREPLACE',test.data$features))*1)
test.data = cbind(t2, SIMPLEX = (grepl('SIMPLEX',test.data$features))*1)
test.data = cbind(t2, LOWRISE = (grepl('LOWRISE',test.data$features))*1)
test.data = cbind(t2, Garage = (grepl('GARAGE',test.data$features))*1)
test.data = cbind(t2, LaundryRoom = (grepl('LAUNDRY ROOM',test.data$features))*1)
test.data = cbind(t2, ReducedFee = (grepl('REDUCED FEE',test.data$features))*1)
test.data = cbind(t2, Furnished = (grepl('FURNISHED',test.data$features))*1)
test.data = cbind(t2, MultiLevel = (grepl('MULTI-LEVEL',test.data$features))*1)
test.data = cbind(t2, Highceilings = (grepl('HIGH CEILINGS',test.data$features))*1)
test.data = cbind(t2, Privateoutdoorspace = (grepl('PRIVATE OUTDOOR SPACE',test.data$features))*1)
test.data = cbind(t2, PublicOutdoor = (grepl('PUBLICOUTDOOR',test.data$features))*1)
test.data = cbind(t2, ParkingSpace = (grepl('PARKING SPACE',test.data$features))*1)
test.data = cbind(t2, Roofdeck = (grepl('ROOF-DECK',test.data$features))*1)
test.data = cbind(t2, LIVEINSUPER = (grepl('LIVE IN SUPER',test.data$features))*1)
test.data = cbind(t2, Renovated = (grepl('RENOVATED',test.data$features))*1)
test.data = cbind(t2, Pool = (grepl('POOL',test.data$features))*1)
test.data = cbind(t2, Onsitelaundry = (grepl('ON-SITE LAUNDRY',test.data$features))*1)
test.data = cbind(t2, LAUNDRY = (grepl('LAUNDRY',test.data$features))*1)
test.data = cbind(t2, GreenBuilding = (grepl('GREEN BUILDING',test.data$features))*1)
test.data = cbind(t2, Storage = (grepl('STORAGE',test.data$features))*1)
test.data = cbind(t2, HighCeiling = (grepl('HIGH CEILING',test.data$features))*1)
test.data = cbind(t2, WasherinUnit = (grepl('WASHER IN UNIT',test.data$features))*1)
test.data = cbind(t2, DryerinUnit = (grepl('DRYER IN UNIT',test.data$features))*1)
test.data = cbind(t2, StainlessSteelAppliances = (grepl('STAINLESS STEEL APPLIANCES',test.data$features))*1)
test.data = cbind(t2, Concierge = (grepl('CONCIERGE',test.data$features))*1)
test.data = cbind(t2, Newlyrenovated = (grepl('NEWLY RENOVATED',test.data$features))*1)
test.data = cbind(t2, Light = (grepl('LIGHT',test.data$features))*1)
test.data = cbind(t2, OnsiteGarage = (grepl('ON-SITE GARAGE',test.data$features))*1)
test.data = cbind(t2, Patio = (grepl('PATIO',test.data$features))*1)
test.data = cbind(t2, WasherDryer = (grepl('WASHER/DRYER',test.data$features))*1)
test.data = cbind(t2, Liveinsuperintendent = (grepl('LIVE-IN SUPERINTENDENT',test.data$features))*1)
test.data = cbind(t2, GraniteKitchen = (grepl('GRANITE KITCHEN',test.data$features))*1)
test.data = cbind(t2, GymFitness = (grepl('GYM/FITNESS',test.data$features))*1)
test.data = cbind(t2, BikeRoom = (grepl('BIKE ROOM',test.data$features))*1)
test.data = cbind(t2, EXPOSEDBRICK = (grepl('EXPOSED BRICK',test.data$features))*1)
test.data = cbind(t2, MarbleBath = (grepl('MARBLE BATH',test.data$features))*1)
test.data = cbind(t2, Petsonapproval = (grepl('PETS ON APPROVAL',test.data$features))*1)
test.data = cbind(t2, WalkinCloset = (grepl('WALK IN CLOSET(S)',test.data$features))*1)
test.data = cbind(t2, Valet = (grepl('VALET',test.data$features))*1)
test.data = cbind(t2, Subway = (grepl('SUBWAY',test.data$features))*1)



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

pred = as.data.frame(cbind(test$listing_id ,predict(model, test.data, type="prob")))
  
  
fwrite(pred, "submission.csv")
