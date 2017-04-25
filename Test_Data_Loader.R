library(jsonlite)
library(purrr)
library(dplyr)
library(xlsx)
library(lubridate)
library(system)
library(stringr)
#system("rundll32.exe powrprof.dll,SetSuspendState 0,1,0")

#Variables
setwd("C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\")

# Train data

# Test data
test_data <- fromJSON("test.json")
vars <- setdiff(names(test_data), c("photos", "features"))
test_data <- map_at(test_data, vars, unlist) %>% tibble::as_tibble(.)

#Kill known bad data and dups
test_data$manager_id = as.factor(test_data$manager_id)
test_data$building_id = as.factor(test_data$building_id)

#DateFeatures
test_data$created = as.Date(test_data$created)
test_data$created = as.Date.POSIXct(test_data$created)
test_data = cbind(test_data, yday=yday(test_data$created))
test_data = cbind(test_data, mday=mday(test_data$created))
test_data = cbind(test_data, wday=wday(test_data$created))
test_data = cbind(test_data, hour=hour(test_data$created))
test_data = cbind(test_data, month=month(test_data$created))
test_data$created = NULL

###Features - Feature Engineering
b.t=data.frame(dummy = 'New')
for (f in newlist2){
  b.t = cbind(b.t,  n = as.factor(grepl(f,str_replace_all(toupper(test_data$features), "[[:punct:]]", " "))*1))  
}

b.t$dummy = NULL
colnames(b.t) = newlist2
test_data = cbind(test_data, b.t)
b.t = NULL


###Statistical Feature Engineering
test_data = cbind(test_data, bath_Z = (test_data$bathrooms - bathmean)/bathsd)
test_data = cbind(test_data, bed_Z = (test_data$bedrooms - bedmean)/bedsd)
test_data = cbind(test_data, price_Z = (test_data$price - pricemean)/pricesd)
test_data = (merge(x=test_data, y=manager_skill, by = 'manager_id', all.x = TRUE))

##NULL out Non-usable
test_data$features = NULL
test_data$description = NULL
test_data_listingid = test_data$listing_id
test_data$listing_id = NULL
test_data$photos = NULL
test_data$display_address = NULL
test_data$street_address = NULL



