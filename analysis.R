test.data <- data.table(bathrooms=unlist(t1$bathrooms)
                 ,bedrooms=unlist(t1$bedrooms)
                 ,building_id=as.factor(unlist(t1$building_id))
                 ,created=as.POSIXct(unlist(t1$created))
                 ,n_photos = as.numeric(sapply(t1$photos, length))
                 ,n_description = as.numeric(sapply(t1$description, nchar))
                 ,description=unlist(t1$description) # parse errors
                 ,features=t1$features # parse errors
                 ,latitude=unlist(t1$latitude)
                 ,longitude=unlist(t1$longitude)
                 ,listing_id=unlist(t1$listing_id)
                 ,manager_id=as.factor(unlist(t1$manager_id))
                 ,price=unlist(t1$price)
                 ,interest_level=as.factor(unlist(t1$interest_level))
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