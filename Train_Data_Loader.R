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
train_data <- fromJSON("train.json")
vars <- setdiff(names(train_data), c("photos", "features"))
train_data <- map_at(train_data, vars, unlist) %>% tibble::as_tibble(.)

#Kill known bad data and dups and fix data types
train_data = unique(train_data)
train_data = train_data[which(train_data$listing_id != 7208764),]
train_data = train_data[which(train_data$listing_id != 7208794),]
train_data = train_data[which(train_data$listing_id != 7013217),]
train_data = train_data[which(train_data$listing_id != 7036279),]
train_data$interest_level = as.factor(train_data$interest_level)
train_data$manager_id = as.factor(train_data$manager_id)
train_data$building_id = as.factor(train_data$building_id)



#DateFeatures
train_data$created = as.Date(train_data$created)
train_data$created = as.Date.POSIXct(train_data$created)
train_data = cbind(train_data, yday=yday(train_data$created))
train_data = cbind(train_data, mday=mday(train_data$created))
train_data = cbind(train_data, wday=wday(train_data$created))
train_data = cbind(train_data, hour=hour(train_data$created))
train_data = cbind(train_data, month=month(train_data$created))
train_data$created = NULL

###Features - Feature Engineering
special <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
newlist = NULL
for (feature in train_data$features) {
  for (f in feature)
  {
    f=toupper(toString(f))
    f = str_replace_all(f, "[^[:alnum:]]", "")
    if(length(f) < 50){
    newlist = c(newlist, f)
      }
    
  }
}

newlist = table(newlist)
newlist = data.frame(newlist)
colnames(newlist) = c('item', 'count')
newlist$count = as.numeric(newlist$count)
newlist = newlist[which(newlist$count > 99),]
newlist2 = as.vector(newlist$item)

b=data.frame(dummy = 'New')
for (f in newlist2){
  b = cbind(b,  n = as.factor(grepl(f,str_replace_all(toupper(train_data$features), "[[:punct:]]", " "))*1))  
}

b$dummy = NULL
colnames(b) = newlist2
train_data = cbind(train_data, b)
b = NULL


###Statistical Feature Engineering
bathmean = mean(train_data$bathrooms)
bathsd = sd(train_data$bathrooms)
bedmean = mean(train_data$bedrooms)
bedsd = sd(train_data$bedrooms)
pricemean = mean(train_data$price)
pricesd = sd(train_data$price)
train_data = cbind(train_data, bath_Z = (train_data$bathrooms - bathmean)/bathsd)
train_data = cbind(train_data, bed_Z = (train_data$bedrooms - bedmean)/bedsd)
train_data = cbind(train_data, price_Z = (train_data$price - pricemean)/pricesd)

manager_skill = data.frame(manager_id = train_data$manager_id, high =ifelse(train_data$interest_level == 'high', 1,0), medium =ifelse(train_data$interest_level == 'medium', 1,0), low =ifelse(train_data$interest_level == 'low', 1,0))
manager_skill.h = aggregate(manager_skill$high, list(manager_skill$manager_id), FUN=sum)
manager_skill.m = aggregate(manager_skill$medium, list(manager_skill$manager_id), FUN=sum)
manager_skill.l = aggregate(manager_skill$low, list(manager_skill$manager_id), FUN=sum)
manager_skill = data.frame(manager_id = manager_skill.h$Group.1, high = manager_skill.h$x, medium = manager_skill.m$x, low = manager_skill.l$x)
manager_skill.h = NULL
manager_skill.m = NULL
manager_skill.l = NULL
manager_skill = cbind(manager_skill, total = manager_skill$high+manager_skill$medium+manager_skill$low)
manager_skill = cbind(manager_skill, high.frac = manager_skill$high/manager_skill$total, medium.frac = manager_skill$medium/manager_skill$total, low.frac = manager_skill$low/manager_skill$total  )
manager_skill.mean = mean((manager_skill$high.frac*2 + manager_skill$medium.frac))
manager_skill = cbind(manager_skill, skillscore = ifelse(manager_skill$total <= 20, manager_skill.mean, manager_skill$high.frac*2 + manager_skill$medium.frac))
manager_skill = data.frame(manager_id = manager_skill$manager_id, manager_skill$high.frac, manager_skill$medium.frac, manager_skill$skillscore)
train_data = (merge(train_data, manager_skill, by = 'manager_id'))

##NULL out Non-usable
train_data$features = NULL
train_data$description = NULL
train_data$listing_id = NULL
train_data$photos = NULL
train_data$display_address = NULL
train_data$street_address = NULL

##Reorder 
col_idx = grep("interest_level",names(train_data))
train_data = train_data[, c(col_idx, (1:ncol(train_data))[-col_idx])]

x = as.data.frame(lapply(train_data, class))
