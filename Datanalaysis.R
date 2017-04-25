library(jsonlite)
library(purrr)
library(dplyr)
library(xlsx)
setwd("C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\")

packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
json_file <- "C:\\Users\\Mike\\OneDrive\\Documents\\Kaggle\\train.json"

data <- fromJSON(json_file)
vars <- setdiff(names(data), c("photos", "features"))
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
data = data.frame(data)

data$building_id <- as.factor(data$building_id) 
data$features <- as.character(data$features)
data$photos <- as.character(data$photos)
data[1:5,]

write.csv(as.character(t1$features), file = "data4.csv", row.names = TRUE)

class(data)
