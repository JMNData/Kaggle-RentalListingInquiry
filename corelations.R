library(Hmisc)
train_data.cor = train_data
train_data.cor$manager_id = NULL
train_data.cor$building_id = NULL
train_data.cor = cbind(train_data.cor, interest_level2 = ifelse(train_data.cor$interest_level=='low',0,ifelse(train_data.cor$interest_level=='medium',1,2)))
train_data.cor$interest_level = NULL
summary(train_data.cor)
cor(train_data.cor, use="complete.obs", method="kendall") 
cor(train_data.cor$interest_level2,train_data.cor[,-c['interest_level2']])

# First Correlogram Example
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order") 