
# Train-Validation-Test SPLIT

library(splitTools)

setwd("D:/Poli/Corsi/NPS/ProjNPS/Github_folder")

rm(list=ls())
cat("\014")

#### data ####--------------------------------------------------------

load("data/data_pools.Rdata")

df <- merged_data
names(df)

rm("merged_data")

# target variable
perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
y <- perc.joe - perc.don
df$y <- y

#### Fraudolent counties ####--------------------------------------------------------

# states that were contested by Trump
c_frodate<-c('AZ', 'GA', 'MI', 'NV', 'PA', 'WI')

# indices of counties in such states
index.fraud <- which(df$state %in% c_frodate)

# response variable in fraudolent counties
y.fraud <- y[index.fraud]

# STRATIFIED split of fraudolent counties
inds.fraud <- partition(y.fraud, p = c(train = 0.001, valid = 0.001, test = 0.998), seed = 1)

# responde variable in other counties
y.other <- y[-index.fraud]

# STRATIFIED split of other counties in train, test and validation sets
inds.other <- partition(y.other, p = c(train = 0.6, valid = 0.2, test = 0.2), seed = 1)

# add a column indicating suspected fraudolent vote
df$fraud = (df$state %in% c_frodate)+0

#### SPLIT ####------------------------------------------------------------------------

# fraudolent
df.fraud <- df[index.fraud,]
df.fraud.train <- df.fraud[inds.fraud$train,]
df.fraud.valid <- df.fraud[inds.fraud$valid,]
df.fraud.test  <- df.fraud[inds.fraud$test,]

# other
df.other <- df[-index.fraud,]
df.other.train <- df.other[inds.other$train,]
df.other.valid <- df.other[inds.other$valid,]
df.other.test  <- df.other[inds.other$test,]

# bind
df.train <- rbind(df.fraud.train, df.other.train)
df.valid <- rbind(df.fraud.valid, df.other.valid)
df.test  <- rbind(df.fraud.test,  df.other.test)

# number of observations
n.train <- nrow(df.train)
n.valid <- nrow(df.valid)
n.test  <- nrow(df.test)

# indices
index.train <- as.numeric(rownames(df.train))
index.valid <- as.numeric(rownames(df.valid))
index.test  <- as.numeric(rownames(df.test))

#### save ####------------------------------------------------------------------------

save(df,
     df.train, 
     df.valid, 
     df.test,
     index.train,
     index.valid,
     index.test,
     n.train,
     n.valid,
     n.test,
     file="data/data_split.RData")


#### check manipulations ####------------------------------------------------------

nrow(df) == n.train + n.valid + n.test

length(which(df$state %in% c_frodate)) == length(inds.fraud$test) + length(inds.fraud$valid) + length(inds.fraud$train)

length(inds.fraud$test) == length(which(df.test$state %in% c_frodate))
length(inds.fraud$valid) == length(which(df.valid$state %in% c_frodate))
length(inds.fraud$train) == length(which(df.train$state %in% c_frodate))

min((df.train$fraud==1) == (df.train$state %in% c_frodate))
min((df.valid$fraud==1) == (df.valid$state %in% c_frodate))
min((df.test$fraud==1)  == (df.test$state %in% c_frodate))

X.train <- df[index.train,]
X.valid <- df[index.valid,]
X.test  <- df[index.test,]

min(X.train==df.train)
min(X.valid==df.valid)
min(X.test==df.test)

