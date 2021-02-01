
# Train-Validation-Test SPLIT

library(splitTools)

setwd("D:/Poli/Corsi/NPS/ProjNPS")

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

# STRATIFIED split of fraudolent counties in train, test and validation sets
inds.fraud <- partition(y.fraud, p = c(train = 1/3, valid = 1/3, test = 1/3), seed = 1)

# responde variable in other counties
y.other <- y[-index.fraud]

# STRATIFIED split of other counties in train, test and validation sets
inds.other <- partition(y.other, p = c(train = 0.6, valid = 0.2, test = 0.2), seed = 1)


#### SPLIT ####------------------------------------------------------------------------

# indices of the three sets, containing both fraudolent and other counties
index.train <- c(inds.fraud$train, inds.other$train)
index.valid <- c(inds.fraud$valid, inds.other$valid)
index.test  <- c(inds.fraud$test,  inds.other$test)

# split dataframe
df.train <- df[index.train,]
df.valid <- df[index.valid,]
df.test  <- df[index.test,]

# number of observations
n.train <- nrow(df.train)
n.valid <- nrow(df.valid)
n.test  <- nrow(df.test)

# check
nrow(df) == n.train + n.valid + n.test


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



