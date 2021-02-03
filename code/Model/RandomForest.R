
library(dplyr)
library(caret)
library(mgcv)
library(ModelMetrics)
library(conformalInference)
library(randomForest)

setwd("D:/Poli/Corsi/NPS/ProjNPS/Github_folder")

rm(list=ls())
cat("\014")

###_____________________________________________________________________
### DATA ####

load("data/data_split.Rdata")

# add democratic margin in 2016
df$diff.2016 <- df$percentage16_Hillary_Clinton - df$percentage16_Donald_Trump

# preprocessing
df$bachelor_or_more <- df$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18
df$pop_density <- log(df$pop_density)

# regressor names
x.names <- c(
  'polls2020',
  'IncomePerCap',
  'White',
  'Black',
  'Hispanic',
  'bachelor_or_more',
  'pop_density',
  'Construction',
  'perc_poveri'
)

# select only the above regressors
df <- dplyr::select(df, all_of(c("y",x.names)))

# split dataframe
df.train <- df[index.train,]
df.valid <- df[index.valid,]
df.test  <- df[index.test,]

# split covariates
x.train <- dplyr::select(df.train, all_of(x.names))
x.valid <- dplyr::select(df.valid, all_of(x.names))
x.test  <- dplyr::select(df.test,  all_of(x.names))

# split target variable
y.train <- df.train$y
y.valid <- df.valid$y
y.test  <- df.test$y

###_____________________________________________________________________
### RANDOM FOREST ####

set.seed(1996)
rf <- randomForest(y ~ ., data=df.train)

rf
plot(rf)  # black: OOB error rate

###_____________________________________________________________________
### TUNING mtry (ie number of variables tried at each split) with caret

control <- trainControl(method="repeatedcv", number=10, repeats=1, search="grid")
set.seed(1996)
tunegrid <- expand.grid(.mtry=c(2:6))
rf_gridsearch <- train(
  y~., 
  data=df.train, 
  method="rf", 
  tuneGrid=tunegrid, 
  metric="oob",
  trControl=control
)

#save.image(file = "RandomForest.RData")
print(rf_gridsearch)
ggplot(rf_gridsearch)

# best performance (accuracy, kappa, ...)
rf_gridsearch$results[which(rf_gridsearch$results[,1]==rf_gridsearch$bestTune$mtry),]

# final model evaluation via OOB estimate of error rate
rf.final <- rf_gridsearch$finalModel
rf.final
plot(rf_gridsearch$finalModel)

###_____________________________________________________________________
### PREDICTION ON VALIDATION SET

rf <- rf.final

y.pred.valid <- predict(rf, x.valid)

# MSE
mse(y.pred.valid, y.valid) # 0.01989009

###_____________________________________________________________________
### CONFORMAL PREDICTION

# train function
train_rf = function(x, y, out=NULL){
  colnames(x) <- x.names
  train_data <- data.frame(y,x)
  randomForest(y ~ ., data=train_data)
}

# predict function
predict_rf = function(obj, new_x){
  new_x = data.frame(new_x)
  colnames(new_x) = x.names
  predict(obj, new_x)
}

# points to predict
x0 <- x.valid

### SPLIT CONFORMAL
alpha <- 0.1
c_preds_split=conformal.pred.split(x=x.train,
                                   y=y.train,
                                   x0=as.matrix(x0),
                                   alpha=alpha,
                                   verbose=T,
                                   train.fun = train_rf,
                                   predict.fun = predict_rf,
                                   seed = 10,
                                   split = NULL # indices that define the data-split to be used,
                                   # Default is NULL, in which case the index is chosen randomly
                                   # default split size is 50%
)

# interval
PI_split <- cbind(c_preds_split$lo, c_preds_split$pred, c_preds_split$up)
names(PI_split) <- c("lower","center","upper")

# cnt: number of y.valid inside the prediction interval 
cnt <- 0
sbagliati <- list()
for(i in 1:n.valid)
{
  if(y.valid[i] > PI_split[i,1] & y.valid[i]<PI_split[i,3])
    cnt=cnt+1
  else
    sbagliati <- c(sbagliati,i)
}

# check percentage of y.valid inside the prediction interval 
cnt/n.valid

# indexes of y.valid outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# mean distance of y.valid from the pointwise predicted value
mean(abs(y.valid-PI_split[,2])) # 0.02823462

# mean length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.4905825


