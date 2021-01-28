
library(dplyr)
library(caret)
library(mgcv)
library(ModelMetrics)
library(conformalInference)

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

# add democratic margin in 2016
df$diff.2016 <- df$percentage16_Hillary_Clinton - df$percentage16_Donald_Trump
df$bachelor_or_more <- df$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18

# regressor names
x.names <- c(
  #"diff.2016", # ?
  "polls2020",
  "total_votes20", # ?
  "Men",
  "Hispanic",                                                             
  "White",
  "Black",
  "Native",
  "Asian",
  "Income",
  "RUCC_code",
  "bachelor_or_more",
  "perc_poveri",
  "pop_density"
)

# select only the above regressors
x  <- dplyr::select(df, x.names)
df <- dplyr::select(df, c("y",x.names))

# preprocessing
x$pop_density <- log(x$pop_density)

# training samples
library(caret)
set.seed(1)
training.samples <- createDataPartition(y, p=0.8, list=FALSE)
length(training.samples)
length(y)-length(training.samples)

# train-test split
x.train  <- x[training.samples,]
x.test   <- x[-training.samples,]
y.train  <- y[training.samples]
y.test   <- y[-training.samples]
df.train <- data.frame(x.train,y=y.train)
df.test  <- data.frame(x.test, y=y.test)
n.train  <- dim(x.train)[1]
n.test   <- dim(x.test)[1]


###_____________________________________________________________________
### RANDOM FOREST ####

library(randomForest)
set.seed(1996)
rf <- randomForest(y ~ ., data=df.train)
rf # confusion matrix is based on OOB data
plot(rf)  # black: OOB error rate

###_____________________________________________________________________
### TUNING mtry (No. of variables tried at each split) with caret ####

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(1996)
tunegrid <- expand.grid(.mtry=c(1:9))
rf_gridsearch <- train(
  y~., 
  data=df.train, 
  method="rf", 
  tuneGrid=tunegrid, 
  metric="oob",
  trControl=control
)
save.image(file = "RandomForest.RData")
print(rf_gridsearch)
ggplot(rf_gridsearch)

# best performance (accuracy, kappa, ...)
rf_gridsearch$results[which(rf_gridsearch$results[,1]==rf_gridsearch$bestTune$mtry),]

# final model evaluation via OOB estimate of error rate
rf.final <- rf_gridsearch$finalModel
rf.final
plot(rf_gridsearch$finalModel)

###_____________________________________________________________________
### PREDICTION ON TEST SET

rf <- rf.final

y.pred.test <- predict(rf, df.test)

# MSE
mse(y.pred.test, y.test) # 0.01599278

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
x0 <- x.test

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

# cnt: number of y.test inside the prediction interval 
cnt <- 0
sbagliati <- list()
for(i in 1:n.test)
{
  if(y.test[i] > PI_split[i,1] & y.test[i]<PI_split[i,3])
    cnt=cnt+1
  else
    sbagliati <- c(sbagliati,i)
}

# percentage of y.test inside the prediction interval 
cnt/n.test

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.test from the pointwise predicted value
mean(abs(y.test-PI_split[,2])) # 0.02823462

# median length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.1237245


