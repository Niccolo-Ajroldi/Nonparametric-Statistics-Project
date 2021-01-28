
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
  "diff.2016", # ?
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


# train-test-valid split
#spec = c(train = .6, test = .2, valid = .2)
#g = sample(cut(seq(nrow(df)),nrow(df)*cumsum(c(0,spec)),labels = names(spec)))
#
## forse non ha senso perchè uso split diversi?
#x.train <- split(x, g)$train
#x.valid <- split(x, g)$valid
#x.test  <- split(x, g)$test
#
#y.train <- split(y, g)$train
#y.valid <- split(y, g)$valid
#y.test  <- split(y, g)$test
#
#df.train <- split(df, g)$train
#df.valid <- split(df, g)$valid
#df.test  <- split(df, g)$test
#
#n.train <- dim(x.train)[1]
#n.valid <- dim(x.valid)[1]
#n.test  <- dim(x.test)[1]
#n       <- nrow(x)

#### CLASSICAL LM ####-------------------------------------------------------

# linear model
model_lm <- lm(y ~ ., df.train)

# diagnostic
summary(model_lm) # Adjusted R-squared:  0.974
plot(model_lm)

# residuals
res <- model_lm$residuals
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

# performances on test set
y.test.lm <- predict(model_lm, newdata=df.test)

# MSE
mse(y.test.lm, y.test) # 0.00335


#### GAM fitting ####--------------------------------------------------------

# GAM
model_gam <- mgcv::gam(y ~ 
                         s(diff.2016,bs='cr') +
                         s(total_votes20,bs='cr') +
                         s(Men,bs='cr') + 
                         s(Hispanic,bs='cr') +
                         s(White,bs='cr') +
                         s(Black,bs='cr') +
                         s(Native,bs='cr') +
                         s(Asian,bs='cr') +
                         s(Income,bs='cr') +
                         s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                         s(bachelor_or_more,bs='cr') +
                         s(perc_poveri,bs='cr') +
                         s(pop_density,bs='cr'), 
                       data=x.train) # L'HO MODIFICATOOOOOOOOOOOOOOOOOOOOOOOO


# diagnostic
summary(model_gam) # R-sq.(adj) =  0.981
#plot(model_gam)

# performances on test set
y.test.gam <- predict(model_gam, newdata=df.test)

# MSE
mse(y.test.gam, y.test) # 0.002652

# residuals
res <- model_gam$residuals
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

# train function
train_gam=function(x, y, out=NULL){
  colnames(x) <- x.names
  train_data <- data.frame(y,x)
  model_gam <- mgcv::gam(y ~ 
                           s(diff.2016,bs='cr') +
                           s(total_votes20,bs='cr') +
                           s(Men,bs='cr') + 
                           s(Hispanic,bs='cr') +
                           s(White,bs='cr') +
                           s(Black,bs='cr') +
                           s(Native,bs='cr') +
                           s(Asian,bs='cr') +
                           s(Income,bs='cr') +
                           s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                           s(bachelor_or_more,bs='cr') +
                           s(perc_poveri,bs='cr') +
                           s(pop_density,bs='cr'), 
                         data=train_data)
}

# predict function
predict_gam=function(obj, new_x){
  new_x = data.frame(new_x)
  colnames(new_x) = x.names
  predict.gam(obj,new_x)
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
                                   train.fun = train_gam,
                                   predict.fun = predict_gam,
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
cnt/n.test # 0.91

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.test from the pointwise predicted value
mean(abs(y.test-PI_split[,2])) # 0.02823462

# median length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.1237245

# plot all y.test and prediction intervals
plot(1:length(y.test),y.test,pch=20)
segments(1:n.test,PI_split[,1], 1:n.test,PI_split[,3],col='blue')
points(1:n.test,PI_split[,1],cex=0.4,pch=10,col='blue')
points(1:n.test,PI_split[,3],cex=0.4,pch=10,col='blue')

# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')


#### GAM w/out 2016 data ####--------------------------------------------------

# da una parte a senso includere i voti 2016 perchè alla fine mi danno
# un'idea di come si vota in quel posto e perchè sono noti al momento del 2020
# d'altro canto però temo che il modello vada a overfittare sulle contee
# molto simili al 2016, prevedendo essenzialmente il voto uguale al 2016
# (dovrei provare a metetre solo il 2016 e vedere MSE e R2) e andando a 
# performare malissimo sulle contee in cui c'è stato il ribaltone.

# GAM
model_gam <- mgcv::gam(y ~ 
                         #s(diff.2016,bs='cr') +
                         s(total_votes20,bs='cr') +
                         s(Men,bs='cr') + 
                         s(Hispanic,bs='cr') +
                         s(White,bs='cr') +
                         s(Black,bs='cr') +
                         s(Native,bs='cr') +
                         s(Asian,bs='cr') +
                         s(Income,bs='cr') +
                         s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                         s(bachelor_or_more,bs='cr') +
                         s(perc_poveri,bs='cr') +
                         s(pop_density,bs='cr'), 
                       data=df.train)


# diagnostic
summary(model_gam) # R-sq.(adj) =  0.739
plot(model_gam) # shows effective dof on the axis

# performances on test set
y.test.gam <- predict(model_gam, newdata=df.test)

# MSE
mse(y.test.gam, y.test) # 0.029

# train function
train_gam=function(x, y, out=NULL){
  colnames(x) <- x.names
  train_data <- data.frame(y,x)
  model_gam <- mgcv::gam(y ~ 
                           #s(diff.2016,bs='cr') +
                           s(total_votes20,bs='cr') +
                           s(Men,bs='cr') + 
                           s(Hispanic,bs='cr') +
                           s(White,bs='cr') +
                           s(Black,bs='cr') +
                           s(Native,bs='cr') +
                           s(Asian,bs='cr') +
                           s(Income,bs='cr') +
                           s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                           s(bachelor_or_more,bs='cr') +
                           s(perc_poveri,bs='cr') +
                           s(pop_density,bs='cr'), 
                         data=train_data)
}

# predict function
predict_gam=function(obj, new_x){
  new_x = data.frame(new_x)
  colnames(new_x) = x.names
  predict.gam(obj,new_x)
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
                                   train.fun = train_gam,
                                   predict.fun = predict_gam,
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
cnt/n.test # 0.9028007

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# mean distance of y.test from the pointwise predicted value
mean(abs(y.test-PI_split[,2])) # 0.1239136

# median length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.5

# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')



#### GAM with pools ####---------------------------------------------------------

# GAM
model_gam <- mgcv::gam(y ~ 
                         s(polls2020,bs='cr') +
                         s(total_votes20,bs='cr') +
                         s(Men,bs='cr') + 
                         s(Hispanic,bs='cr') +
                         s(White,bs='cr') +
                         s(Black,bs='cr') +
                         s(Native,bs='cr') +
                         s(Asian,bs='cr') +
                         s(Income,bs='cr') +
                         s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                         s(bachelor_or_more,bs='cr') +
                         s(perc_poveri,bs='cr') +
                         s(pop_density,bs='cr'), 
                       data=df.train)


# diagnostic
summary(model_gam) # R-sq.(adj) =  0.8
#plot(model_gam) # shows effective dof on the axis

# performances on test set
y.test.gam <- predict(model_gam, newdata=df.test)

# MSE
mse(y.test.gam, y.test) # 0.019

# train function
train_gam=function(x, y, out=NULL){
  colnames(x) <- x.names
  train_data <- data.frame(y,x)
  model_gam <- mgcv::gam(y ~ 
                           s(polls2020,bs='cr') +
                           s(total_votes20,bs='cr') +
                           s(Men,bs='cr') + 
                           s(Hispanic,bs='cr') +
                           s(White,bs='cr') +
                           s(Black,bs='cr') +
                           s(Native,bs='cr') +
                           s(Asian,bs='cr') +
                           s(Income,bs='cr') +
                           s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                           s(bachelor_or_more,bs='cr') +
                           s(perc_poveri,bs='cr') +
                           s(pop_density,bs='cr'), 
                         data=train_data)
}

# predict function
predict_gam=function(obj, new_x){
  new_x = data.frame(new_x)
  colnames(new_x) = x.names
  predict.gam(obj,new_x)
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
                                   train.fun = train_gam,
                                   predict.fun = predict_gam,
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
cnt/n.test # 0.8995058

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# mean distance of y.test from the pointwise predicted value
mean(abs(y.test-PI_split[,2])) # 0.110298

# mean length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.45

# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')


#### GAM with pools and 2016 data ####---------------------------------------------------

# GAM
model_gam <- mgcv::gam(y ~ 
                         s(diff.2016,bs='cr') +
                         s(polls2020,bs='cr') +
                         s(total_votes20,bs='cr') +
                         s(Men,bs='cr') + 
                         s(Hispanic,bs='cr') +
                         s(White,bs='cr') +
                         s(Black,bs='cr') +
                         s(Native,bs='cr') +
                         s(Asian,bs='cr') +
                         s(Income,bs='cr') +
                         s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                         s(bachelor_or_more,bs='cr') +
                         s(perc_poveri,bs='cr') +
                         s(pop_density,bs='cr'), 
                       data=x.train)


# diagnostic
summary(model_gam) # R-sq.(adj) =  0.98
#plot(model_gam) # shows effective dof on the axis

# performances on test set
y.test.gam <- predict(model_gam, newdata=df.test)

# MSE
mse(y.test.gam, y.test) # 0.001786636

# train function
train_gam=function(x, y, out=NULL){
  colnames(x) <- x.names
  train_data <- data.frame(y,x)
  model_gam <- mgcv::gam(y ~ 
                           s(diff.2016,bs='cr') +
                           s(polls2020,bs='cr') +
                           s(total_votes20,bs='cr') +
                           s(Men,bs='cr') + 
                           s(Hispanic,bs='cr') +
                           s(White,bs='cr') +
                           s(Black,bs='cr') +
                           s(Native,bs='cr') +
                           s(Asian,bs='cr') +
                           s(Income,bs='cr') +
                           s(RUCC_code,bs='cr',k=length(unique(df$RUCC_code))) +
                           s(bachelor_or_more,bs='cr') +
                           s(perc_poveri,bs='cr') +
                           s(pop_density,bs='cr'), 
                         data=train_data)
}

# predict function
predict_gam=function(obj, new_x){
  new_x = data.frame(new_x)
  colnames(new_x) = x.names
  predict.gam(obj,new_x)
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
                                   train.fun = train_gam,
                                   predict.fun = predict_gam,
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
cnt/n.test # 0.9159802

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.test from the pointwise predicted value
mean(abs(y.test-PI_split[,2])) #0.02749673

# median length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.1197407

# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')

# MSE on confromal predictions
mse(PI_split[2], y.test) # 0.150704





#### LM w/ only 2016 data ####-----------------------------------------------------

# model
lm_2016 <- lm(y ~ diff.2016, data=df.train)

# diagnostic
#plot(lm_2016)
summary(lm_2016) # R-sq.(adj) =  0.9598

# performances on test set
y.test.lm <- predict(lm_2016, newdata=df.test)

# MSE
mse(y.test.lm, y.test) # 0.003564515

# functions
lm_train   = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun

# alpha
alpha <- 0.1

### SPLIT CONFORMAL
c_preds_split=conformal.pred.split(x$diff.2016,
                                   y,
                                   as.matrix(x.test$diff.2016),
                                   alpha=alpha,
                                   verbose=T,
                                   train.fun = lm_train,
                                   predict.fun = lm_predict,
                                   seed = 1,
                                   split=NULL # indices that define the data-split to be used,
                                   # Default is NULL, in which case the index is chosen randomly
                                   # default split size is 50%
                                   # split=sample(1:n, size=m)
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
cnt/n.test # 0.9011532

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# mean distance of y.test from the pointwise predicted value
mean(abs(y.test-PI_split[,2])) # 0.03891312

# mean length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.1610294

# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')

# MSE on confromal predictions
mse(PI_split[2], y.test) # 0.1343345




#### GAM w/ only 2016 data ####--------------------------------------------------

# GAM
model_gam <- mgcv::gam(y ~ s(diff.2016,bs='cr'), 
                       data=df.train)

# diagnostic
summary(model_gam) # R-sq.(adj) =  0.962
plot(model_gam) # shows effective dof on the axis

# performances on test set
y.test.gam <- predict(model_gam, newdata=df.test)

# MSE
mse(y.test.gam, y.test) # 0.003495493








