
library(dplyr)
library(caret)
library(mgcv)
library(ModelMetrics)
library(conformalInference)
library(randomForest)

setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

### data -------------------------------------------------------------------------

load("data/data_split.Rdata")

# add democratic margin in 2016
df$diff.2016 <- df$percentage16_Hillary_Clinton - df$percentage16_Donald_Trump

# preprocessing
df$bachelor_or_more <- df$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18
df$pop_density <- log(df$pop_density)

# regressor names
x.names <- c(
  "lat",
  "long",
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

# performances on valid set
y.valid.lm <- predict(model_lm, newdata=df.valid)

# MSE
mse(y.valid.lm, y.valid) # 0.00335


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
                       data=df.train) 


# diagnostic
summary(model_gam)
#plot(model_gam)

# performances on valid set
y.valid.gam <- predict(model_gam, newdata=df.valid)

# MSE
mse(y.valid.gam, y.valid)

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
x0 <- x.valid

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

# percentage of y.valid inside the prediction interval 
cnt/n.valid # 0.91

# indexes of y.valid outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.valid from the pointwise predicted value
mean(abs(y.valid-PI_split[,2]))

# median length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.1277129

# plot all y.valid and prediction intervals
col <- "#6699FF"
png(file = "Full_GAM_PI_conformal.png", width = 6000, height = 5000, units = "px", res = 800)
plot(1:length(y.valid),y.valid,
     pch=20, 
     ylim = c(-1,1),
     main="Prediction intervals - Validation set", 
     xlab="Counties", 
     ylab="Democratic margin in 2020")
segments(1:n.valid,PI_split[,1], 1:n.valid,PI_split[,3], lwd=.8, col='light blue')
points(1:n.valid,PI_split[,1],cex=0.4,pch=10,col=col)
points(1:n.valid,PI_split[,3],cex=0.4,pch=10,col=col)
legend("topright",
       legend=c("Conformal PI","True values"),
       col=c(col, 1),
       pch=c(NA,20),
       lty=c(1,NA),
       lwd=c(1,NA),
       cex=0.75)
dev.off()

# plot y.valid outside prediction intervals
n.sbagliati <- length(sbagliati)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3], lwd=.8, col='light blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col=col)
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col=col)

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

# performances on valid set
y.valid.gam <- predict(model_gam, newdata=df.valid)

# MSE
mse(y.valid.gam, y.valid) # 0.029

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
x0 <- x.valid

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

# percentage of y.valid inside the prediction interval 
cnt/n.valid # 0.9028007

# indexes of y.valid outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# mean distance of y.valid from the pointwise predicted value
mean(abs(y.valid-PI_split[,2])) # 0.1239136

# median length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.5

# plot y.valid outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.valid[sbagliati]),y.valid[sbagliati],pch=20)
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
                         s(pop_density,bs='cr') +
                         s(lat,long,bs='tp'),
                       data=df.train)


# diagnostic
summary(model_gam) # R-sq.(adj) =  0.8
plot(model_gam) # shows effective dof on the axis

# performances on valid set
y.valid.gam <- predict(model_gam, newdata=df.valid)

# MSE
mse(y.valid.gam, y.valid)

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
x0 <- x.valid

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

# percentage of y.valid inside the prediction interval 
cnt/n.valid # 0.8995058

# indexes of y.valid outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# mean distance of y.valid from the pointwise predicted value
mean(abs(y.valid-PI_split[,2])) # 0.110298

# mean length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.45

# plot y.valid outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.valid[sbagliati]),y.valid[sbagliati],pch=20)
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

# performances on valid set
y.valid.gam <- predict(model_gam, newdata=df.valid)

# MSE
mse(y.valid.gam, y.valid) # 0.001786636

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
x0 <- x.valid

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

# percentage of y.valid inside the prediction interval 
cnt/n.valid # 0.9159802

# indexes of y.valid outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.valid from the pointwise predicted value
mean(abs(y.valid-PI_split[,2])) #0.02749673

# median length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.1197407

# plot y.valid outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.valid[sbagliati]),y.valid[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')

# MSE on confromal predictions
mse(PI_split[2], y.valid) # 0.150704





#### LM w/ only 2016 data ####-----------------------------------------------------

# model
lm_2016 <- lm(y ~ diff.2016, data=df.train)

# diagnostic
#plot(lm_2016)
summary(lm_2016) # R-sq.(adj) =  0.9598

# performances on valid set
y.valid.lm <- predict(lm_2016, newdata=df.valid)

# MSE
mse(y.valid.lm, y.valid) # 0.003806305

# functions
lm_train   = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun

# alpha
alpha <- 0.1

### SPLIT CONFORMAL
c_preds_split=conformal.pred.split(x.train$diff.2016,
                                   y.train,
                                   as.matrix(x.valid$diff.2016),
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

# percentage of y.valid inside the prediction interval 
cnt/n.valid # 0.9011532

# indexes of y.valid outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# mean distance of y.valid from the pointwise predicted value
mean(abs(y.valid-PI_split[,2]))

# mean length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3]))

# plot y.valid outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.valid[sbagliati]),y.valid[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')




#### GAM w/ only 2016 data ####--------------------------------------------------

# GAM
model_gam <- mgcv::gam(y ~ s(diff.2016,bs='cr'), 
                       data=df.train)

# diagnostic
summary(model_gam) # R-sq.(adj) =  0.962
plot(model_gam) # shows effective dof on the axis

# performances on valid set
y.valid.gam <- predict(model_gam, newdata=df.valid)

# MSE
mse(y.valid.gam, y.valid) # 0.003495493








