
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
x <- dplyr::select(df, x.names)

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
                       data=df.train)


# diagnostic
summary(model_gam) # R-sq.(adj) =  0.981
plot(model_gam)

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
                         data=df.train)
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
median(abs(y.test-PI_split[,2]))

# median length of the prediction interval
median(abs(PI_split[,1]-PI_split[,3])) # 0.11

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
                         data=df.train)
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
cnt/n.test # 0.873

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.test from the pointwise predicted value
median(abs(y.test-PI_split[,2]))

# median length of the prediction interval
median(abs(PI_split[,1]-PI_split[,3])) # 0.5, altissima!!

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
plot(model_gam) # shows effective dof on the axis

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
                         data=df.train)
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
cnt/n.test # 0.8978

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.test from the pointwise predicted value
median(abs(y.test-PI_split[,2]))

# median length of the prediction interval
median(abs(PI_split[,1]-PI_split[,3])) # 0.4, ancora alta!!

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
                       data=df.train)


# diagnostic
summary(model_gam) # R-sq.(adj) =  0.98
plot(model_gam) # shows effective dof on the axis

# performances on test set
y.test.gam <- predict(model_gam, newdata=df.test)

# MSE
mse(y.test.gam, y.test) # 0.019

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
                         data=df.train)
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
cnt/n.test # 0.8978

# indexes of y.test outside the prediction interval
sbagliati <- unlist(sbagliati)
length(sbagliati)

# median distance of y.test from the pointwise predicted value
median(abs(y.test-PI_split[,2]))

# median length of the prediction interval
median(abs(PI_split[,1]-PI_split[,3])) # 0.4, ancora alta!!

# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')






#### LM w/ only 2016 data ####-----------------------------------------------------

# ocio a includere 2016: pure un modello lineare fa bene!!
plot(y ~ df$diff.2016, ylim=c(-1,1))
abline(lm(y ~ df$diff.2016), col="red", lwd=3) # noi siamo interessati proprio a quelle che deviano da sto grafico

lm_2016 <- lm(y ~ df$diff.2016)
summary(lm_2016) # R-sq.(adj) =  0.961

res <- lm_2016$residuals
q <- quantile(res, probs=c(0.025,0.975))
q
suspect <- which(res>q[2] | res<q[1])
points(df$diff.2016[suspect], y[suspect], col="green", pch=19)

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








