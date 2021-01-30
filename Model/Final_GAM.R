
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
x  <- dplyr::select(df, x.names)
df <- dplyr::select(df, c("y",x.names))

# training samples
library(caret)
set.seed(1)
training.samples <- createDataPartition(y, p=0.75, list=FALSE)
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

#### FINAL MODEL -------------------------------------------------------------------------

# formula
formula.final.names <- c(
  "s(polls2020,bs='cr')",
  "s(IncomePerCap,bs='cr')",
  "s(White,bs='cr')",
  "s(Hispanic,bs='cr')",
  "s(Black,bs='cr')",
  "s(bachelor_or_more,bs='cr')",
  "s(pop_density,bs='cr')",
  "s(Construction,bs='cr')",
  "s(perc_poveri,bs='cr')"
)

formula.final.rhs = paste(formula.final.names, collapse = " + ")
formula.final <- as.formula(paste0("y ~ ", formula.final.rhs))

# number of regressors
p <- length(formula.final.names)
p

# full model
fit_full <- mgcv::gam(formula.final, data=df.train)

# diagnostic
summary(fit_full) # R-sq.(adj) =  0.79
plot(fit_full) # shows effective dof on the axis

# performances on test set
y.test.gam <- predict(fit_full, newdata=x.test)

# MSE
mse(y.test.gam, y.test) # 0.019

# train function
train_gam=function(x, y, out=NULL){
  colnames(x) <- x.names
  train_data <- data.frame(y,x)
  model_gam <- mgcv::gam(formula.final, 
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
                                   seed = 1,
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

# mean distance of y.test from the pointwise predicted value
mean(abs(y.test-PI_split[,2])) # 0.11

# mean length of the prediction interval
mean(abs(PI_split[,1]-PI_split[,3])) # 0.47

# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')
abline(h=0, col='red', lty=2)


