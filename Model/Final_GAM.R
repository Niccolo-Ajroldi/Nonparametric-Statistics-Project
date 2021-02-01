
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
summary(fit_full) # R-sq.(adj) =  0.814
plot(fit_full) # shows effective dof on the axis

# residuals
res = residuals(fit_full)
hist(res)
layout(1)
qqnorm(res, main="Normal Q-Q Plot")
qqline(res, col=col.3, lwd=2)
shapiro.test(res)

# save plot
col.3 <- "#F5A700"
png(file = "Pics/GAM_residual.png", width = 6000, height = 5000, units = "px", res = 800)
par(mfrow=c(1,2))
hist(res,
     col=col.3,
     border="black",
     prob = TRUE,
     xlab = "Residuals",
     ylim = c(0,3.5),
     main = "GAM residuals")
lines(density(res), # density plot
      lwd = 2, # thickness of line
      col = "orange3")
q <- qqnorm(res, main="Normal Q-Q Plot")
text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)-0.05,
     pos = 4, "Shapiro-Wilk test \np-value <.001", col = 1, font = 3)
qqline(res, col=col.3, lwd=2)
dev.off()


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
median(abs(PI_split[,1]-PI_split[,3])) # 0.47


# plot y.test outside prediction intervals
n.sbagliati <- length(sbagliati)
plot(1:length(y.test[sbagliati]),y.test[sbagliati],pch=20)
segments(1:n.sbagliati,PI_split[sbagliati,1], 1:n.sbagliati,PI_split[sbagliati,3],col='blue')
points(1:n.sbagliati,PI_split[sbagliati,1],cex=0.4,pch=10,col='blue')
points(1:n.sbagliati,PI_split[sbagliati,3],cex=0.4,pch=10,col='blue')
abline(h=0, col='red', lty=2)


