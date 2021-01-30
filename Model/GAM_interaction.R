
rm(list=ls())
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
df$diff.2016 <- cut(df$diff.2016, breaks=6)
table(df$diff.2016)
df$diff.2016 <- as.numeric(df$diff.2016)

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

#-------------------------------------------------------------------------------------------

# GAM
model_gam <- mgcv::gam(y ~ 
                         s(diff.2016,bs='cr', k=6) +
                         #s(diff.2016,bs='cr', k=length(unique(df.train$diff.2016))) +
                         s(polls2020,bs='cr') +
                         s(total_votes20,bs='cr') +
                         s(Men,bs='cr') + 
                         s(Hispanic,bs='cr') +
                         s(White,bs='cr') +
                         s(Black,bs='cr') +
                         s(Native,bs='cr') +
                         s(Asian,bs='cr') +
                         s(Income,bs='cr') +
                         s(RUCC_code,bs='cr',k=length(unique(df.train$RUCC_code))) +
                         s(bachelor_or_more,bs='cr') +
                         s(perc_poveri,bs='cr') +
                         s(pop_density,bs='cr'), 
                       data=df.train)

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
                           s(diff.2016,bs='cr', k=6) +
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


