cat("\014")
remove(list = ls())


library(splines)
library(pbmcapply)
library(mgcv)
library(conformalInference)


setwd("C:/Users/edoar/Desktop/Nonparametric statistics/Project")
source("code/Independence test function.R")
source('code/Reverse percentile intervals function.R')
source('code/Permutational regression function.R')

#### DATA ----
load("data/data_last_version3/data_pools.Rdata")

attach(merged_data)

### regroup by nations

data_nations <- matrix(NA, nrow = length(factor(levels(state))), ncol = dim(merged_data)[2]-2)
i <- 0
for(nation in factor(levels(state))){
  i <- i + 1
  data_nations[i,1] <- nation
  temp_data <- merged_data[which(state==nation), 4:68]
  data_nations[i,2:66] <- colMeans(temp_data)
}

colnames(data_nations) <- c('state',names(merged_data)[4:68])

detach(merged_data)
remove("merged_data")

data_nations <- data.frame(data_nations)
for (i in 2:dim(data_nations)[2]) { # coerce to type numeric
  data_nations[,i] <- as.numeric(data_nations[,i])
}
attach(data_nations)

# target variable
y <- percentage20_Joe_Biden - percentage20_Donald_Trump

diff_2016 <- percentage16_Hillary_Clinton - percentage16_Donald_Trump

detach(data_nations)

results_indep_test <- list()


source("code/Independence test function.R")


#### ETHNICITY ----

x <- dplyr::select(data_nations, c("Hispanic", "White", "Black", "Native", "Asian"))

results_indep_test[["ethnicity"]] <- independence_perm_test(y, x)



#### EDUCATION ----

x <- dplyr::select(data_nations, c("Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18"))

aplpack::bagplot(data.frame(x,y), show.whiskers = F)

results_indep_test[["education"]] <- independence_perm_test(y, x) 
### p-value = 0


plot(data.frame(x,y))
which(x>50) # 7


fit_education <- lm(y ~. , data = data.frame(x,y))


plot(data.frame(x,y))
abline(fit_education$coefficients)

summary(fit_education) ### adj-R^2 = 0.71, pvalues significant
plot(fit_education) ### no real homoscedasticity, qqplot good
shapiro.test(fit_education$residuals) ### p = 0.89, normality ok


# without 7
x_temp <- x[-7,]
y_temp <- y[-7]
plot(data.frame(x_temp,y_temp))
fit_education <- lm(y_temp ~. , data = data.frame(x_temp,y_temp))

plot(data.frame(x_temp,y_temp))
abline(fit_education$coefficients)

summary(fit_education) # R^2 = 0.61, pvalues significants
plot(fit_education) # homoscedasticity may be better 
shapiro.test(fit_education$residuals) # p = 0.83, normality ok





#### SEX ----

x <- dplyr::select(data_nations, c("Men"))

aplpack::bagplot(data.frame(x,y), show.whiskers = F)

results_indep_test[["sex"]] <- independence_perm_test(y, x)
### p-value = 0


plot(data.frame(x,y))
which(x>250000) # 4, 7

fit_sex <- lm(y ~. , data = data.frame(x,y))


plot(data.frame(x,y))
abline(fit_sex$coefficients)

summary(fit_sex) ### adj-R^2 = 0.55, pvalues significant
plot(fit_sex) ### high leverage 4 and 7
shapiro.test(fit_sex$residuals)

# without 4 and 7
x_temp <- x[-c(4,7),]
y_temp <- y[-c(4,7)]
plot(data.frame(x_temp,y_temp))
fit_sex <- lm(y_temp ~. , data = data.frame(x_temp,y_temp))

plot(data.frame(x_temp,y_temp))
abline(fit_sex$coefficients)

summary(fit_sex) # adj-R^2 = 0.47, pvalues significants
plot(fit_sex) # no real homoscedasticity, some deviations in qqplot
shapiro.test(fit_sex$residuals) # p = 0.10


### spline

fit_sex_spline <- smooth.spline(data.frame(x, y), cv = T)
plot(data.frame(x, y))
lines(fit_sex_spline) ### it seems linear
fit_sex_spline$lambda
fit_sex_spline$df ### only 2


#### POPULATION DENSITY ----

x <- log(dplyr::select(data_nations, c("pop_density"))) ### log

bgplot <- aplpack::bagplot(data.frame(x,y), show.whiskers = F)

# look for ouliers
X <- data.frame(x,y)
indexes <- numeric(dim(bgplot$pxy.outlier)[1]) 
for (i in 1:dim(bgplot$pxy.outlier)[1]) {
  for (j in 1:dim(X)[1]) {
    if(X[j,1]==bgplot[["pxy.outlier"]][i,1]&& X[j,2]==bgplot[["pxy.outlier"]][i,2]){
      indexes[i] <- j
    }
  }
}
indexes
X[indexes,]


results_indep_test[["population density"]] <- independence_perm_test(y, x)
### p-value = 0


plot(data.frame(x,y))


fit_popdensity <- lm(y ~. , data = data.frame(x,y))


plot(data.frame(x,y))
abline(fit_popdensity$coefficients)

summary(fit_popdensity) # R^2 = 0.49, pvalues significant
plot(fit_popdensity) # homoscedasticity may be better, fat tail qq-plot
                     # high leveraging 7
shapiro.test(fit_popdensity$residuals) # p = 0.17


# without 7
x_temp <- x[-7,]
y_temp <- y[-7]
plot(data.frame(x_temp,y_temp))
fit_popdensity <- lm(y_temp ~. , data = data.frame(x_temp,y_temp))

plot(data.frame(x_temp,y_temp))
abline(fit_popdensity$coefficients)

summary(fit_popdensity) # R^2 = 0.38, pvalues significants
plot(fit_popdensity) # homoscedasticity may be better, fat tails qqplot
shapiro.test(fit_popdensity$residuals) # not really significantly normal


### spline

fit_popdensity_spline <- smooth.spline(data.frame(x, y), cv = T)
plot(data.frame(x, y))
lines(fit_popdensity_spline) ### somehow quadratic
fit_popdensity_spline$lambda
fit_popdensity_spline$df ### 3


#### INCOME ---- 

x <- dplyr::select(data_nations, c("Income"))

results_indep_test[["income"]] <- independence_perm_test(y, x)
### p-value = 0


plot(data.frame(x,y))


fit_income <- lm(y ~. , data = data.frame(x,y))


plot(data.frame(x,y))
abline(fit_income$coefficients)

summary(fit_income) # R^2 = 0.43, pvalues significant
plot(fit_income) # homoscedasticity may be better, qqplot ok
                     # high leveraging 7
shapiro.test(fit_income$residuals) # p = 0.61


# without 7
x_temp <- x[-7,]
y_temp <- y[-7]
plot(data.frame(x_temp,y_temp))
fit_income <- lm(y_temp ~. , data = data.frame(x_temp,y_temp))

plot(data.frame(x_temp,y_temp))
abline(fit_income$coefficients)

summary(fit_income) # R^2 = 0.36, pvalues significants
plot(fit_income) # homoscedasticity may be better, qqplot almost ok
shapiro.test(fit_income$residuals) # p = 0.51



#### POVERTY PERCENTAGE ----

x <- dplyr::select(data_nations, c("perc_poveri"))

bgplot <- aplpack::bagplot(data.frame(x,y), show.whiskers = F)

# look for ouliers
X <- data.frame(x,y)
indexes <- numeric(dim(bgplot$pxy.outlier)[1]) 
for (i in 1:dim(bgplot$pxy.outlier)[1]) {
  for (j in 1:dim(X)[1]) {
    if(X[j,1]==bgplot[["pxy.outlier"]][i,1]&& X[j,2]==bgplot[["pxy.outlier"]][i,2]){
      indexes[i] <- j
    }
  }
}
indexes
X[indexes,]


results_indep_test[["poverty percentage"]] <- independence_perm_test(y, x)
### p = 0.033


plot(data.frame(x,y))


fit_income <- lm(y ~. , data = data.frame(x,y))


plot(data.frame(x,y))
abline(fit_income$coefficients)

summary(fit_income) # R^2 = 0.43, pvalues significant
plot(fit_income) # not really homoscedastic, qqplot ok except for 7
shapiro.test(fit_income$residuals) # not normal!!

### bootstrap test for coefficients test
B <- 300
fitted.obs <- fitted(fit_income)
res.obs    <- residuals(fit_income)
b0.obs <- coefficients(fit_income)[1]
b1.obs <- coefficients(fit_income)[2]

T.boot.b0 <- numeric(B)
T.boot.b1 <- numeric(B)

library(pbmcapply)
pb <- progressBar(min = 0, max = B)

for(b in 1:B)
{
  response.b <- fitted.obs + sample(res.obs, replace = T)
  fm.b <- lm(response.b ~ y)
  T.boot.b0[b] <- coefficients(fm.b)[1]
  T.boot.b1[b] <- coefficients(fm.b)[2]
  setTxtProgressBar(pb, b)
}

x11()
plot(ecdf(T.boot.b0), main='Intercept')
abline(v=b0.obs, lty=2)
x11()
plot(ecdf(T.boot.b1), main='Slope', col='red')
abline(v=b1.obs, lty=2, col='red') ### !!!! 

source('code/Reverse percentile intervals function.R')

reverse_percentile_interval(b0.obs,T.boot.b0,.05) # ok
reverse_percentile_interval(b1.obs,T.boot.b1,.05) # NOPE


#### POLLS ----

x <- dplyr::select(data_nations, c("polls2020"))

aplpack::bagplot(data.frame(x,y), show.whiskers = F)

results_indep_test[["polls"]] <- independence_perm_test(y, x)
### p = 0


plot(data.frame(x,y))


fit_polls <- lm(y ~. , data = data.frame(x,y))


plot(data.frame(x,y))
abline(fit_polls$coefficients)

summary(fit_polls) # R^2 = 0.74, pvalues significant
plot(fit_polls) # not really homoscedastic, qqplot lightly fat tails
                # high leverage 7
shapiro.test(fit_polls$residuals) # normality


#### RESULTS 2016 ----

results_indep_test[["results 2016"]] <- independence_perm_test(y, diff_2016)

bgplot <- aplpack::bagplot(data.frame(diff_2016,y), show.whiskers = F)


# look for ouliers
X <- data.frame(diff_2016,y)
indexes <- numeric(dim(bgplot$pxy.outlier)[1]) 
for (i in 1:dim(bgplot$pxy.outlier)[1]) {
  for (j in 1:dim(X)[1]) {
    if(X[j,1]==bgplot[["pxy.outlier"]][i,1]&& X[j,2]==bgplot[["pxy.outlier"]][i,2]){
      indexes[i] <- j
    }
  }
}
indexes
X[indexes,] #### 6 7 39



plot(data.frame(diff_2016,y))
which(diff_2016>0.5) ### 7


fit_result2016 <- lm(y ~. , data = data.frame(diff_2016,y))


plot(data.frame(diff_2016,y))
abline(fit_result2016$coefficients)

summary(fit_result2016) # adj-R^2 = 0.96, pvalues significant
plot(fit_result2016) # no homoscedasticity, 
                     # deviance in qqplot 
                     # high leveraging 6 7

shapiro.test(fit_result2016$residuals) # normality




# without 7
x_temp <- diff_2016[-c(6,7)]
y_temp <- y[-c(6,7)]
plot(data.frame(x_temp,y_temp))
fit_result2016 <- lm(y_temp ~. , data = data.frame(x_temp,y_temp))

plot(data.frame(x_temp,y_temp))
abline(fit_result2016$coefficients)

summary(fit_result2016) # adj-R^2 = 0.97, pvalues significants
plot(fit_result2016) # no homoscedasticity 
                     # deviance in qqplot
                     # high leverage 37
shapiro.test(fit_result2016$residuals) # normality ok



### LINEAR MODEL ----

attach(data_nations)

regressors <- cbind(state,
                    Hispanic, 
                    White, 
                    Black, 
                    Native, 
                    Asian,
                    Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,
                    Men,
                    pop_density,
                    Income,
                    perc_poveri,
                    polls2020,
                    diff_2016
                    )

names <- colnames(regressors)
names[7] <- 'perc_bachelor'
colnames(regressors) <- names

regressors <- data.frame(regressors)
for (i in 2:dim(regressors)[2]) { # coerce to type numeric
  regressors[,i] <- as.numeric(regressors[,i])
}



detach(data_nations)

### training and test sample
set.seed(1997)
partition <- sample(1:50, 35)
partition <- sort(partition)
regressors_train <- regressors[partition,]
regressors_train <- regressors_train[, -5]
y_train <- y[partition]

regressors_test <- regressors[-partition,]
regressors_test <- regressors_test[, -5]
y_test <- y[-partition]


fit_complete <- lm(y_train ~. , data = data.frame(regressors_train[,2:12]))
summary(fit_complete)
plot(fit_complete) # no homoscedasticity, deviance in qq-plot
                   # high leverage 7 and 11
shapiro.test(fit_complete$residuals) # residuals not normal !!!!

source('code/Permutational regression function.R')

result_perm_test_regression <- list()

### F test
result_perm_test_regression[["F_test"]] <- permutational_F_test( x = regressors_train[,2:12], 
                                                                 y = y_train, 
                                                                 perm = 500)
View(result_perm_test_regression[["F_test"]])
# p = 0

for(i in 0:dim(as.matrix(regressors_train[,2:12]))[2]){
  result_perm_test_regression[[paste0('beta_',i)]] <- permutational_t_test(x = regressors_train[,2:12], 
                                                                  y = y_train, 
                                                                  coeff=i,
                                                                  perm = 500)
}


View(result_perm_test_regression)
### only diff_2016 significant



### Model without diff_2016


fit_complete <- lm(y_train ~. , data = data.frame(regressors_train[,2:11]))
summary(fit_complete)
plot(fit_complete) # no homoscedasticity, qq-plot quite nice
                   # high leverage 7 and 11
shapiro.test(fit_complete$residuals) # residuals normal

### perc_bachelor and perc_polls significants

### Reduced model polls2020 + perc_bachelor + total_pop

fit_reduced <- lm(y_train ~ 
                    perc_bachelor +
                    log(pop_density) +
                    polls2020, 
                  data = regressors_train)


summary(fit_reduced) # adj-R^2 0.84
plot(fit_reduced) # no homoscedasticity, fat tails qqplot
shapiro.test(fit_reduced$residuals) # p = 0.66


result_perm_test_regression <- list()



x <- dplyr::select(regressors_train, c('perc_bachelor',
                                       'pop_density',
                                       'polls2020'))


result_perm_test_regression[["F_test"]] <- permutational_F_test( x = x,
                                                                 y = y_train, 
                                                                 perm = 500)



for(i in 0:dim(as.matrix(x))[2]){
  result_perm_test_regression[[paste0('beta_',i)]] <- permutational_t_test(x = x, 
                                                                           y = y_train, 
                                                                           coeff=i,
                                                                           perm = 500)
} # we do not reject beta_2 = 0

pvalues <- c()

for(i in 0:dim(as.matrix(x))[2]){
  pvalues <- c(pvalues, result_perm_test_regression[[paste0('beta_',i)]]$p)
} 

pvalues_adj <- p.adjust(pvalues, method = 'BH')
pvalues_adj


### Reduced model perc_bachelor + polls2020

fit_reduced <- lm(y_train ~ 
                         perc_bachelor +
                         polls2020, 
                       data = regressors_train)

summary(fit_reduced)

x <- dplyr::select(regressors_train, c('perc_bachelor',
                                       'polls2020'))



result_perm_test_regression <- list()

result_perm_test_regression[["F_test"]] <- permutational_F_test( x = x,
                                                                 y = y_train, 
                                                                 perm = 500)



for(i in 0:dim(as.matrix(x))[2]){
  result_perm_test_regression[[paste0('beta_',i)]] <- permutational_t_test(x = x, 
                                                                           y = y_train, 
                                                                           coeff=i,
                                                                           perm = 500)
}
View(result_perm_test_regression)


# Test set and prediction


library(conformalInference)


newdata <- data.frame(dplyr::select(regressors_test, c('perc_bachelor',
                                                       'polls2020')))


Y <- y_train
X <- dplyr::select(regressors_train, c('perc_bachelor', 'polls2020'))


prediction.grid <- seq(range(X)[1],range(X)[2], by=100)

pred <- predict(fit_reduced, newdata=newdata, type='response', se.fit = T)


# functions

lm_train=lm.funs(intercept = T)$train.fun
lm_predict=lm.funs(intercept = T)$predict.fun

c_preds <- conformal.pred(X,
                          Y,
                          as.matrix(newdata),
                          alpha=0.05,
                          verbose=T,
                          train.fun = lm_train,
                          predict.fun = lm_predict)


View(c_preds)




PI <- cbind(c_preds$lo, c_preds$pred, c_preds$up)

colnames(PI) <- c("lower","center","upper")

View(PI)

mean(abs(PI[,3]-PI[,1]))

# MSE
mean((c_preds$pred - y_test)^2)
# 0.02531819



# cnt: number of y_test inside the prediction interval 
cnt <- 0
missclassified <- list()
for(i in 1:length(y_test))
{
  if(y_test[i] > PI[i,1] & y_test[i]<PI[i,3])
    cnt=cnt+1
  else
    missclassified <- c(missclassified,i)
}

### no obs missclassified, but intervals enormous in length 









### GAM ----


# GAM

library(mgcv)

model_gam <- mgcv::gam(y_train ~ 
                         s(perc_bachelor,bs='cr') + 
                         s(Men,bs='cr'), 
                       data = regressors_train)
      # adj_R^2 = 0.80

summary(model_gam)
plot(model_gam)

# check normality of residuals
hist(model_gam$residuals)
qqnorm(model_gam$residuals)
# normality test
shapiro.test(model_gam$residuals)


y_test_gam <- predict(model_gam, newdata=regressors_test)

# MSE
mean((y_test_gam - y_test)^2)
# 0.02100486


newdata <- data.frame(regressors_test[,6:7])

pred <- predict(model_gam, newdata=newdata, type='response', se.fit = T)


#confidence intervals
alpha <- 0.05

gdl <- model_gam$df.residual

lwr <- pred$fit-pred$se.fit*qt(1-(alpha/2),gdl)
lvl <- pred$fit
upr <- pred$fit+pred$se.fit*qt(1-(alpha/2),gdl)

PI <- cbind(lwr,lvl,upr)

names(PI) <- c("lower","center","upper")

# cnt: number of y_test inside the prediction interval 
cnt <- 0
missclassified <- list()
for(i in 1:length(y_test))
{
  if(y_test[i] > PI[i,1] & y_test[i]<PI[i,3])
    cnt=cnt+1
  else
    missclassified <- c(missclassified,i)
}






# s(Hispanic,bs='cr'),
# s(White,bs='cr') +
# s(Black,bs='cr') +
# s(Asian,bs='cr')
# s(perc_bachelor,bs='cr') +
# s(Men,bs='cr') + 
# s(pop_density,bs='cr') +
# s(Income,bs='cr') +
# s(perc_poveri,bs='cr') +
# s(polls2020,bs='cr') +
# s(diff_2016,bs='cr')