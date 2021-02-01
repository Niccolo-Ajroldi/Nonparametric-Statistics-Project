
library(nnet)
library(NeuralNetTools)
library(dplyr)
library(caret)
library(mgcv)
library(ModelMetrics)
library(conformalInference)

setwd("D:/Poli/Corsi/NPS/ProjNPS")

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
### TUNING

# cross-validation to tune parameters
control <- trainControl(method="cv", number=10) # number = k folds
set.seed(2)
net.caret <- train(
  y~.,
  data = df.train,
  preProcess = c("center", "scale"),
  method = "nnet",
  trControl = control,
  tuneLength = 10  # number of levels for each tuning parameters: 10 per "size" e 10 per "decay"
  # se in trainControl ho impostato search="random", allora tuneLength rappresenta
  # il massimo numero di step (e quindi di combinazioni di parametri) nella ricerca
)
net.caret
plot(net.caret)

net.caret$bestTune
results.1 <- net.caret$results

size.1 <- net.caret$bestTune$size
decay.1 <- net.caret$bestTune$decay

###_____________________________________________________________________
### BEST MODEL PREDICTION OVER TEST SET

library(nnet)
library(NeuralNetTools)

# pensa lui a fare il preProcess!
# capitolo 5.5.1:
# https://topepo.github.io/caret/model-training-and-tuning.html#
# stackoverflow:
# https://stats.stackexchange.com/questions/81609/is-preprocessing-needed-before-prediction-using-finalmodel-of-randomforest-with

# usando predict.train (anche scrivendo solo predict funzia uguale)
# ed evito di dover passare attraverso le probabilità come in soy_nnet
y.pred.test <- predict.train(net.caret, df.test)
confusionMatrix(y.pred.test, y.test)
