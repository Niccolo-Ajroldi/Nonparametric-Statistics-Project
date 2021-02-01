
library(nnet)
library(NeuralNetTools)

###_____________________________________________________________________
### TUNING

# cross-validation to tune parameters
control <- trainControl(method="cv", number=10) # number = k folds
set.seed(1996)
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
