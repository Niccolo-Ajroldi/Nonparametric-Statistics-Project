#### Permutational linear regression

#### F-test ----

# H0: beta_1 = beta_2 = beta_3 = ... = 0

permutational_F_test <- function(x, y, perm, seed = F){
  
  if(seed!=F){
    set.seed(seed)
  }
  
  result <- lm(y ~., data = data.frame(x))
  T0_glob <- summary(result)$f[1]
  
  B <- perm
  
  library(pbmcapply)
  pb <- progressBar(min = 0, max = B)
  
  n <- dim(as.matrix(y))[1]
  
  T_H0glob <- numeric(B)
  for(perm in 1:B){
    permutazione <- sample(n)
    y.perm.glob <- y[permutazione]
    T_H0glob[perm] <- summary(lm(y.perm.glob ~., data = data.frame(x)))$f[1]
    setTxtProgressBar(pb, perm)
  }

  
  p <- sum(T_H0glob>=T0_glob)/B
  
  return(list(T0 = T0_glob,
              T_stat = T_H0glob,
              p = p))
   
}




#### Test on coefficients ----

# H0: beta_i = 0

#'@param coeff indicates the coefficient we want to test
#'             0 for intercept, 1 for slope in simple regression etc.
#'
permutational_t_test <- function(x, y, perm, coeff, seed = F){
  
  if(seed!=F){
    set.seed(seed)
  }
  
  result <- lm(y ~., data = data.frame(x))
  t0_stat <- abs(summary(result)$coefficients[coeff+1,3])
  
  B <- perm
  
  library(pbmcapply)
  pb <- progressBar(min = 0, max = B)
  
  n <- dim(as.matrix(y))[1]
  
  t_stat <- numeric(B)
  for(perm in 1:B){
    permutazione <- sample(n)
    y.perm.glob <- y[permutazione]
    t_stat[perm] <- abs(summary(lm(y.perm.glob ~., data = data.frame(x)))$coefficients[coeff+1,3])
    setTxtProgressBar(pb, perm)
  }
  
  
  p <- sum(t_stat>=t0_stat)/B
  
  return(list(t0 = t0_stat,
              t_stat = t_stat,
              p = p))
  
}

# B <- 1000
# T_H0glob <- T_H01 <- T_H02 <- T_H03 <- numeric(B)
# 
# for(perm in 1:B){
#   permutazione <- sample(n)
#   
#   Y.perm.glob <- Y[permutazione]
#   T_H0glob[perm] <- summary(lm(Y.perm.glob ~ x1 + x2 + x3))$f[1]
#   
#   residui.H01.perm <- residui.H01[permutazione]
#   Y.perm.H01 <- regr.H01$fitted + residui.H01.perm
#   T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ x1 + x2 + x3))$coefficients[2,3])
#   
#   residui.H02.perm <- residui.H02[permutazione]
#   Y.perm.H02 <- regr.H02$fitted + residui.H02.perm
#   T_H02[perm] <- abs(summary(lm(Y.perm.H02 ~ x1 + x2 + x3))$coefficients[3,3])
#   
#   residui.H03.perm <- residui.H03[permutazione]
#   Y.perm.H03 <- regr.H03$fitted + residui.H03.perm
#   T_H03[perm] <- abs(summary(lm(Y.perm.H03 ~ x1 + x2 + x3))$coefficients[4,3])
#   
# }
# 
# sum(T_H0glob>=T0_glob)/B
# sum(T_H01>=T0_x1)/B
# sum(T_H02>=T0_x2)/B
# sum(T_H03>=T0_x3)/B
# 
