
#' Independence permutational test based on distance correlation (dCorr) index
#' 
#' @references for dCorr index:
#' Székely, G J., Rizzo, M. L. and Bakirov, N. K. (2007). 
#' Measuring and testing dependence by correlation of distances. The annals of Statistics 35 6 2769-2794.
#' 
#' The test is based on dCorr index that is a generalization of the linear correlation coefficients.
#' dCorr takes still values among [0,1], but unlike the linear correlation coefficient, 
#' dCorr is defined for two datasets X and Y in arbitrary dimensions, with the same number of observations.
#' This index is able to spot nonlinear and nonmonotonic depences among data and 
#' moreover dCorr==0 characterizes independence of X and Y.
#' 
#' The test shows good performances in terms of power w.r.t. parametric tests on simulated datasets.
#'  
#'  
#'  @param X data
#'  @param Y data
#'  @param seed seed to be set if specified, default is no seed
#'  
#'  @return a list with the following components:
#'          R0 numeric, observed value of dCorr
#'          R_stat vector, value of dCorr under permutations
#'          p numeric, pvalue of the test
#'           
#' The number of permutations is set as in the simulations reported in the paper equal to the floor of 200 + 5000/n
#' where n is the number of observations.
#' 
#' WARNING:sample size of X and Y must agree (number of rows), 
#'         not necessarily the number of covariates (number of columns)
#' 


independence_perm_test <- function(X, Y, seed = F){
  
  ### SETTING SEED
  # if provided
  if(seed!=F)
    {
    set.seed(seed)
  }
  
  
  ### SETTING THE NUMBER OF OBSERVATIONS
  n <- dim(as.matrix(X))[1]
  if(n != dim(as.matrix(Y))[1])
    {
    stop('The sample size of the two dataset must agree')
  }
  
  
  ### SETTING THE NUMBER OF ITERATIONS (see reference)
  niter <- floor(200 + 5000/n)
  
  
  ### SETTING PROGRESS BAR
  library(pbmcapply)
  pb <- progressBar(min = 0, max = niter)
  
  
  ### COMPUTING OBSERVED VALUE OF THE STATISTICS
  library('energy')
  R0 <- dcor(X,Y)
  
  
  ### MC SIMULATION OF THE STATISTICS DISTRIBUTION UNDER H0
  R_stat <- numeric(niter)
  for(perm in 1:niter)
    {
    
    # permutation
    permutation <- sample(1:n) 
    X_perm <- X[permutation]
    
    # test statistic:
    R_stat[perm] <- dcor(X_perm, Y)
    setTxtProgressBar(pb, perm)
  }
  
  
  ### COMPUTATION OF P-VALUE
  p <- sum(R_stat>=R0)/niter
  
  ### PLOTS
  hist(R_stat, xlim=range(c(R_stat,R0)), breaks=30, main = 'Permutational distribution of dCorr under H0')
  abline(v=R0,col=3,lwd=2)
  
  plot(ecdf(R_stat), main = 'Permutational cdf of dCorr under H0')
  
  plot(ecdf(R_stat), xlim=range(c(R_stat,R0)), main = 'Permutational cdf of dCorr under H0')
  abline(v=R0, col=3, lwd=2)
  
  
  return(list(R0 = R0,
              R_stat = R_stat, 
              p = p))
  
}


