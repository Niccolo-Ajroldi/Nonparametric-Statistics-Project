
setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

library(mgcv)
library(pbapply)
library(dplyr)

#### data ------------------------------------------------------------------------------------

load("data/data_pools.Rdata")

#### cleaning --------------------------------------------------------------------------------

X <- merged_data

# preprocessing
X$pop_density <- log(X$pop_density)
X$diff20 <- X$percentage20_Joe_Biden - X$percentage20_Donald_Trump
names(X)

x.names <- c('diff20',
             'polls2020',
             'total_votes20',
             'IncomePerCap',
             'Women',
             'White',
             'Black',
             'Hispanic',
             'Native',
             'Asian',
             'Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18',
             'estimated_median_household_income',
             'pop_density',
             'Construction',
             'perc_poveri',
             'RUCC_code'
)

# kepp only interesting variables
X <- dplyr::select(X, all_of(x.names))
n <- dim(X)[1]

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

## DATASET for following parts
X <- x.train
n <- n.train

#### FULL MODEL --------------------------------------------------------------------------------

# full formula
formula.regressors <- c(
  "s(polls2020,bs='cr')",
  "s(total_votes20,bs='cr')",
  "s(IncomePerCap,bs='cr')",
  "s(Women,bs='cr')",
  "s(White,bs='cr')",
  "s(Hispanic,bs='cr')",
  "s(Black,bs='cr')",
  "s(Native,bs='cr')",
  "s(Asian,bs='cr')",
  "s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr')",
  "s(estimated_median_household_income,bs='cr')",
  "s(pop_density,bs='cr')",
  "s(Construction,bs='cr')",
  "s(perc_poveri,bs='cr')",
  "s(RUCC_code,bs='cr',k=7)"
)
formula.full.rhs = paste(formula.regressors, collapse = " + ")
formula.full <- as.formula(paste0("diff20 ~ ", formula.full.rhs))

# number of regressors
p <- length(formula.regressors)

# full model
fit_full <- mgcv::gam(formula.full, data=X)

# save observed test stat values
T0 <- NULL
for(j in 1:p)
  T0[j] <- summary(fit_full)$s.table[j,3]


#### REDUCED MODELS -----------------------------------------------------------------------------

# save resiudals and fitted for ALL the reduced models
res <- matrix(0, n, p)
fitted <- matrix(0,n,p)

for(i in 1:p)
{
  # reduced formula
  formula.reduced.rhs <- paste(formula.regressors[-1], collapse = " + ")
  formula.reduced <- as.formula(paste0("diff20 ~ ", formula.reduced.rhs))
  
  # reduced model
  gam.reduced <- mgcv::gam(formula.reduced, data=X)
  
  # save residuals and fitted values
  res[,i] <- gam.reduced$residuals
  fitted[,j] <- gam.reduced$fitted.values
}


#### TEST ------------------------------------------------------------------------------------

X_noy <- X

B <- 500
T.boot <- matrix(0,B,p)

wrapper = function(dummy){
  perm <- sample(1:n)
  res_p <- res[perm,j]
  X_noy$diff20 <- fitted[,j] + res_p
  boot <- summary(mgcv::gam(formula.full,data=X_noy))$s.table[j,3]
}

p_val <- NULL

# initialize parallel custers
cl <- makeCluster(8)

for(j in 1:p){
  boot <- numeric(B)
  clusterExport(cl=cl,list('wrapper','fitted','j','formula_full','res','X_noy','T0'))
  boot <- pbsapply(boot,wrapper,cl=cl)
  T.boot[,j] <- boot
  p_val[j] <- sum(boot >= T0[j])/B
  print(p_val[j])
}

# stop parallel clusters
stopCluster(cl)


