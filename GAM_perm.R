
setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

library(mgcv)
library(parallel)
library(pbapply)
library(dplyr)
library(caret)

#### data ------------------------------------------------------------------------------------

load("data/data_pools.Rdata")

#### cleaning --------------------------------------------------------------------------------

X <- merged_data

# preprocessing
X$pop_density <- log(X$pop_density)
X$diff20 <- X$percentage20_Joe_Biden - X$percentage20_Donald_Trump
X$bachelor_or_more <- X$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18
names(X)

x.names <- c('diff20',
             'polls2020',
             'total_votes20',
             'IncomePerCap',
             'Women',
             'White',
             'Black',
             'Hispanic',
             'bachelor_or_more',
             'pop_density',
             'Construction',
             'perc_poveri',
             'RUCC_code'
)

# kepp only interesting variables
X <- dplyr::select(X, all_of(x.names))
n <- dim(X)[1]

# training samples
set.seed(1)
train.samples <- createDataPartition(X$diff20, p=0.75, list=FALSE)

X.train <- X[train.samples]

length(train.samples)
n-length(train.samples)

# train-test split
X.train  <- X[train.samples,]
n.train  <- dim(X.train)[1]

## DATASET for following parts
X <- X.train
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
  "s(bachelor_or_more,bs='cr')",
  "s(pop_density,bs='cr')",
  "s(Construction,bs='cr')",
  "s(perc_poveri,bs='cr')",
  "s(RUCC_code,bs='cr',k=9)"
)
formula.full.rhs = paste(formula.regressors, collapse = " + ")
formula.full <- as.formula(paste0("diff20 ~ ", formula.full.rhs))

# number of regressors
p <- length(formula.regressors)

# full model
fit_full <- mgcv::gam(formula.full, data=X)
summary(fit_full)

# save observed test stat values
T0 <- NULL
for(j in 1:p)
  T0[j] <- summary(fit_full)$s.table[j,3]


#### REDUCED MODELS -------------------------------------------------------------------------

# save resiudals and fitted values for ALL the reduced models
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


#### TEST(s) ----------------------------------------------------------------------------------

X_noy <- X

B <- 500
T.boot <- matrix(0,B,p)

wrapper = function(dummy){
  perm  <- sample(1:n)
  res_p <- res[perm,j]
  X_noy$diff20 <- fitted[,j] + res_p
  boot <- summary(mgcv::gam(formula.full,data=X_noy))$s.table[j,3]
}

p_val <- NULL

# perform p tests
set.seed(1)

for(j in 1:p){
  
  print(j)
  
  boot <- numeric(B)
  
  # initialize parallel custers
  cl <- makeCluster(8)
  clusterExport(cl=cl,list('wrapper','fitted','j','formula.full','res','X_noy','T0','n'))
  
  boot <- pbsapply(boot,wrapper,cl=cl)
  T.boot[,j] <- boot
  
  p_val[j] <- sum(boot >= T0[j])/B
  print(p_val[j])
  
  # stop parallel clusters
  stopCluster(cl)
}

# save workspace
save.image("GAM_perm_results.RData")

#### p-values correction ------------------------------------------------------------------

load("GAM_perm_results.RData")

# p-values
p_val
p_val_adj = p.adjust(p_val, method="BH")

# parametric p-values
ss <- summary(fit_full)
ss$s.table
pv_GAM <- ss$s.table[,4]
pv_GAM_adj <- p.adjust(pv_GAM, method="BH")

pv_table <- data.frame(pv_GAM, pv_GAM_adj, p_val, p_val_adj)
View(pv_table)

T0

