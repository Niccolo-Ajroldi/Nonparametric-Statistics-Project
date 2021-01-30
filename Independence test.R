remove(list = ls())
cat("\014")

setwd("C:/Users/edoar/Desktop/Nonparametric statistics/Project")

source("code/Independence test function.R")

#### DATA ----
load("data/data_last_version3/data_pools.Rdata")

attach(merged_data)

# target variable
y <- percentage20_Joe_Biden - percentage20_Donald_Trump

diff_2016 <- percentage16_Hillary_Clinton - percentage16_Donald_Trump

detach(merged_data)



results_indep_test <- list()



#### ETHNICITY ----

x <- dplyr::select(merged_data, c("Hispanic", "White", "Black", "Native", "Asian"))

results_indep_test[["ethnicity"]] <- independence_perm_test(y, x)


#### EDUCATION ----

x <- dplyr::select(merged_data, c("Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18"))

results_indep_test[["education"]] <- independence_perm_test(y, x)

#### SEX ----

x <- dplyr::select(merged_data, c("Men"))

results_indep_test[["sex"]] <- independence_perm_test(y, x)

#### POPULATION DENSITY ----

x <- log(dplyr::select(merged_data, c("pop_density"))) ### log

results_indep_test[["population density"]] <- independence_perm_test(y, x)

#### INCOME ---- 

x <- dplyr::select(merged_data, c("Income"))

results_indep_test[["income"]] <- independence_perm_test(y, x)


#### POVERTY PERCENTAGE ----

x <- dplyr::select(merged_data, c("perc_poveri"))

results_indep_test[["poverty percentage"]] <- independence_perm_test(y, x)

#### POLLS ----

x <- dplyr::select(merged_data, c("polls2020"))

results_indep_test[["polls"]] <- independence_perm_test(y, x)

#### RESULTS 2016 ----

results_indep_test[["results 2016"]] <- independence_perm_test(y, diff_2016)


#### TO PLOT ----

# set the following values

# R0 <- # numeric, observed value of the test statistics
# R_stat <- # vector, value of the test statistics under permutations


hist(R_stat, xlim=range(c(R_stat,R0)), breaks=30, main = 'Permutational distribution of dCorr under H0')
abline(v=R0,col=3,lwd=2)

plot(ecdf(R_stat), main = 'Permutational cdf of dCorr under H0')

plot(ecdf(R_stat), xlim=range(c(R_stat,R0)), main = 'Permutational cdf of dCorr under H0')
abline(v=R0, col=3, lwd=2)

