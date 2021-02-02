
library(dplyr)
library(mgcv)
library(ModelMetrics)

setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

### data -------------------------------------------------------------------------

load("data/data_split.Rdata")

# add democratic margin in 2016
df$diff.2016 <- df$percentage16_Hillary_Clinton - df$percentage16_Donald_Trump

# nuova target variable
df$y <- df$y - df$diff.2016

# covid
df$covid_case_perc  <- df$cases/df$TotalPop
df$covid_death_perc <- df$deaths/df$TotalPop

# preprocessing
df$bachelor_or_more <- df$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18
df$pop_density <- log(df$pop_density)

# regressor names
x.names <- c(
  'covid_case_perc',
  'covid_death_perc',
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

#### FINAL MODEL -------------------------------------------------------------------------

# formula
formula.final.names <- c(
  "s(covid_case_perc,bs='cr')",
  "s(covid_death_perc,bs='cr')",
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
summary(fit_full)
plot(fit_full) # shows effective dof on the axis

# dof
dof <- summary(fit_full)$s.table[,1]

# HISPANIC
fit=smooth.spline(df$y~df$Hispanic,df=dof[6])
png(file = "Hispanic_diff.png", width = 6000, height = 5000, units = "px", res = 800)
col.3 <- "#F5A700"
plot(df$Hispanic,
     df$y,
     cex =.5,
     col = "darkgrey", 
     main= "Democratic gain vs Hispanic %",
     ylab= "Democratic gain wrt 2016 election",
     xlab= "Hispanic %",
     ylim=c(-0.35,0.35))
lines(fit,col=col.3,lwd=2)
legend("topright",
       legend=c("Estimated regression line"),
       col=c(col.3), 
       lty=c(1),
       lwd=c(2),
       cex=0.75)
dev.off()


# BACHELOR
fit=smooth.spline(df$y~df$bachelor_or_more,df=dof[8])
png(file = "Bachelor_diff.png", width = 6000, height = 5000, units = "px", res = 800)
col.3 <- "#F5A700"
plot(df$bachelor_or_more,
     df$y,
     cex =.5,
     col = "darkgrey", 
     main= "Democratic gain in 2020 vs education",
     ylab= "Democratic gain wrt 2016 election",
     xlab= "% of people with bachelor or more",
     ylim=c(-0.3,0.3))
lines(fit,col=col.3,lwd=3)
legend("topright",
       legend=c("Estimated regression line"),
       col=c(col.3), 
       lty=c(1),
       lwd=c(2),
       cex=0.75)
dev.off()
