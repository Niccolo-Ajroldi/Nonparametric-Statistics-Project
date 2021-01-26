rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####
d <- read.csv('data/merged_data.csv')

# merge w/ df

plot(d$percentage20_Joe_Biden ~ d$pop_density)

rd <- which(d$pop_density < 5000)
plot(d[rd,]$percentage20_Joe_Biden ~ d[rd,]$pop_density)


plot(d$percentage20_Joe_Biden ~ d$estimated_median_household_income)
plot(d$percentage16_Hillary_Clinton ~ d$estimated_median_household_income)

d$state
r <- which(d$state=="CA")
plot(d[r,]$percentage20_Joe_Biden ~ d[r,]$estimated_median_household_income)

library(Hmisc)

library(cpsvote)
library(srvyr)
library(dplyr)
library(here)
cps16 <- cps_load_basic(years = 2016, datadir = here::here('cps_data'))

