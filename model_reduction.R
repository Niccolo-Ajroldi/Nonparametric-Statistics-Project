
setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

#### data ####--------------------------------------------------------

load("data/data_pools.Rdata")

library(mgcv)
library(pbapply)

#### A

X<-merged_data
X$diff20<-X$percentage20_Joe_Biden-X$percentage20_Donald_Trump
names(X)
n<-dim(X)[1]
p<-dim(X)[2]
X$lat_long<-X$lat*X$long
X$HB<-X$Black*X$Hispanic

formula_full<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

fit_full<-mgcv::gam(formula_full,data=X)

summary(fit_full)
names_cov<-c('polls2016','polls2020',"Black","Hispanic","total_votes20",
             "Native",'White','lat_long','IncomePerCap',
             'Percent.of.adults.with.a.high.school.diploma.only..2014.18',
             'Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18',
             "Women","pop_density","RUCC_code" ,'Asian',
             "Construction","perc_poveri",'estimated_median_household_income')
length(summary(fit_full)$s.table[,3])

formula_polls16<-diff20~s(polls2020,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_polls20<-diff20~
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_HB<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_constr<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_income<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_hisp<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_black<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_native<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_latlong<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')



formula_school1<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_school2<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_esthouse<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +

  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_white<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_women<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +

  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_popdens<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +

  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_percpov<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +

  s(total_votes20,bs='cr')+
  s(Asian,bs='cr')

formula_totvot<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +

  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +

  s(Asian,bs='cr')

formula_asian<-diff20~s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(HB,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Native,bs='cr') +
  s(lat_long,bs='cr') +
  s(Percent.of.adults.with.a.high.school.diploma.only..2014.18,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(estimated_median_household_income,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')

res=matrix(0,3044,19)

fit<-mgcv::gam(formula_polls20,data=X)
res[,1]<-fit$residuals
fit<-mgcv::gam(formula_polls16,data=X)
res[,2]<-fit$residuals
fit<-mgcv::gam(formula_HB,data=X)
res[,3]<-fit$residuals
fit<-mgcv::gam(formula_constr,data=X)
res[,4]<-fit$residuals
fit<-mgcv::gam(formula_income,data=X)
res[,5]<-fit$residuals
fit<-mgcv::gam(formula_hisp,data=X)
res[,6]<-fit$residuals
fit<-mgcv::gam(formula_black,data=X)
res[,7]<-fit$residuals
fit<-mgcv::gam(formula_native,data=X)
res[,8]<-fit$residuals
fit<-mgcv::gam(formula_latlong,data=X)
res[,9]<-fit$residuals
fit<-mgcv::gam(formula_school1,data=X)
res[,10]<-fit$residuals
fit<-mgcv::gam(formula_school2,data=X)
res[,11]<-fit$residuals
fit<-mgcv::gam(formula_esthouse,data=X)
res[,12]<-fit$residuals
fit<-mgcv::gam(formula_white,data=X)
res[,13]<-fit$residuals
fit<-mgcv::gam(formula_women,data=X)
res[,14]<-fit$residuals
fit<-mgcv::gam(formula_popdens,data=X)
res[,15]<-fit$residuals
fit<-mgcv::gam(formula_percpov,data=X)
res[,16]<-fit$residuals
fit<-mgcv::gam(formula_totvot,data=X)
res[,17]<-fit$residuals
fit<-mgcv::gam(formula_asian,data=X)
res[,18]<-fit$residuals
fitted=matrix(0,3044,18)

for(j in 1:18){
  fitted[,j]<-X$diff20-res[,j]
}


#### TEST ####

fit_full<-mgcv::gam(formula_full,data=X)
T0 <- NULL
for(j in 1:18)
  T0[j]<-summary(fit_full)$s.table[j,3]

X_noy<-X
B<-500
T.boot<-matrix(0,B,18)
wrapper=function(dummy){
  perm=sample(1:3044)
  res_p<-res[perm,j]
  X_noy$diff20<-fitted[,j]+res_p
  boot<-summary(mgcv::gam(formula_full,data=X_noy))$s.table[j,3]
}
p_val<-NULL
cl=makeCluster(8)

for(j in 1:18){
  boot<-numeric(B)
  clusterExport(cl=cl,list('wrapper','fitted','j','formula_full','res','X_noy','T0'))
  boot<-pbsapply(boot,wrapper,cl=cl)
  T.boot[,j]=boot
  p_val[j]<-sum(boot>=T0[j])/B
  print(p_val[j])
}

# stop parallel clusters
stopCluster(cl)





