rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####
d <- read.csv('data/merged_data.csv')

cases_p  <- d$cases/d$TotalPop
deaths_p <- d$deaths/d$TotalPop

diff_vote_16_20_T <- d$percentage20_Donald_Trump - d$percentage16_Donald_Trump

# HA SENSO SOTTRARRE PERCENTUALI??
x <- cases_p
y <- diff_vote_16_20_T
n <- length(x)

plot(y~x, pch=20,
     ylab="% difference votes Trump 2020-2016",
     xlab="Covid cases %",
     main="Trump votes against covid cases")
abline(h=0.0, lty=2, col="gray")

fit=lm(y~x)
abline(fit, col=2)
summary(fit)

B=20000
T0=abs(summary(fit)$coefficients[2,3])
T_b=numeric(B)
for(b in 1:B){
  s=sample(n)
  y_p=y[s]
  fit_p<-lm(y_p~x)
  T_b[b]=abs(summary(fit_p)$coefficients[2,3])
}

hist(T_b,xlim=range(T_b,T0))
abline(v=T0,col='red')

p<-sum(T0<=T_b)/B
p
plot(ecdf(T_b))
abline(v = T0, lty=2)


