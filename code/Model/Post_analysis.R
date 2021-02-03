
rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS/Github_folder")

#_________________________________________________________________________________________
#### data ####

load("data/data_pools.RData")
X <- merged_data

X$diff20<-X$percentage20_Joe_Biden-X$percentage20_Donald_Trump

#_________________________________________________________________________________________
#### posterior analysis ####

load("data/data_split.Rdata")
x.names <- c(
  "diff.2016",
  "polls2016",# ?
  "polls2020",
  "total_votes20", # ?
  "Men",
  "Hispanic",                                                             
  "White",
  "Black",
  "Native",
  "Asian",
  "Income",
  "RUCC_code",
  "bachelor_or_more",
  "perc_poveri",
  "pop_density"
)
formula_<-y~
  s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')


formula_16<-y~diff16+
  s(polls2020,bs='cr') + 
  s(polls2016,bs='cr') + 
  s(Construction,bs='cr') +
  s(IncomePerCap,bs='cr') +
  s(Hispanic,bs='cr') +
  s(Black,bs='cr') +
  
  s(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,bs='cr') +
  
  s(White,bs='cr') +
  s(Women,bs='cr') +
  s(pop_density,bs='cr') +
  s(perc_poveri,bs='cr') +
  s(total_votes20,bs='cr')

#### Estraggo il 50 per cento delle county fraudolente, circa 200
X_train<-df.train
X_test<-df.test
c_frodate<-c('AZ', 'GA', 'MI', 'NV', 'PA', 'WI')
cf<-NULL
nn<-665
for( j in c_frodate){
  cf<-c(cf,which(df.train$state==j))
}
X_test$frode<-numeric(nn)
X_test$frode[cf]<-1
length(X_test[which(X_test$frode==1),1])
c_frodate<-c('AZ', 'GA', 'MI', 'NV', 'PA', 'WI')
cf<-NULL
for( j in c_frodate){
  cf<-c(cf,which(X_train$state==j))
}
X_train$frode<-numeric(1711)
X_train$frode[cf]<-1
X_train<-X_train[which(X_train$frode==0),]

X_train<-X_train[,-c(1,2,3)]
X_test<-X_test[,-c(1,2,3)]
X_train$diff20<-X_train$percentage20_Joe_Biden-X_train$percentage20_Donald_Trump
X_train$diff16<-X_train$percentage16_Hillary_Clinton-X_train$percentage16_Donald_Trump
X_test$diff20<-X_test$percentage20_Joe_Biden-X_test$percentage20_Donald_Trump
X_test$diff16<-X_test$percentage16_Hillary_Clinton-X_test$percentage16_Donald_Trump
x.names=names(X_train)

## creo le function per cnformal prediction
train_f=function(x,y,out=NULL){
  colnames(x) <- x.names
  train_data <- data.frame(y=y,x)
  
  obj<-mgcv::gam(formula_ ,data=train_data)
}

predict_gam=function(obj, new_x){
  new_x = data.frame(new_x)
  colnames(new_x) = x.names
  predict(obj,new_x)
}

library(conformalInference)
#X_train<-X_train[,-c(1,2,3)] # tolgo stato e contea
#X_test<-X_test[,-c(1,2,3)]
PI<-conformal.pred.split(x=as.matrix(X_train),
                         y=(X_train$diff20),
                         x0=as.matrix(X_test),
                         train.fun = train_f,
                         predict.fun = predict_gam)

nn=dim(X_test)[1]

PI<-cbind(PI$lo,PI$pred,PI$up)

png(file = "Pred_intervals.png", width = 6000, height = 5000, units = "px", res = 800)
plot(1:nn,X_test$diff20, pch=20,cex=.8,ylab='Prediction', xlab="Counties" ,main='Prediction Intervals of reduced gam' ,col=ifelse(X_test$frode ==0,'black','#F5A700'))
segments(1:nn,PI[,1], 1:nn,PI[,3],lwd=.8, col='light blue')
points(1:nn,PI[,1],cex=0.4,pch=10,col='#6699FF')
points(1:nn,PI[,3],cex=0.4,pch=10,col='#6699FF')
points(1:nn,X_test$diff20, pch=20, cex=.8 , col=ifelse(X_test$frode==0,'black','#F5A700'))
dev.off()

x11()
res<-X_test$diff20-PI[,2]
median(abs(res))
mean(abs(PI[,3]-PI[,1]))
x11()
out=boxplot(res)
abline(h=0.34)
out$out
res[row_out]
row_out<- which(res>=0.332 | res<=-0.39)
outliers<-df.test[row_out,]
View(outliers)
row_out_tot<-NULL
X$predicted<-numeric(3044)
for(i in 1:length(row_out)){
  row_out_tot[i]<-which(X$votes20_Joe_Biden==outliers$votes20_Joe_Biden[i] 
                     & X$votes20_Donald_Trump ==outliers$votes20_Donald_Trump[i])
  X$predicted[row_out_tot[i]]<-PI[row_out[i],2]
}
outliers<-X[row_out_tot,]
View(outliers)
View(outliers[,c(2,3,55,69)])


## Test for difference between froud and not
res<-X_test$y-PI[,2] # so it is positive if the prediction is in  more favor of trump
res_f<-res[which(X_test$frode==1)]
res_n<-res[which(X_test$frode==0)]

T0<-median(res_f)-median(res_n) ## We want to see if T0 > Tb
B<-1000
Tbx<-numeric(B)
n1=length(res_f)
n=length(res)
for(b in 1:B){
  ran_p<-sample(res)
  ran_1<-ran_p[1:n1]
  ran_2<-ran_p[(n1+1):n]
  Tbx[b]<-median(ran_1)-median(ran_2)
}
sum(Tbx>=T0)/B

T0mean<-mean(res_f)-mean(res_n) ## We want to see if T0 < Tb
B<-1000
tbmean<-numeric(B)
n1=length(res_f)
n=length(res)
for(b in 1:B){
  ran_p<-sample(res)
  ran_1<-ran_p[1:n1]
  ran_2<-ran_p[(n1+1):n]
  tbmean[b]<-mean(ran_1)-mean(ran_2)
}
sum(tbmean>=T0mean)/B
png(file = "test of pred.png", width = 8000, height = 4000, units = "px", res = 800)
par(mfrow=c(2,2))
hist(Tbx,col='black',nclass = 15, xlab='',main='Histogram of the median')
abline(v=T0,col='#F5A700',lwd=2)
plot(ecdf(Tbx),col='black', xlab='',main='ECDF of the median',pch=20)
abline(v=T0,col='#F5A700',lwd=2)
hist(tbmean,col='black',nclass = 15, xlab='',main='Histogram of the mean')
abline(v=T0mean,col='#F5A700',lwd=2)
plot(ecdf(tbmean),col='black', xlab='',main='ECDF of the mean',pch=20)
abline(v=T0mean,col='#F5A700',lwd=2)
dev.off()


### Calculate the depth

library(DepthProc)
xnames=c('total_votes20','Women','Hispanic','White','Black','IncomePerCap',
         'Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18',
         'perc_poveri','pop_density','polls2020','polls2016')
X<-merged_data
X_n<-dplyr::select(X,xnames)
x.depths <- depth(X_n, method="Tukey")
x11()
boxplot(x.depths,col='black')

abline(h=x.depths[which(X$state=='CA' & X$county=='Alpine')],col='#F5A700',lwd=2)
abline(h=x.depths[which(X$state=='CA' & X$county=='Los Angeles')])
png(file = "Hist outlier.png", width = 6000, height = 5000, units = "px", res = 800)
hist(x.depths,col='black',nclass = 20, xlab='Depth w.r.t. covariates of the reduced model')
abline(v=x.depths[which(X$state=='CA' & X$county=='Alpine')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='CA' & X$county=='Lake')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='CA' & X$county=='Marine')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='CA' & X$county=='San Francisco')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='CA' & X$county=='Sonoma')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='IA' & X$county=='Des Moines')],col='#F5A700',lwd=2)

abline(v=x.depths[which(X$state=='IA' & X$county=='Muscatine')],col='#F5A700',lwd=2)


abline(v=x.depths[which(X$state=='CO' & X$county=='Elbert')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='CO' & X$county=='Pitkin')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='DC' & X$county=='District of Columbia')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='HI' & X$county=='Honolulu')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='IA' & X$county=='Marshall')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='IL' & X$county=='Richland')],col='#F5A700',lwd=2)
abline(v=x.depths[which(X$state=='OH' & X$county=='Lucas')],col='#F5A700',lwd=2)

dev.off()

png(file = "Pointwise Prediction.png", width = 6000, height = 5000, units = "px", res = 800)
par(mfrow=c(1,1))
plot(PI[,2],X_test$diff20, pch=20, cex=.8,  ylab='Difference 2020',xlab='Pointwise prediction' ,main='Pointwise prediction error' ,col=ifelse(X_test$frode==0,'black','#F5A700'))
points(PI[row_out,2],X_test$diff20[row_out], pch=20, cex=.8,col='springgreen')
abline(0,1,col='blue',lwd=1.5)
legend(.3, -.5, legend=c("Outlier", "Fraudulent counties", 'Normal counties'),
       col=c("springgreen",'#F5A700', "black"), lty=1, cex=0.8)
dev.off()
x11()
aplpack::bagplot(PI[,2],X_test$diff20)
