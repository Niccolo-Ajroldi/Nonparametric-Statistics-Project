
rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS/Github_folder")

#_________________________________________________________________________________________
#### data ####

load("data/data_split.Rdata")

#_________________________________________________________________________________________
#### Data exploration ####

# Mode and mean
# vicotries of trump vs biden
X<-df
X$diff20<-X$y
mean(X$diff20)
median(X$diff20)
r_b<-which(X$diff20>=0)
sum(X$TotalPop[r_b])
sum(X$TotalPop[-r_b])
length(r_b)

o=boxplot(X$diff20)
min(o$out)
o$out
cnt=1
coo<-which(X$diff20>min(o$out))


X$contested<-X$fraud
x11()
par(bg='white', mfrow=c(2,3))
plot(X$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,X$diff20,
     pch=20,
     cex=.8,
     xlab = 'Percentege of adults with a bachelor degree or higher',
     ylab= 'Difference B-T 2020',
     col=ifelse(X$contested==1,'#FF6600','black')
)
plot(X$Women/X$TotalPop,X$diff20,
     pch=20,
     cex=.8,
     xlab = 'Percentege of Women',
     ylab= 'Difference B-T 2020',
     col=ifelse(X$contested==1,'#FF6600','black')
)
plot(X$Hispanic,X$diff20,
     pch=20,
     cex=.8,
     xlab = 'Percentege of Hispanic',
     ylab= 'Difference B-T 2020',
     col=ifelse(X$contested==1,'#FF6600','black')
)
plot(X$Income,X$diff20,
     pch=20,
     cex=.8,
     xlab = 'Income',
     ylab= 'Difference B-T 2020',
     col=ifelse(X$contested==1,'#FF6600','black')
)
plot(log(X$pop_density),X$diff20,
     pch=20,
     cex=.8,
     xlab = 'Population density (log-scaled)',
     ylab= 'Difference B-T 2020',
     col=ifelse(X$contested==1,'#FF6600','black')
)
X$diff16<-X$percentage16_Hillary_Clinton-X$percentage16_Donald_Trump
plot(X$diff16,X$diff20,
     pch=20,
     cex=.8,
     xlab = 'Difference Clinton-Trump 2020',
     ylab= 'Difference B-T 2020',
     col=ifelse(X$contested==1,'#FF6600','black')
)

X$wp=X$Women/X$TotalPop
x11()
par(bg='white', mfrow=c(2,3))
o=aplpack::bagplot(X$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,
                   X$diff20)
o=o$pxy.outlier
cnt=1
out=NULL
for( i in 1:length(o[,1]))
{
  out[cnt]=which(X$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18==o[i,1] &
                   X$diff20==o[i,2])
  cnt=cnt+1
}


X$state
x11()
par(mfrow=c(2,3))
o=aplpack::bagplot(X$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18,X$diff20
                   ,col.baghull='#FFFF66',col.looppoints="#000000",
                   col.bagpoints="#FF6600", transparency=TRUE,
                   col.loophull="#FF9900")
aplpack::bagplot(X$wp,X$diff20
                 ,col.baghull='#FFFF66',col.looppoints="#000000",
                 col.bagpoints="#FF6600", transparency=TRUE,
                 col.loophull="#FF9900"
)
aplpack::bagplot(X$Hispanic,X$diff20
                 ,col.baghull='#FFFF66',col.looppoints="#000000",
                 col.bagpoints="#FF6600", transparency=TRUE,
                 col.loophull="#FF9900"
)
aplpack::bagplot(X$Income,X$diff20
                 ,col.baghull='#FFFF66',col.looppoints="#000000",
                 col.bagpoints="#FF6600", transparency=TRUE,
                 col.loophull="#FF9900"
)
aplpack::bagplot(log(X$pop_density),X$diff20
                 ,col.baghull='#FFFF66',col.looppoints="#000000",
                 col.bagpoints="#FF6600", transparency=TRUE,
                 col.loophull="#FF9900"
)
aplpack::bagplot(X$diff16,X$diff20
                 ,col.baghull='#FFFF66',col.looppoints="#000000",
                 col.bagpoints="#FF6600", transparency=TRUE,
                 col.loophull="#FF9900"
)

load("data_pools.Rdata")
load("data_state.Rdata")

#X<-merged_data
#X$diff20<-X$percentage20_Joe_Biden-X$percentage20_Donald_Trump
#set polls in 0-1 interval
#X$polls2016<-X$polls2016/100
#X$polls2020<-X$polls2020/100

## States that were contested by Trump
c_frodate<-c('AZ', 'GA', 'MI', 'NV', 'PA', 'WI')
cf<-NULL
for( j in c_frodate){
  cf<-c(cf,which(X$state==j))
}
cnorm<-X[-cf,]
cfrod<-X[cf,]

x11()
boxplot(cnorm$diff20,cfrod$diff20)

n<-3044
n1<-dim(cnorm)[1]
T0<-median(cnorm$diff20)-median(cfrod$diff20)
B<-5000
Tb<-numeric(B)
for(b in 1:B) {
  perm<-sample(1:n)
  xp<-X[perm,]
  x1<-xp[1:n1,]
  x2<-xp[(1+n1):n,]
  Tb[b]<-median(x1$diff20)-median(x2$diff20)
}
p<-sum(Tb<=T0)/B
p # 0

x11()
hist(Tb,xlim=c(-0.15,0.15))
abline(v=T0,col='red')


cnorm$percvotes<-cnorm$total_votes20/cnorm$TotalPop
cfrod$percvotes<-cfrod$total_votes20/cfrod$TotalPop
cnorm<-cnorm[which(cnorm$percvotes<=1),]
x11()
boxplot(cnorm$percvotes,cfrod$percvotes)
T0<-median(cnorm$total_votes20)-median(cfrod$total_votes20)
B<-1000
Tb<-numeric(B)
for(b in 1:B) {
  perm<-sample(1:n)
  xp<-X[perm,]
  x1<-xp[1:n1,]
  x2<-xp[(1+n1):n,]
  Tb[b]<-median(x1$total_votes20)-median(x2$total_votes20)
  
}
p<-sum(Tb<=T0)/B
p # p=0

x11()
hist(Tb)
abline(v=T0,col='red')


