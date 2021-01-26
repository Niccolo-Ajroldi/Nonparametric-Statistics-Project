
setwd("D:/Poli/Corsi/NPS/ProjNPS")
source("data.R")

# data

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

niggaz.perc <- df$Hispanic


# H0: (diff.perc) indipedente da (niggaz.perc)
# H1: H0^c

y <- diff.perc
x <- niggaz.perc
n <- length(x)

layout(1)
plot(y~x)
abline(h=0, lty=2, col='green')

# regression, test the significance of the coefficient of x
# under H0, y values are exchangeable
model.0 <- lm(y~x)
summary(model.0)
T0 <- abs(summary(model.0)$coefficients[2,3])
T0

# permutation test
set.seed(1)
B <- 10000
T_vec <- numeric(B)

# parallel test
library(pbapply)
library(parallel)

cores.number <-  detectCores()
cl <- makeCluster(cores.number)

wrapper=function(dummy){
  
  permutation <- sample(n)
  Y.perm <- y[permutation]
  model.perm <- lm(Y.perm ~ x)
  T_vec <- abs(summary(model.perm)$coefficients[2,3]) # fittiamo y permutata con le x

}

clusterExport(cl=cl, list('wrapper','y','x','n'))
T_vec <- pbsapply(T_vec,wrapper,cl=cl)

par(mfrow=c(1,2))
hist(T_vec,xlim=range(c(T_vec,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_vec), xlim=(range(T_vec,T0)), main='Sample median')
abline(v=T0,col=3,lwd=3)



hispanic.percent <- df$Hispanic
x11()
plot(diff.perc ~ hispanic.percent, ylab="Joe-Trump %")
abline(lm(diff.perc ~ hispanic.percent))
abline(h=0, col=2, lty=3)
abline(v=50, col=2, lty=3)
