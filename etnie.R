
setwd("D:/Poli/Corsi/NPS/ProjNPS")
source("data.R")

#### multivariate test #### 

# data
x <- df[,21:26]
names(x)

x.joe <- x[rows.joe,]
x.don <- x[rows.don,]

n1 <- dim(x.joe)[1]
n2 <- dim(x.don)[1]
n  <- n1 + n2

x.joe.mean <- colMeans(x.joe)
x.don.mean <- colMeans(x.don)
S.joe <- var(x.joe)
S.don <- var(x.don)

# test statistics, observed value
T0 <- as.numeric((x.joe.mean-x.don.mean) %*% (S.joe/n1 + S.don/n2) %*% (x.joe.mean-x.don.mean))
T0

B <- 1000
T_vec <- numeric(B)

for(b in 1:B){
  # permute the indexes
  permutation <- sample(n)
  x.perm <- x[permutation,]
  x.joe.perm <- x.perm[1:n1,]
  x.don.perm <- x.perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  x.joe.mean.perm <- colMeans(x.joe.perm)
  x.don.mean.perm <- colMeans(x.don.perm)
  S.joe.perm <- var(x.joe.perm)
  S.don.perm <- var(x.don.perm)
  
  T_vec[b]  <- (x.joe.mean.perm-x.don.mean.perm) %*% (S.joe.perm/n1 + S.don.perm/n2) %*% (x.joe.mean.perm-x.don.mean.perm) 
}


# plotting the permutational distribution under H0
hist(T_vec,xlim=range(c(T_vec,T0)),breaks=1000)
abline(v=T0,col=3,lwd=4)

plot(ecdf(T_vec), xlim=range(c(T_vec,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_vec>=T0)/B
p_val


#### univariate tests ####

his <- x$Hispanic
g <- ifelse(df$votes20_Joe_Biden > df$votes20_Donald_Trump, "Biden", "Trump")
boxplot(his ~ g)

# data
x <- df[,21:26]
names(x)

x.joe <- x[rows.joe,]
x.don <- x[rows.don,]

n1 <- dim(x.joe)[1]
n2 <- dim(x.don)[1]
n  <- n1 + n2

x.joe.mean <- sapply(x.joe, mean)
x.don.mean <- sapply(x.don, mean)

abs(x.joe.median-x.don.median)
# test statistics, observed value
T0 <- as.numeric(abs(x.joe.mean-x.don.mean))
T0

B <- 10000
T_vec <- matrix(0,B,6)

for(b in 1:B){
  # permute the indexes
  permutation <- sample(n)
  x.perm <- x[permutation,]
  x.joe.perm <- x.perm[1:n1,]
  x.don.perm <- x.perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  x.joe.mean.perm <- sapply(x.joe.perm, mean)
  x.don.mean.perm <- sapply(x.don.perm, mean)
  
  T_vec[b,]  <- as.numeric(abs(x.joe.mean.perm-x.don.mean.perm))
}

# plotting the permutational distribution under H0

# histograms
x11()
par(mfrow=c(2,3))
for(i in 1:6)
{
  hist(T_vec[,i],xlim=range(c(T_vec[,i],T0[i])),breaks=30, main=names(x)[i])
  abline(v=T0[i],col=3,lwd=2)
}

# ecdf
x11()
par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(ecdf(T_vec[,i]),xlim=range(c(T_vec[,i],T0[i])),breaks=30, main=names(x)[i])
  abline(v=T0[i],col=3,lwd=2)
}

# p-values
p_val = numeric(6)
for(i in 1:6)
{
  p_val[i] <- sum(T_ve[,i]>=T0[i])/B
}
p_val


#### al contrario, divido per etnie, poi anova ####

etnie <- names(x)

# prova sulle prime 10
x[1:10,]
(etnie[apply(x[1:10,],1,which.max)])

# etnia predominante per ogni stato
etnie <- as.factor(etnie[apply(x,1,which.max)])
g <- length(levels(etnie))
table(etnie) # Pacific non è mai predominante
# some groups have very low numerosity -> cannot do classical ANOVA!!

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

# plot
plot(etnie, diff.perc, xlab='Etnia', ylab='%(joe-don)', col=rainbow(g), main='Differenza percentuale joe - don')
abline(h=0, lty=3)

# anova
fit <- aov(diff.perc ~ etnie)
summary(fit)

# how is classical anova performing?
hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals)
shapiro.test(fit$residuals)

# permutation test
T0 <- summary(fit)[[1]][1,4]
T0
B <- 10000
T_vec <- numeric(B) 
n <- length(diff.perc)

for(b in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  etnie.perm <- etnie[permutation]
  fit.perm <- aov(diff.perc ~ etnie.perm)
  
  # Test statistic:
  T_vec[b] <- summary(fit.perm)[[1]][1,4]
}

layout(1)
hist(T_vec,xlim=range(c(T_vec,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_vec),xlim=range(c(T_vec,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_vec>=T0)/B
p_val


