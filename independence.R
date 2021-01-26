
setwd("D:/Poli/Corsi/NPS/ProjNPS")
source("data.R")

# data

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

niggaz.perc <- df$Black


# H0: (diff.perc) indipedente da (niggaz.perc)
# H1: H0^c

y <- diff.perc
x <- niggaz.perc
n <- length(x)

plot(y~x)
abline(h=0, lty=2, col='green')

# regression, test the significance of the coefficient of x
# under H0, y values are exchangeable
model.0 <- lm(y~x)
summary(model.0)
T0 <- abs(summary(model.0)$coefficients[2,3])
T0

# permutation test
B <- 1000
T_vec <- numeric(B)

#


for(b in 1:B){
  
  permutation <- sample(n)
  
  # test on x1
  Y.perm <- y[permutation]
  model.perm <- lm(Y.perm ~ x)
  T_vec[b] <- abs(summary(model.perm)$coefficients[2,3]) # fittiamo y permutata con le x
  
}


par(mfrow=c(1,2))
hist(T_vec,xlim=range(c(T_vec,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_vec),xlim=range(c(T_vec,T0)))
abline(v=T0,col=3,lwd=4)

sum(T_vec>=T0)/B
