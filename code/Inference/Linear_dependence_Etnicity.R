
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#_________________________________________________________________________________________
#### data ####

load("data/data_pools.Rdata")

# ethnic groups
etnie <- df[,21:26]
G <- dim(etnie)[2]

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

y <- diff.perc
n <- length(y)

#_________________________________________________________________________________________
#### plot ####

x11()
par(mar=c(1,1,1,1))
par(mfrow=c(6,2))

#_________________________________________________________________________________________
#### Parallelized permutation Test for linear dependence ####

# intialization
B <- 1000
T0 <- numeric(G)
T_vec <- matrix(0, nrow=B, ncol=G)

# parallel
library(pbapply)
library(parallel)
cores.number <-  detectCores()
cl <- makeCluster(cores.number)

wrapper <- function(dummy,gruppo){
  x <- etnie[,gruppo]
  permutation <- sample(n)
  Y.perm <- y[permutation]
  model.perm <- lm(Y.perm ~ x)
  T_vec[,gruppo] <- abs(summary(model.perm)$coefficients[2,3])
}

clusterExport(cl=cl, list('wrapper','y','etnie','n','T_vec'))

## cycle over the etnies
for(g in 1:6)
{
  # current etnia
  x <- etnie[,g]
  plot(y~x, main=names(etnie)[g], xlab=paste("% of ", names(etnie)[g]), ylab="diff % votes")
  
  # model under H0
  model.0 <- lm(y~x)
  T0[g] <- abs(summary(model.0)$coefficients[2,3])
  
  # permutation test
  T_vec[,g] <- pbsapply(T_vec[,g], # a chi applicare
                        wrapper,   # cosa applicare
                        gruppo=g,  # secondo argomento del wrapper
                        cl=cl      # cluster object
                        ) 
  
  hist(T_vec[,g],xlim=range(c(T_vec[,g],T0[g])),breaks=30)
  abline(v=T0[g],col=3,lwd=2)
  
}





