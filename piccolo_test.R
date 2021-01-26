cores.number <-  detectCores()
cl <- makeCluster(cores.number)

T_vec <- matrix(0,10,6)

# lo scopo è riempire la clonna g con il nuero g
# ogni processore lavora su una riga diversa
# il ciclo for scorre le colonne
wrapper <- function(dummy,col){
  T_vec[,col] <- col
}

clusterExport(cl=cl, list('wrapper','T_vec'))

for(g in 1:6)
{
  T_vec[,g] <- pbsapply(T_vec[,g], wrapper, col=g, cl=cl)
}

T_vec
