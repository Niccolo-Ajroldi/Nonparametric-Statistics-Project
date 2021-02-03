
rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#_________________________________________________________________________________________
#### data ####

df <- read.csv('data/merged_data.csv')

#_________________________________________________________________________________________
#### stati trump vs stati biden ####
perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

#_________________________________________________________________________________________
#### education ####
x <- df[,58:61]
names(x)
names(x) <- c(
  "less_diploma",
  "only_diploma",
  "college",
  "bachelor_or_more"
)
y <- diff.perc*100
x11()
par(mfrow=c(2,2))
plot(y ~ x[,1], main="", ylab="% Joe Biden", xlab="% of adults w/ less than high school diploma")
plot(y ~ x[,2], main="", ylab="% Joe Biden", xlab="% of adults w/ high school diploma only")
plot(y ~ x[,3], main="", ylab="% Joe Biden", xlab="% of adults compleating college or associate")
plot(y ~ x[,4], main="", ylab="% Joe Biden", xlab="% of adults w/ bachelor or more")

#_________________________________________________________________________________________
#### TEST ####

y = diff.perc*100
x <- x$bachelor_or_more
n <- length(x)

# regression, test the significance of the coefficient of x
# under H0, y values are exchangeable
model.0 <- lm(y~x)
summary(model.0)
T0 <- summary(model.0)$coefficients[2,3]
T0

col.3 <- "#F5A700"

tiff(file = "Pics/Eduction_linear.tiff", width = 6000, height = 5000, units = "px", res = 800)
plot(y~x,
     xlab="% of people with a bachelor degree or more",
     ylab="Biden-Trump % of votes",
     main="Education impact on elections",
     ylim=c(-100,100),
     #pch=19,
     cex=0.75,
     col="darkgrey"
     )
abline(model.0, col=col.3, lwd=2, lty=1)
legend(48, -73, 
       legend=c("Linear regression line"),
       col=col.3, 
       lty=1,
       lwd=2,
       cex=0.9)
dev.off()

#_________________________________________________________________________________________
#### PERMUTATIONTEST ####

# permutation test
B <- 10000
T_vec <- numeric(B)

for(b in 1:B){
  permutation <- sample(n)
  # test on x1
  Y.perm <- y[permutation]
  model.perm <- lm(Y.perm ~ x)
  T_vec[b] <- -summary(model.perm)$coefficients[2,3] # fittiamo y permutata con le x
}

  
# plot
tiff(file = "Education.test.tiff", width = 6000, height = 3000, units = "px", res = 800)
p <- hist(T_vec,
          xlim=range(c(T_vec,T0)),
          breaks=25, 
          xlab="Test statistic",
          main="Permutational distribution of the t-statistic")
abline(v=T0, col=col.3 ,lwd=3)
legend(10, max(p$counts), 
       legend=c("Distribution of the t-statistic", "Observed value of the t-statistic"),
       col=c(1, col.3), 
       lty=c(1,1),
       lwd=c(2,2),
       cex=0.9)
dev.off()


par(mfrow=c(1,2))
hist(T_vec,xlim=range(c(T_vec,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_vec),xlim=range(c(T_vec,T0)))
abline(v=T0,col=3,lwd=4)

sum(T_vec>=T0)/B
T_vec <- abs(T_vec)

sum(T_vec>=T0)/B

