rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####
df <- read.csv('data/merged_data.csv')

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

x <- df$pop_density
y <- diff.perc*100
x <- log10(x)

rd <- which(df$pop_density < 5000)
col.3 <- "#F5A700"

#col = ifelse(df$percentage20_Joe_Biden > df$percentage20_Donald_Trump,"darkblue","darkred")
#rd <- which(x < 5000)
#plot(100*diff.perc[rd] ~ x[rd],
#     main="Population density",
#     xlab="Population density [people/square mile]",
#     ylab="Biden-Trump % of votes",
#     ylim=c(-100,100),
#     pch=20,
#     col=col[rd]
#     )


tiff(file = "Density_lm_LOG.tiff", width = 6000, height = 5000, units = "px", res = 800)
plot(y~x,
     main="Log Population Density",
     xlab="log(Population density [people/square mile])",
     ylab="Biden-Trump % of votes",
     ylim=c(-100,100),
     #pch=19,
     cex=0.75,
     col="darkgrey"
)
model.0 <- lm(y~x)
abline(model.0, col=col.3, lwd=2, lty=1)
# prediction bands
x.grid=seq(range(x)[1],range(x)[2],by=0.5)
preds=predict(model.0,list(x=x.grid),se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit) # wrong because normal assumption does not hold
lines(x.grid,preds$fit ,lwd =2, col=col.3)
matlines (x.grid ,se.bands ,lwd =1, col =col.3,lty =3)
# legend
legend("bottomright",
       legend=c("Linear regression line", "95% prediction bands"),
       col=c(col.3,col.3), 
       lty=c(1,3),
       lwd=c(2,2),
       cex=0.75)
dev.off()

# REMOVE OUTLIERS
x.r <- x[rd]
y.r <- y[rd]
tiff(file = "Density_clean.tiff", width = 6000, height = 5000, units = "px", res = 800)
plot(y.r~x.r,
     main="Removing high density counties",
     xlab="Population density [people/square mile]",
     ylab="Biden-Trump % of votes",
     ylim=c(-100,100),
     #pch=19,
     cex=0.75,
     col="darkgrey"
)
model.0 <- lm(y.r~x.r)
abline(model.0, col=col.3, lwd=2, lty=1)
# prediction bands
x.grid=seq(range(x.r)[1],range(x.r)[2],by=0.5)
preds=predict(model.0,list(x.r=x.grid),se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit) # wrong because normal assumption does not hold
lines(x.grid,preds$fit ,lwd =2, col=col.3)
matlines (x.grid ,se.bands ,lwd =1, col =col.3,lty =3)
# legend
legend("bottomright",
       legend=c("Linear regression line", "95% prediction bands"),
       col=c(col.3,col.3), 
       lty=c(1,3),
       lwd=c(2,2),
       cex=0.75)
dev.off()


