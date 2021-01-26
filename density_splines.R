rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####
df <- read.csv('data/merged_data.csv')

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

x <- df[,66]
y <- diff.perc*100

rd <- which(x < 5000)

col.3 <- "#F5A700"


#### SMOOTHING SPLINES ####--------------------------------------------------------------------------


library(splines)

# natural smoothing splines
#knots=seq(25,60,by=10)
#boundary.knots=c(20,65)

fit=smooth.spline(x,y,cv=F)
plot(x ,y,cex =.5, col =" darkgrey ")
lines(fit,col=col.3,lwd=2)
fit$lambda
fit$df

fit=smooth.spline(x,y,lambda=1e-10)
plot(x ,y,cex =.5, col =" darkgrey ")
lines(fit,col=col.3,lwd=2)

fit=smooth.spline(x,y,lambda=1e-5)
plot(x ,y,cex =.5, col =" darkgrey ")
lines(fit,col=col.3,lwd=2)

fit=smooth.spline(x,y,lambda=1e10)
plot(x ,y,cex =.5, col =" darkgrey ")
lines(fit,col=col.3,lwd=2)


# Without outliers
x.r <- x[rd]
y.r <- y[rd]

fit=smooth.spline(x.r,y.r,cv=F)
plot(x.r,y.r,cex =.5, col =" darkgrey ")
lines(fit,col="blue",lwd=2)
fit$lambda
fit$df

fit=smooth.spline(x.r,y.r,lambda=1e-10)
plot(x.r,y.r,cex =.5, col =" darkgrey ")
lines(fit,col=col.3,lwd=2)

fit=smooth.spline(x.r,y.r,lambda=1e-5)
plot(x.r,y.r,cex =.5, col =" darkgrey ")
lines(fit,col=col.3,lwd=2)

fit=smooth.spline(x.r,y.r,lambda=1e-2)
plot(x.r,y.r,cex =.5, col =" darkgrey ")
lines(fit,col=col.3,lwd=2)


#### NATURAL REGRESSION SPLINES ####------------------------------------------------------------

# dof=20 --> QUANTI NODI??
boundary.knots=c(10,10000)

tiff(file = "Density_splines.tiff", width = 6000, height = 5000, units = "px", res = 800)
model_cut=lm(y ~ ns(x, df=20, Boundary.knots=boundary.knots))
x.grid=seq(range(x)[1],range(x)[2],by=0.5)
preds=predict(model_cut,list(x=x.grid),se=T)
#se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(x, y,
     xlim=range(x.grid),
     main="Natural regression spline",
     xlab="Population density [people/square mile]",
     ylab="Biden-Trump % of votes",
     ylim=c(-100,100),
     #pch=19,
     cex=0.75,
     col="darkgrey"
)
lines(x.grid,preds$fit ,lwd =2, col =col.3)
dev.off()

# MODELLO LINEARE
tiff(file = "Density_linear_model.tiff", width = 6000, height = 5000, units = "px", res = 800)
plot(y~x,
     main="Classical Linear Regression",
     xlab="Population density [people/square mile]",
     ylab="Biden-Trump % of votes",
     ylim=c(-100,100),
     #pch=19,
     cex=0.75,
     col="darkgrey"
)
model.0 <- lm(y~x)
abline(model.0, col=col.3, lwd=2, lty=1)
# prediction bands
#x.grid=seq(range(x)[1],range(x)[2],by=0.5)
#preds=predict(model.0,list(x=x.grid),se=T)
#se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit) # wrong because normal assumption does not hold
#lines(x.grid,preds$fit ,lwd =2, col=col.3)
#matlines (x.grid ,se.bands ,lwd =1, col =col.3,lty =3)
# legend
#legend("bottomright",
#       legend=c("Linear regression line", "95% prediction bands"),
#       col=c(col.3,col.3), 
#       lty=c(1,3),
#       lwd=c(2,2),
#       cex=0.75)
dev.off()

#### NATURAL REGRESSION SPLINES ####------------------------------------------------------------

# dof=20 --> QUANTI NODI??
x <- log10(x)
#boundary.knots=c(10,10000)

tiff(file = "Density_LOG_splines.tiff", width = 6000, height = 5000, units = "px", res = 800)
model_cut=lm(y ~ ns(x, df=20))
x.grid=seq(range(x)[1],range(x)[2],by=0.5)
preds=predict(model_cut,list(x=x.grid),se=T)
plot(x, y,
     xlim=range(x.grid),
     main="Natural regression spline",
     xlab="Population density [people/square mile]",
     ylab="Biden-Trump % of votes",
     ylim=c(-100,100),
     #pch=19,
     cex=0.75,
     col="darkgrey"
)
lines(x.grid,preds$fit ,lwd =2, col =col.3)
dev.off()


