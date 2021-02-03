
setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

#### data ####-----------------------------------------------------------------------

df <- read.csv('data/merged_data.csv')

x <- df[,21:26]
names(x)

rows.joe <- which(df$votes20_Joe_Biden > df$votes20_Donald_Trump)
rows.don <- which(df$votes20_Joe_Biden < df$votes20_Donald_Trump)

x.joe <- x[rows.joe,]
x.don <- x[rows.don,]
n1 <- dim(x.joe)[1]
n2 <- dim(x.don)[1]
n  <- n1 + n2

#### divide by etnies ####-----------------------------------------------------------

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


#### plot ####------------------------------------------------------------------------

# plot

etnie.ordered <- factor(etnie, levels = c("Asian", "Black", "Native", "Hispanic", "White"))
f <- colorRampPalette(c("blue", "red"))

png(file = "Pics/Etnie_boxplot.png", width = 6000, height = 5000, units = "px", res = 800)
boxplot(diff.perc*100~etnie.ordered,
        #data=airquality,
        main="Biden percentage margin vs dominant etnicity",
        xlab="Dominant ethnicity",
        ylab="Biden-Trump % of votes",
        #col=c("gold2","darkorange","cornflowerblue","firebrick2","aquamarine3"),
        col=f(5),
        ylim=c(-100,100)
        #border="darkgray"
)
abline(h=0, lty=3, col="darkgray", lwd=2)
dev.off()

# Basic violin plot
library(ggplot2)
data <- data.frame(diff.perc,etnie)
ggplot(data, aes(x=etnie, y=diff.perc)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(title="Plot of length  by dose",x="Dose (mg)", y = "Length")+
  geom_boxplot(width=0.1)+
  theme_classic()
dp <- ggplot(data, aes(x=etnie, y=diff.perc, fill=etnie)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of length  by dose",x="Dose (mg)", y = "Length")
dp + theme_classic()

# Continusous colors
dp + scale_fill_brewer(palette="Blues") + theme_classic()
# Discrete colors
dp + scale_fill_brewer(palette="Dark2") + theme_minimal()
# Gradient colors
dp + scale_fill_brewer(palette="RdBu") + theme_minimal()


#### ANOVA ####-----------------------------------------------------------------------

# anova
fit <- aov(diff.perc ~ etnie)
summary(fit)

# how is classical anova performing?
col.3 <- "#F5A700"

png(file = "Pics/Etnies_qqnorm.png", width = 6000, height = 5000, units = "px", res = 800)
par(mfrow=c(1,2))
hist(fit$residuals,
     col=col.3,
     border="black",
     prob = TRUE,
     xlab = "Residuals",
     main = "Residuals of classical anova")
lines(density(fit$residuals), # density plot
      lwd = 2, # thickness of line
      col = "orange3")
q <- qqnorm(fit$residuals, main="Normal Q-Q Plot")
text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)-0.05,
     pos = 4, "Shapiro-Wilk test \np-value <.001", col = 1, font = 3)
qqline(fit$residuals, col=col.3, lwd=2)

dev.off()

shapiro.test(fit$residuals)
table(etnie)

#### PERMUTATIONAL ANOVA  ####----------------------------------------------------------

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

# plot
png(file = "Pics/Etnie_test.png", width = 6000, height = 5000, units = "px", res = 800)
p <- hist(T_vec,
          xlim=range(c(T_vec,T0)),
          breaks=20, 
          xlab="Test statistic",
          main="Permutational distribution of the F statistic")
abline(v=T0, col=col.3 ,lwd=3)
legend(10, max(p$counts), 
       legend=c("Distribution of the F statistic", "Observed value of the F statistic"),
       col=c(1, col.3), 
       lty=c(1,1),
       lwd=c(2,2),
       cex=0.9)
dev.off()

# p-value
p_val <- sum(T_vec>=T0)/B
p_val

