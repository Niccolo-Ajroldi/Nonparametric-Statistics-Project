
rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#_________________________________________________________________________________________
#### data ####

df <- read.csv('data/merged_data.csv')

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

x <- as.factor(df$RUCC_code)
g <- length(levels(x))
col.3 <- "#F5A700"

#_________________________________________________________________________________________
#### plot ####

f <- colorRampPalette(c("blue", "red"))
colors <- f(9)

tiff(file = "RUCC_boxplot.tiff", width = 6000, height = 5000, units = "px", res = 800)
boxplot(diff.perc*100~x,
        #data=airquality,
        main="Biden percentage margin vs rural code",
        xlab="Rural code (high code = high rurality)",
        ylab="Biden-Trump % of votes",
        #col=plasma(9),
        col=f(9),
        ylim=c(-100,100)
        #border="darkgray"
)
abline(h=0, lty=3, col="darkgray", lwd=2)
dev.off()

table(x)

# ggplot
library(ggplot2)

# create a dataset
data <- data.frame(RUCC=x,y=100*perc.joe)

# Most basic violin chart
p <- ggplot(data, aes(x=RUCC, y=y, fill=RUCC)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()
p


#_________________________________________________________________________________________
#### anova ####

# classical anova
fit <- aov(diff.perc ~ x)
summary(fit)

table(x)

# how is classical anova performing?
col.1 <- "aquamarine3"
col.2 <- "aquamarine4"
tiff(file = "RUCC_qqnorm.tiff", width = 6000, height = 5000, units = "px", res = 800)

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


#_________________________________________________________________________________________
#### Permutational anova ####

# permutation test
T0 <- summary(fit)[[1]][1,4]
T0
B <- 10000
T_vec <- numeric(B) 
n <- length(diff.perc)

for(b in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  x.perm <- x[permutation] # permute the RUC
  fit.perm <- aov(diff.perc ~ x.perm)
  
  # Test statistic:
  T_vec[b] <- summary(fit.perm)[[1]][1,4]
}

# plot
tiff(file = "RUCC_test.tiff", width = 6000, height = 5000, units = "px", res = 800)
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


