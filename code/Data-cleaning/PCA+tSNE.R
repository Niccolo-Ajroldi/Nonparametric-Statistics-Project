
library(dplyr)

setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

#### data ####------------------------------------------------------------

load("data/data_pools.Rdata")

df <- merged_data
names(df)

rm("merged_data")

#### cleaning ####--------------------------------------------------------

# target variable
perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
y <- perc.joe - perc.don
df$y <- y

# add democratic margin in 2016
df$diff.2016 <- df$percentage16_Hillary_Clinton - df$percentage16_Donald_Trump
df$bachelor_or_more <- df$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18
df$pop_density <- log(df$pop_density)

# covariates
x.names <- c(
  'diff.2016',
  'polls2020',
  'total_votes20',
  'IncomePerCap',
  'Women',
  'White',
  'Black',
  'Hispanic',
  'Native',
  'Asian',
  'bachelor_or_more',
  'pop_density',
  'Construction',
  'perc_poveri',
  'RUCC_code'
)

# select only the above regressors
x  <- dplyr::select(df, all_of(x.names))
df <- dplyr::select(df, all_of(c("y",x.names)))

# rows indexes
rows.joe <- which(df$y >= 0)
rows.don <- which(df$y <  0)

#### PCA ####--------------------------------------------------------

# scale data for PCA
x.scaled <- scale(x)

# perform pca
pca <- princomp(x.scaled, scores=T)
summary(pca)
plot(pca)

# pca scores
scores <- pca$scores

# 1 pc
class <- ifelse(df$y>=0, "blue", "red")
plot(scores[,1], col=class)

# 2 pc
png(file = "Pics/PCA.png", width = 6000, height = 5000, units = "px", res = 800)
layout(1)
plot(scores[,1:2], 
     xlab="1st PC", 
     ylab="2nd PC", 
     main="PCA scores", 
     xlim=c(-4,16))
abline(h=0, v=0, lty=2, col='grey')
points(scores[rows.don,1], scores[rows.don,2], col="firebrick2",  pch=1)
points(scores[rows.joe,1], scores[rows.joe,2], col="dodgerblue1", pch=1)
legend("topright",
       legend=c("Democratic counties", "Republican counties"),
       col=c("dodgerblue1", "firebrick2"), 
       pch=c(1,1),
       cex=0.9)
dev.off()

# cumulative proportion of explained variance
png(file = "Pics/PCA_Variance.png", width = 6000, height = 5000, units = "px", res = 800)
cs <- cumsum(pca$sd^2)/sum(pca$sd^2)
cs[2]
col.3 <- "#F5A700"
plot(cumsum(pca$sd^2)/sum(pca$sd^2), 
     type='b', 
     axes=F, 
     xlab='Number of PCs', 
     ylab='Contribution to the total variance', 
     main="Cumulative proportion of variance",
     ylim=c(0,1))
abline(h=1, col=col.3)
abline(h=cs[2], lty=2, col=col.3)
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(df),labels=1:ncol(df),las=2)
dev.off()

# loadings first pc
load <- pca$loadings[1:ncol(x),1:ncol(x)]

#View(load)
comp.1 <- load[,1]
comp.2 <- load[,2]
comp.1
comp.2

#### t-sne ####-------------------------------------------------------

library(Rtsne)
colors = c("red","blue")
names(colors) = unique(class)

tsne <- Rtsne(x, dims = 2, perplexity=5, verbose=TRUE, max_iter = 5000, check_duplicates = FALSE)
plot(tsne$Y, col=class,  main="tsne, perplexity=5")


#### depth ####-------------------------------------------------------

# exlporatory analysis:
# color previous plots by depth scores

# depths
library(DepthProc)
my.method <- "Tukey"
x.depths <- depth(x,method=my.method)
hist(x.depths)

# rank observations by depth
colfunc <- colorRampPalette(c("grey35", "yellow"))
ordine <- rank(x.depths, ties="first")
colors <- colfunc(nrow(x))[ordine] # high depth -> high rank -> high order -> more yellow

# 2 pc
plot(scores[,1:2])
abline(h=0, v=0, lty=2, col='grey')
points(scores[,1], scores[,2], col=colors, pch=1)

# tsne
colfunc <- colorRampPalette(c("grey35", "yellow"))
ordine <- rank(x.depths, ties="first")
colors <- colfunc(nrow(x))[ordine] # high depth -> high rank -> high order -> more yellow
plot(tsne$Y, col=colors,  main="tsne, perplexity=5", pch=19)





