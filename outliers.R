
library(dplyr)
library(ModelMetrics)

setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

#### data ####--------------------------------------------------------

load("data/data_pools.Rdata")

df <- merged_data
names(df)

rm("merged_data")

# target variable
perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
y <- perc.joe - perc.don
df$y <- y

# add democratic margin in 2016
df$diff.2016 <- df$percentage16_Hillary_Clinton - df$percentage16_Donald_Trump
df$bachelor_or_more <- df$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18

# regressor names
x.names <- c(
  "diff.2016", # ?
  "polls2020",
  "total_votes20", # ?
  "Men",
  "Hispanic",                                                             
  "White",
  "Black",
  "Native",
  "Asian",
  "Income",
  "RUCC_code",
  "bachelor_or_more",
  "perc_poveri",
  "pop_density"
)

# select only the above regressors
x  <- dplyr::select(df, all_of(x.names))
df <- dplyr::select(df, c("y",x.names))

# preprocessing
x$pop_density <- log(x$pop_density)

# rows indexes
rows.joe <- which(df$y >= 0)
rows.don <- which(df$y <  0)

#### PCA ####--------------------------------------------------------

# sclae data
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
plot(scores[,1:2])
abline(h=0, v=0, lty=2, col='grey')
points(scores[rows.don,1], scores[rows.don,2], col="red")
points(scores[rows.joe,1], scores[rows.joe,2], col="blue", pch=1)

arrows(0,0,5,5 ,angle = 10)

# loadings first pc
load <- pca$loadings[1:ncol(x),1:ncol(x)]
#View(load)
comp.1 <- load[,1]
comp.2 <- load[,2]

comp.diag <- comp.1 + comp.2
View(comp.diag)

#### t-sne ####-------------------------------------------------------

library(Rtsne)
colors = c("red","blue")
names(colors) = unique(class)

tsne <- Rtsne(x, dims = 2, perplexity=5, verbose=TRUE, max_iter = 5000, check_duplicates = FALSE)
plot(tsne$Y, col=class,  main="tsne, perplexity=5")


#### depth ####-------------------------------------------------------

# prova
a.depth <- c(23,1,4)
ordine <- rank(a.depth, ties="first")
ordine # high depth -> high rank -> high ordine -> more white
colfunc <- colorRampPalette(c("black", "white"))
colors <- colfunc(3)[ordine]
plot(rep(1,3),col=colors,pch=19,cex=3)

library(DepthProc)
my.method <- "Tukey"
x.depths <- depth(x,method=my.method)
hist(x.depths)

#
colfunc <- colorRampPalette(c("grey35", "yellow"))
ordine <- rank(x.depths, ties="first")
colors <- colfunc(nrow(x))[ordine] # high depth -> high rank -> high order -> more yellow

# 2 pc
plot(scores[,1:2])
abline(h=0, v=0, lty=2, col='grey')
points(scores[,1], scores[,2], col=colors, pch=1)


# 2 pc
colors <- ifelse(x.depths<0.001, "blue", "grey")
plot(scores[,1:2])
abline(h=0, v=0, lty=2, col='grey')
points(scores[,1], scores[,2], col=colors, pch=1)

#
colfunc <- colorRampPalette(c("grey35", "yellow"))
ordine <- rank(x.depths, ties="first")
colors <- colfunc(nrow(x))[ordine] # high depth -> high rank -> high order -> more yellow
plot(tsne$Y, col=colors,  main="tsne, perplexity=5", pch=19)





