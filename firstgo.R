
rm(list=ls())
cat("\014")

setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####

df1 <- read.csv('data/county_statistics.csv')
df2 <- read.csv('data/trump_biden_polls.csv')
df3 <- read.csv('data/trump_clinton_polls.csv')

df <- na.omit(df1)

dim(df)
names(df)

# dalla colonna 16 in poi abbiamo le stats sulle counties
x <- df[,16:51]

# counties in which Biden won
rows.joe <- which(df$votes20_Joe_Biden >= df$votes20_Donald_Trump)
rows.don <- which(df$votes20_Joe_Biden < df$votes20_Donald_Trump)

#### pca ####

pca <- princomp(x, scores=T)
summary(pca)
plot(pca)
# pca scores
scores <- pca$scores
# 1 pc
class <- ifelse(df$votes20_Joe_Biden >= df$votes20_Donald_Trump, "blue", "red")
plot(scores[,1], col=class)
# 2 pc
plot(scores[,1:2])
abline(h=0, v=0, lty=2, col='grey')
points(scores[rows.joe,1], scores[rows.joe,2], col="blue")
points(scores[rows.don,1], scores[rows.don,2], col="red")

#### t-sne ####

# library(Rtsne)
# colors = c("red","blue")
# names(colors) = unique(class)
# 
# tsne <- Rtsne(x, dims = 2, perplexity=5, verbose=TRUE, max_iter = 5000, check_duplicates = FALSE)
# plot(tsne$Y, col=class,  main="tsne, perplexity=5")

#### 2 groups ####

x.joe <- x[rows.joe,]
x.don <- x[rows.don,]


#### covid cases test ####
y.joe <- x.joe$cases
y.don <- x.don$cases

T.obs <- median(covid.joe) - median(covid.don)
T.obs

