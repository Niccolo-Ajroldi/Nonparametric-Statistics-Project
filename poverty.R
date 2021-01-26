rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####
df <- read.csv('data/merged_data.csv')

#### stati trump vs stati biden ####
rows.joe <- which(df$votes20_Joe_Biden > df$votes20_Donald_Trump)
rows.don <- which(df$votes20_Joe_Biden < df$votes20_Donald_Trump)

perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don

#### education ####
x <- df[,c(63,64)]
names(x)

x11()
par(mfrow=c(1,2))
plot(100*perc.joe ~ x[,1], main="", ylab="% Joe Biden", xlab="% poveri")
plot(100*perc.joe ~ x[,2], main="", ylab="% Joe Biden", xlab="estimated median household income")


#### test poveri ####