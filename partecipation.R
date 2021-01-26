rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####
df <- read.csv('data/merged_data.csv')

#### stati trump vs stati biden ####
rows.joe <- which(df$votes20_Joe_Biden > df$votes20_Donald_Trump)
rows.don <- which(df$votes20_Joe_Biden < df$votes20_Donald_Trump)

perc.joe <- df$percentx20_Joe_Biden
perc.don <- df$percentx20_Donald_Trump
diff.perc <- perc.joe - perc.don

#### education ####
x <- df[,58:61]
names(x)