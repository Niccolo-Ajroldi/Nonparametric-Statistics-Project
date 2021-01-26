
rm(list=ls())
cat("\014")

#### data ####
df1 <- read.csv('data/county_statistics.csv')
df <- na.omit(df1)

rm(df1)

#### stati trump vs stati biden ####
names(df)
attach(df)

rows.joe <- which(df$votes20_Joe_Biden > df$votes20_Donald_Trump)
rows.don <- which(df$votes20_Joe_Biden < df$votes20_Donald_Trump)
