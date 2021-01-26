
rm(list=ls())
cat("\014")
setwd("D:/Poli/Corsi/NPS/ProjNPS")

#### data ####
df <- read.csv('data/merged_data.csv')

#### stati trump vs stati biden ####
rows.joe <- which(df$votes20_Joe_Biden > df$votes20_Donald_Trump)
rows.don <- which(df$votes20_Joe_Biden < df$votes20_Donald_Trump)

rows.hilary <- which(df$votes16_Hillary_Clinton > df$votes16_Donald_Trump)
rows.don16  <- which(df$votes16_Hillary_Clinton < df$votes16_Donald_Trump)

# contee che han cambiato colore
rows.blue.red <- which(rows.hilary %in% rows.don) # 28
rows.red.blue <- which(rows.don16  %in% rows.joe) # 54

#### percentuali ####
perc.joe <- df$percentage20_Joe_Biden
perc.don <- df$percentage20_Donald_Trump
diff.perc <- perc.joe - perc.don
perc.hilary <- df$percentage16_Hillary_Clinton
perc.don16  <- df$percentage16_Donald_Trump
diff.perc <- perc.hilary - perc.don16

# c'è stato un incremento % di votanti in queste contee?










## dove c'è stato un aumento significativo di voti, a chi sono andati?

# differenza percentuale di votanti
votes.16 <- df$total_votes16
votes.20 <- df$total_votes20
diff.votes <- votes.20 - votes.16
plot(diff.votes)
plot(diff.votes, diff.perc)

which(diff.votes > 1000000)
df$county[176]
df[176,]$total_votes16 # ?BOH! 3.544.000
df[176,]$total_votes20
df[176,]$percentage16_Donald_Trump
df[176,]$percentage16_Hillary_Clinton















