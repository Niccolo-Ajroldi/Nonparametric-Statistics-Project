
setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

#_______________________________________________________________________________________________
#### data ####

df1 <- read.csv('data/county_statistics.csv')
df <- na.omit(df1)
df <- df[,-1]
rm(df1)

FIPS.df <- read.csv('data/FIPS.csv', colClasses = c(FIPS = "character"))
rural.urban.df <- read.csv('data/rural_urban_codes_2013.csv', colClasses = c(FIPS = "character"))
education <- read.csv('data/education_2014_2018.csv', colClasses = c(FIPS = "character"))
poverty <- read.csv('data/poverty_2018.csv', colClasses = c(FIPS = "character"))
area <- read.csv('data/county_area.csv', colClasses = c(FIPS = "character"))

#_______________________________________________________________________________________________
#### merging our data w/ FIPS code ####

d <- merge(FIPS.df, df, by=c("state","county"))
write.csv(d,"data\\county_statistics.csv_with_FIPS.csv", row.names = FALSE)

# rural urban code
d <- merge(d, rural.urban.df[,-c(2,3)], by="FIPS")
# education
d <- merge(d, education[,-c(2,3)], by="FIPS")
# poverty
d <- merge(d, poverty[,-c(2,3)], by="FIPS")
# county area
d <- merge(d, area, by="FIPS")
d$pop_density <- d$TotalPop/d$county_area_sq_mi

#_______________________________________________________________________________________________
#### SAVE ####

write.csv(d,"data\\merged_data.csv", row.names = FALSE)



