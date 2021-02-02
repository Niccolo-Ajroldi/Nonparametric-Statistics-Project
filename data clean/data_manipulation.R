
setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

library("readxl")

# stati
statez.data <- read.csv('data/states.csv')
statez <- statez.data$State
statez <- as.character(statez)
statez <- c("United States", statez)
statez <- as.factor(statez)
statez <- statez


#_______________________________________________________________________________________________
#### EDUCATION ####

education <- read_excel("data/new_data/Education.xls", range="A5:AU3288")
names(education)[names(education) == "Area name"] <- "county"
names(education)[names(education) == "State"] <- "state"
names(education)[names(education) == "FIPS Code"] <- "FIPS"

# rimuovo lo *stato* DC
education <- education[-which(education$state=="DC")[1],]

# devo toglierli United STates e tutti gli stati 
# (tranne DC che ho già tolto a mano ed è il nome della contea)
rows_to_remove <- which(education$county %in% statez[-10])
education$county[rows_to_remove] # ok
education <- education[-rows_to_remove,] # 3231 contee, boh?

# tolgo pure Porto Rico, perchè tanto non votano
rows_PC <- which(education$state=="PR")
education <- education[-rows_PC,] # 3153 contee

# mo devo togliere gli ultimi 7 caratteri ad ogni contea
str <-"Autauga County"
substr(str, 1, nchar(str)-7)

str=education$county
education$county <- substr(str, 1, nchar(str)-7)

# e rimetto a posto district of columbia
education$county[which(education$state=="DC")]
education$county[which(education$state=="DC")] <- "District of Columbia"
education$county[which(education$state=="DC")]


# SUBSETS
education.2014.2018 <- education[,c(1:3,40:47)]
education.1970.2018 <- education[,-c(4:7)]
rural.urban.codes.2013 <- education[,c(1:3,6,7)]
FIPS_state_county <- education[,c(1,2,3)]

# names
names(rural.urban.codes.2013)[4] <- "RUCC_code"
names(rural.urban.codes.2013)[5] <- "UBI_code"

# save
write.csv(education.2014.2018,"data\\education_2014_2018.csv", row.names = FALSE)
write.csv(education.1970.2018,"data\\education_1970_2018.csv", row.names = FALSE)
write.csv(rural.urban.codes.2013,"data\\rural_urban_codes_2013.csv", row.names = FALSE)
write.csv(FIPS_state_county,"data\\FIPS.csv", row.names = FALSE)

#_______________________________________________________________________________________________
#### POVERTY ####

rm(list=ls())
cat('\014')

# stati
statez.data <- read.csv('data/states.csv')
statez <- statez.data$State
statez <- as.character(statez)
statez <- c("United States", statez)
statez <- as.factor(statez)
statez <- statez

poverty <- read_excel("data/new_data/PovertyEstimates.xls", sheet=1, range="A5:AB3198")
names(poverty)[names(poverty) == "Area_name"] <- "county"
names(poverty)[names(poverty) == "Stabr"] <- "state"
names(poverty)[names(poverty) == "FIPStxt"] <- "FIPS"

# rimuovo lo *stato* DC
poverty <- poverty[-which(poverty$state=="DC")[1],]

# devo toglierli United STates e tutti gli stati
rows_to_remove <- which(poverty$county %in% statez[-10])
poverty$county[rows_to_remove] # ok
poverty <- poverty[-rows_to_remove,]

# tolgo pure Porto Rico, perchè tanto non votano
which(poverty$state=="PR") # non c'è PC

# mo devo togliere gli ultimi 7 caratteri ad ogni contea
str=poverty$county
poverty$county <- substr(str, 1, nchar(str)-7)

# e rimetto a posto district of columbia
poverty$county[which(poverty$state=="DC")]
poverty$county[which(poverty$state=="DC")] <- "District of Columbia"
poverty$county[which(poverty$state=="DC")]

# keep only significant variables
cols.name <- c("FIPS", "state", "county","POVALL_2018", "PCTPOVALL_2018", "MEDHHINC_2018")
poverty <- poverty[, which(names(poverty) %in% cols.name)]

# change names
names(poverty)[names(poverty) == "POVALL_2018"] <- "tot_poveri"
names(poverty)[names(poverty) == "PCTPOVALL_2018"] <- "perc_poveri"
names(poverty)[names(poverty) == "MEDHHINC_2018"] <- "estimated_median_household_income"

write.csv(poverty,"data\\poverty_2018.csv", row.names = FALSE)

#_______________________________________________________________________________________________
#### COUNTY AREA ####

area <- read_excel("data/LND01.xls")
area <- area[,c(2,24)]
names(area)[1] <- "FIPS"
names(area)[2] <- "county_area_sq_mi"
write.csv(area,"data\\county_area.csv", row.names = FALSE)


