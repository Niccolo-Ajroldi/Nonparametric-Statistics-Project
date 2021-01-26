
setwd("D:/Poli/Corsi/NPS/ProjNPS")

rm(list=ls())
cat("\014")

#### data ####
df1 <- read.csv('data/county_statistics.csv')
df <- na.omit(df1)
rm(df1)

library("readxl")
        
# stati
statez.data <- read.csv('data/states.csv')
statez <- statez.data$State
statez <- as.character(statez)
statez <- c("United States", statez)
statez <- as.factor(statez)

#### EDUCATION ####

education <- read_excel("data/new_data/Education.xls", range="A5:AU3288")
names(education)[names(education) == "Area name"] <- "county"
names(education)[names(education) == "State"] <- "state"
names(education)[names(education) == "FIPS Code"] <- "FIPS"

# devo toglierli United STates e tutti gli stati
rows_to_remove <- which(education$county %in% statez)
education$county[rows_to_remove] # ok
education.1 <- education[-rows_to_remove,] # 3231 contee, boh?

# mo devo togliere gli ultimi 7 caratteri ad ogni contea
str <-"Autauga County"
substr(str, 1, nchar(str)-7)

str=education.1$county
education.1$county <- substr(str, 1, nchar(str)-7)
write.csv(education.1,"data\\education_1970_2018.csv", row.names = FALSE)

# education.2 contiene solo i dati 2014-2018
educatio.1_2014_2018 <- education.1[,c(1:3,40:47)]
write.csv(educatio.1_2014_2018,"data\\education_2014_2018.csv", row.names = FALSE)


#### FIPS ####

FIPS_state_county <- education.1[,c(1,2,3)]
write.csv(FIPS_state_county,"data\\FIPS.csv", row.names = FALSE)

#### POVERTY ####

poverty <- read_excel("data/new_data/PovertyEstimates.xls", sheet=1, range="A5:AB3198")
names(poverty)[names(poverty) == "Area_name"] <- "county"
names(poverty)[names(poverty) == "Stabr"] <- "state"
names(poverty)[names(poverty) == "FIPStxt"] <- "FIPS"

# devo toglierli United STates e tutti gli stati
rows_to_remove <- which(poverty$county %in% statez)
poverty$county[rows_to_remove] # ok
poverty.1 <- poverty[-rows_to_remove,] # 3231 contee, boh?

# mo devo togliere gli ultimi 7 caratteri ad ogni contea
str=poverty.1$county
poverty.1$county <- substr(str, 1, nchar(str)-7)

# extract Rural and urban codes
rural_names <- c("FIPS", "state", "county","Rural-urban_Continuum_Code_2003","Urban_Influence_Code_2003")
rural_urban_codes <- poverty.1[,rural_names]
names(rural_urban_codes)[names(rural_urban_codes) == "Rural-urban_Continuum_Code_2003"] <- "rural_urban_code"
names(rural_urban_codes)[names(rural_urban_codes) == "Urban_Influence_Code_2003"] <- "urban_influence_code"
write.csv(rural_urban_codes,"data\\rural_urban_codes.csv", row.names = FALSE)

# save poverty
cols.name <- c("FIPS", "state", "county","POVALL_2018", "PCTPOVALL_2018", "MEDHHINC_2018")
names(poverty.1)[names(poverty.1) == "POVALL_2018"] <- "tot_poveri"
names(poverty.1)[names(poverty.1) == "PCTPOVALL_2018"] <- "perc_poveri"
names(poverty.1)[names(poverty.1) == "MEDHHINC_2018"] <- "estimated_median_household_income"
poverty.1 <- poverty.1[,which(names(poverty) %in% cols.name)]
write.csv(poverty.1,"data\\poverty_2018.csv", row.names = FALSE)



#### LABOR FORCE DATA ####

#labor_2020 <- read_excel("data/labor_force_data_2020.xlsx", sheet=1, range="A7:I45072")
#names(poverty)[names(poverty) == "Area_name"] <- "county"
#names(poverty)[names(poverty) == "Stabr"] <- "state"
#names(poverty)[names(poverty) == "FIPStxt"] <- "FIPS"


