rm(list=ls())

library(readxl)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(MASS)
library(randomForest)
library(ranger)
library(rfinterval)
library(gganimate)
library(mgcv)
library(leaps)
library(usmap)
library(caret)
library(dataRetrieval)
library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
dir <- "C:/Users/Sasa/Documents/Blog/09_police_deaths/"
input <- paste0(dir, "input/")
output <- paste0(dir, "output/")
 
dat <- read_excel(paste0(input, "police_deaths.xlsx"))
pop_0_9 <- read_excel(paste0(input, "pop_00_10.xls"), skip = 8)
pop_10_19 <- read_excel(paste0(input, "pop_10_19.xlsx"), skip = 8)
zip_county <- read_csv(paste0(input, "zip_county.csv"))
minority <- read_csv(paste0(input, "minority_share.csv"), skip = 1)
population <- read_excel(paste0(input, "PopulationEstimates.xls"), skip = 2)
health <- read.csv(paste0(input, "health_ineq_online_table_12.csv"))
election <- read.csv(paste0(input, "2016_US_County_Level_Presidential_Results.csv"))
income <- read_excel(paste0(input, "Unemployment.xls"), skip = 4)
race_county <- read.csv(paste0(input, "county_ACSDP5Y2018.DP05_data_with_overlays_2020-04-11T185628.csv"), skip = 1)


income <- income %>%
  dplyr::select(FIPS, Median_Household_Income_2018) %>%
  mutate(FIPS = as.numeric(FIPS))


election <- election %>%
  rename(FIPS = combined_fips) %>%
  dplyr::select(FIPS, per_dem)

health <- health %>%
  rename(health_name = county_name, FIPS = cty, city = intersects_msa) %>%
  mutate(health_name = as.character(health_name),
         FIPS = as.numeric(FIPS),
         city = as.numeric(city)) %>%
  dplyr::select(FIPS, csa_name) %>%
  filter(!is.na(csa_name) & csa_name != "")


population <- population %>%
  dplyr::select(FIPS, POP_ESTIMATE_2018) %>%
  mutate(FIPS = as.numeric(FIPS))

colnames(pop_0_9) <- c("state", "ignore", paste0("year", 2000:2009), "ignore2", "ignore3")
pop_0_9 <- pop_0_9 %>%
  dplyr::select(-ignore, -ignore2, -ignore3) %>%
  mutate(state = gsub("\\.", "", state)) %>%
  filter(state %in% state.name)

colnames(pop_10_19) <- c("state", "ignore", "ignore2", paste0("year", 2010:2019))
pop_10_19 <- pop_10_19 %>%
  dplyr::select(-ignore, -ignore2) %>%
  mutate(state = gsub("\\.", "", state)) %>%
  filter(state %in% state.name)

pop <- left_join(pop_0_9, pop_10_19, by = "state")
t <- statepop %>%
  dplyr::select(abbr, full)
pop <- left_join(pop, t, by = c("state" = "full"))
pop <- pop %>%
  dplyr::select(-state)

pop <- pop %>%
  pivot_longer(cols = c(paste0("year", 2000:2019)), names_to = "year", values_to = "pop") %>%
  mutate(year = gsub("year", "", year), 
         year = as.numeric(year),
         merge_id = paste0(abbr, year))

dat <- dat %>%
  rename(subject_age = `Subject's age`, 
         subject_gender = `Subject's gender`,
         subject_race = `Subject's race`,
         subject_race_imp = `Subject's race with imputations`,
         date = `Date of injury resulting in death (month/day/year)`,
         state = `Location of death (state)`,
         city = `Location of death (city)`,
         zip = `Location of death (zip code)`,
         cause_of_death = `Cause of death`,
         latitude = Latitude, 
         longitude = Longitude) %>%
  dplyr::select(subject_age, subject_gender, subject_race_imp, 
                date, city, zip, state, cause_of_death, latitude, longitude)

dat <- dat %>%
  mutate(subject_age = as.numeric(subject_age), 
         date = as.Date(date),
         zip = as.character(zip), 
         subject_race_imp = ifelse(subject_race_imp == "African-American/Black", "black", 
                            ifelse(subject_race_imp == "European-American/White" | subject_race_imp == "European American/White", "white", 
                            ifelse(subject_race_imp == "Hispanic/Latino" | subject_race_imp ==  "HIspanic/Latino", "latino", 
                            ifelse(subject_race_imp == "NA" | subject_race_imp == "Race unspecified", NA, 
                            ifelse(subject_race_imp == "Asian/Pacific Islander", "asian",
                            ifelse(subject_race_imp == "Native American/Alaskan", "native", 
                            ifelse(subject_race_imp == "Other Race", "other", 
                            ifelse(subject_race_imp == "Middle Eastern", "middle_eastern", NA)))))))))

zip_county <- zip_county %>%
  rename(zip = ZIP, county = COUNTY, ratio = TOT_RATIO) 

zip_county_sub <- zip_county %>%
  mutate(id = 1:n()) %>%
  group_by(zip) %>%
  summarize(ratio = max(ratio),
            id = max(id))
zip_county <- zip_county %>%
  mutate(id = 1:n()) %>%
  filter(id %in% zip_county_sub$id) %>%
  dplyr::select(zip, county)

race_names <- function(df) {
  x <- grep("Estimate!!RACE!!Total population!!One race!!Black or African American", colnames(df))[1]
  colnames(df)[x] <- "black"
  
  x <- grep("Estimate!!RACE!!Total population!!One race!!Asian", colnames(df))[1]
  colnames(df)[x] <- "asian"
  
  x <- grep("Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino", colnames(df))[1]
  colnames(df)[x] <- "latino"
  
  x <- grep("Estimate!!RACE!!Total population!!One race!!American Indian and Alaska Native", colnames(df))[1]
  colnames(df)[x] <- "native"
  
  x <- grep("Estimate!!RACE!!Total population!!One race!!White", colnames(df))[1]
  colnames(df)[x] <- "white"
  return(df)
}

race_names_county <- function(df) {
  x <- grep("Estimate..RACE..Total.population..One.race..Black.or.African.American", colnames(df))[1]
  colnames(df)[x] <- "black"
  
  x <- grep("Estimate..RACE..Total.population..One.race..Asian", colnames(df))[1]
  colnames(df)[x] <- "asian"
  
  x <- grep("Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Hispanic.or.Latino", colnames(df))[1]
  colnames(df)[x] <- "latino"
  
  x <- grep("Estimate..RACE..Total.population..One.race..American.Indian.and.Alaska.Native", colnames(df))[1]
  colnames(df)[x] <- "native"
  
  x <- grep("Estimate..RACE..Total.population..One.race..White", colnames(df))[1]
  colnames(df)[x] <- "white"
  return(df)
}

minority <- race_names(minority)
minority <- minority %>%
  dplyr::select(black, asian, latino, native, white)
minority <- t(minority)
minority <- data.frame(cbind(rownames(minority), minority))
colnames(minority) <- c("race", "min_pop")

race_county <- race_names_county(race_county)

colnames(race_county)[1] <- "FIPS"
race_county <- race_county %>%
  dplyr::select(FIPS, black, asian, latino, native, white) %>%
  mutate(index = regexpr("US", FIPS),
         FIPS = as.character(FIPS),
         FIPS = substr(FIPS, index+2, nchar(FIPS)), 
         FIPS = as.numeric(FIPS)) %>%
  dplyr::select(FIPS, black, asian, latino, native, white) %>%
  rename(black_county_pop = black, 
         asian_county_pop = asian, 
         latino_county_pop = latino, 
         native_county_pop = native, 
         white_county_pop = white)



dat <- dat %>%
  mutate(merge_id = paste0(state, year(date)))
 
csa_pop <- left_join(population, health, by = "FIPS") %>%
  left_join(race_county, by = "FIPS")
csa_pop <- csa_pop %>%
  filter(!is.na(csa_name)) %>%
  group_by(csa_name) %>%
  summarize(csa_pop = sum(POP_ESTIMATE_2018),
            csa_pop_black = sum(black_county_pop, na.rm = T), 
            csa_pop_asian = sum(asian_county_pop, na.rm = T), 
            csa_pop_latino = sum(latino_county_pop, na.rm = T), 
            csa_pop_native = sum(native_county_pop, na.rm = T), 
            csa_pop_white = sum(white_county_pop, na.rm = T)) %>%
  mutate(csa_pop_black = ifelse(is.na(csa_pop_black), 0, csa_pop_black), 
         csa_pop_asian = ifelse(is.na(csa_pop_asian), 0, csa_pop_asian), 
         csa_pop_latino = ifelse(is.na(csa_pop_latino), 0, csa_pop_latino), 
         csa_pop_native = ifelse(is.na(csa_pop_native), 0, csa_pop_native), 
         csa_pop_white = ifelse(is.na(csa_pop_white), 0, csa_pop_white))
 
boston_name <- as.character(health$csa_name[grep("Boston", health$csa_name)][1])



dat <- left_join(dat, pop, by = "merge_id")
dat <- left_join(dat, minority, by = c("subject_race_imp" = "race"))
dat <- left_join(dat, zip_county, by = "zip")
dat <- left_join(dat, population, by = c("county" = "FIPS"))
dat <- left_join(dat, health, by = c("county" = "FIPS"))

dat <- dat %>%
  mutate(csa_name = as.character(csa_name),
         csa_name = ifelse(city == "Boston" & state == "MA", boston_name, csa_name))

dat <- left_join(dat, csa_pop, by = "csa_name")
dat <- left_join(dat, election, by = c("county" = "FIPS"))
dat <- left_join(dat, income, by = c("county" = "FIPS"))
dat <- left_join(dat, race_county, by = c("county" = "FIPS"))

saveRDS(dat, paste0(input, "master.rds"))
saveRDS(csa_pop, paste0(input, "csa_pop.rds"))
