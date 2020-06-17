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
library(ggmap)
library(fiftystater)
library(sf)
dir <- "C:/Users/Sasa/Documents/Blog/09_police_deaths/"
input <- paste0(dir, "input/")
output <- paste0(dir, "output/")

dat <- readRDS(paste0(input, "master.RDS"))

skeleton <- read_excel(paste0(input, "PopulationEstimates.xls"), skip = 2)
skeleton <- skeleton %>%
  dplyr::select(FIPS) %>%
  mutate(FIPS = as.numeric(FIPS))

dat <- dat %>%
  mutate(minority = ifelse(subject_race_imp == "white", 0, 1),
         white = ifelse(subject_race_imp == "white", 1, 0),
         police_killing = ifelse(cause_of_death == "Asphyxiated/Restrained" 
                           | cause_of_death == "Beaten/Bludgeoned with instrument" 
                           | cause_of_death == "Chemical agent/Pepper spray" 
                           | cause_of_death == "Gunshot" 
                           | cause_of_death == "Tasered", 1, 0),
         latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude), 
         csa_name = as.character(csa_name),
         csa_minority = csa_pop_black + csa_pop_asian + csa_pop_latino + csa_pop_native,
         black_county_pop = ifelse(is.na(black_county_pop), 0, black_county_pop),
         asian_county_pop = ifelse(is.na(asian_county_pop), 0, asian_county_pop), 
         latino_county_pop = ifelse(is.na(latino_county_pop), 0, latino_county_pop), 
         native_county_pop = ifelse(is.na(native_county_pop), 0, native_county_pop), 
         white_county_pop = ifelse(is.na(white_county_pop), 0, white_county_pop), 
         minority_county_pop = black_county_pop + asian_county_pop + latino_county_pop + native_county_pop)



dat_csa_top10 <- dat %>%
  filter(!is.na(csa_name)) %>%
  group_by(csa_name) %>%
  summarize(min_killing = sum(minority, na.rm = T),
            pop = max(csa_pop, na.rm = T),
            pop_minority = max(csa_minority, na.rm = T),
            lat = mean(latitude, na.rm = T), 
            lon = mean(longitude, na.rm = T)) %>%
  mutate(death_rate = min_killing/pop_minority) %>%
  filter(pop > 5000000) %>%
  arrange(-death_rate) %>%
  mutate(ranked = paste0(1:n(), ". ", substr(str_extract(csa_name, "([^\\-]+).\\-*"), 1, 
                                       nchar(str_extract(csa_name, "([^\\-]+).\\-*"))-1)),
         ranked = gsub("\\-", "", ranked),
         share_minority = pop_minority/pop)
  
top10 <- dat_csa_top10$csa_name

US <- st_as_sf(fifty_states, coords =c("long", "lat"), crs = 4326) %>%
  cbind(st_coordinates(.))

p_top10 <- ggplot() + 
  geom_polygon(data = US, aes(x=X, y = Y, group = group), fill="grey", alpha=0.3) +
  geom_point(data = dat_csa_top10, alpha = 0.5, aes(x=dat_csa_top10$lon, y=dat_csa_top10$lat, 
                                              size=dat_csa_top10$death_rate, color="red")) +
  ggrepel::geom_text_repel(data = dat_csa_top10, aes(x = dat_csa_top10$lon, y = dat_csa_top10$lat, label = ranked, 
                                hjust = -0.15), size = 3) + 
  # scale_size_continuous(name="Premium", trans="log",  range=c(0.1,4)) +
  theme_void() +
  ggtitle("Minority Police Killings in \n\r America's Largest Metropolitan Areas") +
  theme(
    legend.position = c("none"),
    plot.title = element_text(hjust=0.5)
  )
ggsave(paste0(output, "fatalities_by_city.png"),p_top10, width = 5, height = 4)


dat_csa_all <- dat %>%
  filter(!is.na(csa_name)) %>%
  group_by(csa_name) %>%
  summarize(min_killing = sum(minority, na.rm = T),
            pop = max(csa_pop, na.rm = T),
            pop_minority = max(csa_minority, na.rm = T),
            lat = mean(latitude, na.rm = T), 
            lon = mean(longitude, na.rm = T)) %>%
  mutate(death_rate = min_killing/pop_minority) %>%
  filter(pop > 1000000) %>%
  arrange(-death_rate) %>%
  mutate(rank =1:n(), 
         name = paste0(rank, ". ", substr(str_extract(csa_name, "([^\\-]+).\\-*"), 1, 
                                             nchar(str_extract(csa_name, "([^\\-]+).\\-*"))-1)),
         name = gsub("\\-", "", name),
         share_minority = pop_minority/pop)

get_heatmap <- function(df, subsection, name) {
  dat_csa <- df %>%
    filter(rank %in% subsection)
  
  p <- ggplot() + 
    geom_polygon(data = US, aes(x=X, y = Y, group = group), fill="grey", alpha=0.3) +
    geom_point(data = dat_csa, alpha = 0.5, aes(x=dat_csa$lon, y=dat_csa$lat, 
                                                size=dat_csa$death_rate, color="red")) +
    ggrepel::geom_text_repel(data = dat_csa, aes(x = dat_csa$lon, y = dat_csa$lat, label = name, 
                                  hjust = -0.15), size = 3) + 
    # scale_size_continuous(name="Premium", trans="log",  range=c(0.1,4)) +
    theme_void() +
    ggtitle("Minority Police Killings in \n\r America's Metropolitan Areas") +
    theme(
      legend.position = c("none"),
      plot.title = element_text(hjust=0.5)
    )
  ggsave(paste0(output, paste0(name, "fatalities_by_city.png")),p, width = 5, height = 4)
  return(p)
}


t <- get_heatmap(dat_csa_all, 1:10, "top10")
t <- get_heatmap(dat_csa_all, 11:20, "top20")
t <- get_heatmap(dat_csa_all, 21:30, "top30")
t <- get_heatmap(dat_csa_all, 31:40, "top40")
t <- get_heatmap(dat_csa_all, 41:54, "top50")

csa_pop <- readRDS(paste0(input, "csa_pop.RDS"))

dat_csa_all <- left_join(csa_pop, dat_csa_all, by = "csa_name")

dat_csa_all <- dat_csa_all %>%
  filter(csa_pop > 1000000)


###################################
### Adjustment ####################
###################################

dat_csa_white <- dat %>%
  filter(!is.na(csa_name)) %>%
  group_by(csa_name) %>%
  summarize(white_killing = sum(white, na.rm = T),
            pop = max(csa_pop, na.rm = T),
            pop_minority = max(csa_minority, na.rm = T),
            lat = mean(latitude, na.rm = T), 
            lon = mean(longitude, na.rm = T)) %>%
  mutate(death_rate_white = white_killing/pop) %>%
  filter(pop > 1000000) %>%
  arrange(-death_rate_white) %>%
  mutate(name = paste0(1:n(), ". ", substr(str_extract(csa_name, "([^\\-]+).\\-*"), 1, 
                                             nchar(str_extract(csa_name, "([^\\-]+).\\-*"))-1)),
         name = gsub("\\-", "", name),
         share_minority = pop_minority/pop)

minority <- dat_csa_all %>%
  dplyr::select(csa_name, csa_pop, min_killing, death_rate)
white <- dat_csa_white %>%
  dplyr::select(csa_name, white_killing, death_rate_white)
csa_comparison <- left_join(minority, white, by = "csa_name")

csa_comparison <- csa_comparison %>%
  mutate(increase = (death_rate - death_rate_white)/death_rate_white) %>%
  mutate(name = substr(str_extract(csa_name, "([^\\-]+).\\-*"), 1, 
                                           nchar(str_extract(csa_name, "([^\\-]+).\\-*"))-1),
         name = gsub("\\-", "", name))
write.csv(csa_comparison, paste0(output, "csa_comparison.csv"))

csas <- dat_csa_white$csa_name

all_counties <- dat %>%
  filter(!is.na(csa_name)) %>%
  group_by(csa_name) %>%
  summarize(count = n_distinct(county),
            pop = max(csa_pop)) %>%
  filter(pop > 1000000) %>%
  dplyr::select(-pop)

poor_counties <- dat %>%
  distinct(county, .keep_all = T) %>%
  group_by(csa_name) %>%
  filter(Median_Household_Income_2018 <= quantile(Median_Household_Income_2018, na.rm = T)[2]) %>%
  summarize(count = n_distinct(county),
            pop = sum(POP_ESTIMATE_2018),
            csa_pop = max(csa_pop),
            minority_pop = sum(minority_county_pop),
            white_pop = sum(white_county_pop)) %>%
  mutate(share = minority_pop/pop) %>%
  filter(csa_pop > 1000000) %>%
  dplyr::select(csa_name, minority_pop, white_pop)


dat_csa_white_rob <- dat %>%
  filter(!is.na(csa_name)) %>%
  group_by(csa_name) %>%
  filter(Median_Household_Income_2018 <= quantile(Median_Household_Income_2018, na.rm = T)[2]) %>%
  summarize(white_killing = sum(white, na.rm = T),
            pop = max(csa_pop, na.rm = T),
            pop_minority = max(csa_minority, na.rm = T),
            lat = mean(latitude, na.rm = T), 
            lon = mean(longitude, na.rm = T),
            counties = n_distinct(county)) %>%
  mutate(death_rate_white = white_killing/pop) %>%
  filter(pop > 1000000) %>%
  arrange(-death_rate_white) %>%
  mutate(name = paste0(1:n(), ". ", substr(str_extract(csa_name, "([^\\-]+).\\-*"), 1, 
                                           nchar(str_extract(csa_name, "([^\\-]+).\\-*"))-1)),
         name = gsub("\\-", "", name),
         share_minority = pop_minority/pop)

dat_csa_all_rob <- dat %>%
  filter(!is.na(csa_name)) %>%
  group_by(csa_name) %>%
  filter(Median_Household_Income_2018 <= quantile(Median_Household_Income_2018, na.rm = T)[2]) %>%
  summarize(min_killing = sum(minority, na.rm = T),
            pop = max(csa_pop, na.rm = T),
            pop_minority = max(csa_minority, na.rm = T),
            lat = mean(latitude, na.rm = T), 
            lon = mean(longitude, na.rm = T),
            counties = n_distinct(county)) %>%
  mutate(death_rate = min_killing/pop_minority) %>%
  filter(pop > 1000000) %>%
  arrange(-death_rate) %>%
  mutate(rank =1:n(), 
         name = paste0(rank, ". ", substr(str_extract(csa_name, "([^\\-]+).\\-*"), 1, 
                                          nchar(str_extract(csa_name, "([^\\-]+).\\-*"))-1)),
         name = gsub("\\-", "", name),
         share_minority = pop_minority/pop)

minority <- dat_csa_all_rob %>%
  dplyr::select(csa_name, pop, min_killing, counties)
white <- dat_csa_white_rob %>%
  dplyr::select(csa_name, white_killing)
robust <- left_join(minority, white, by = "csa_name")
robust <- left_join(robust, poor_counties, by = "csa_name")

robust <- robust %>%
  mutate(death_rate = min_killing/minority_pop, 
         death_rate_white = white_killing/white_pop)

robust <- left_join(robust, counties, by = "csa_name")

robust <- robust %>%
  mutate(pct = counties/count) %>%
  filter(pct != 1 & death_rate != 0)

robust <- robust %>%
  mutate(increase = (death_rate - death_rate_white)/death_rate_white) %>%
  mutate(name = substr(str_extract(csa_name, "([^\\-]+).\\-*"), 1, 
                       nchar(str_extract(csa_name, "([^\\-]+).\\-*"))-1),
         name = gsub("\\-", "", name))
write.csv(csa_comparison, paste0(output, "csa_comparison_robust.csv"))
