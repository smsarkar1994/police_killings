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

dat <-  readRDS(paste0(input, "master.RDS"))

min_pop <- distinct(dat, dat$subject_race_imp, dat$min_pop) %>%
  filter(!is.na(`dat$min_pop`)) 
colnames(min_pop) <- c("race", "pop")
min_pop <- min_pop %>%
  filter(race != "white")
tot_min_pop = sum(min_pop$pop)

dat <- dat %>%
  mutate(minority = ifelse(subject_race_imp == "white", "White", "Minority"),
         police_killing = ifelse(cause_of_death == "Asphyxiated/Restrained" 
                                 | cause_of_death == "Beaten/Bludgeoned with instrument" 
                                 | cause_of_death == "Chemical agent/Pepper spray" 
                                 | cause_of_death == "Gunshot" 
                                 | cause_of_death == "Tasered", 1, 0),
         latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude), 
         csa_name = as.character(csa_name),
         min_pop = as.numeric(as.character(min_pop)),
         oth_pop = ifelse(minority == "Minority", tot_min_pop, min_pop)) %>%
  rename(race = subject_race_imp) %>%
  mutate(race = ifelse(race == "asian", "Asian", 
                       ifelse(race == "black", "Black", 
                              ifelse(race == "latino", "Hispanic", 
                                     ifelse(race == "native", "Native \n\r American", 
                                            ifelse(race == "white", "White", NA))))))

dat_race <- dat %>%
  group_by(race) %>%
  summarize(count = sum(police_killing, na.rm = T),
            pop = max(min_pop)) %>%
  mutate(death_rate = count/pop) %>%
  filter(!is.na(death_rate))

p <- ggplot(data = dat_race, aes(x = race)) + geom_bar(aes(weight = death_rate)) + 
  theme_classic() + scale_y_continuous(expand = c(0, 0), labels = scales::percent) + 
  xlab("Race") + ylab("Police Killing Rate") + 
  ggtitle("Police Killing Rate by Race") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0(output, "killing_rate_race.png"),p)
