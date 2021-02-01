# AEASP2020-Covid19

## Author: J Monroe Gamble IV
## Project: Covid-19 Exploratory Data
## Created: 06/21/2020
## Description: Updates most recent JHU data at the county level and calculates daily change. 
## Code ouputs two csv files to evaluate pre- and post-Memorial Day trends.


# Clear Workspace ----------------------------------------
remove(list = ls())

# Load Packages
library(tidyverse)
library(reshape2)

# Set directory & Load Data
setwd("**Substitute: File location**/data")

## UPDATE Data ######################################

## John's Hopkins Cases & Deaths Time series (Daily) -------------------------

JHUCases <-
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
JHUDeaths <-
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

JHU_c <- read_csv(JHUCases)
JHU_d <- read_csv(JHUDeaths)

write.csv(JHU_c, paste0("rawcases", Sys.Date(),".csv"))
write.csv(JHU_d, paste0("rawcases", Sys.Date(),".csv"))

# JHU Cases & Deaths by County (long) ---------------------------------------------

# Daily Cases
cases_long <-
  JHU_c %>%
  select(-UID, -iso2, -iso3, -code3) %>%
  melt(
    id.vars = c("FIPS", "Admin2", "Province_State",  "Country_Region", "Lat", "Long_","Combined_Key"),
    variable.name = "date",
    value.name = "cases"
  )  %>%
  as_tibble() %>%
  mutate(cases = as.integer(cases)) %>%
  na.omit(FIPS) %>%
  mutate(date = as.Date(format(as.Date(date, "%m/%d/%Y"), "%2020-%m-%d")))

# Daily Deaths
deaths_long <-
  JHU_d %>%
  select(-UID,-iso2,-iso3,-code3) %>%
  melt(
    id.vars = c( "FIPS","Admin2", "Province_State","Country_Region", "Lat", "Long_", "Combined_Key", "Population"),
    variable.name = "date",
    value.name = "deaths"
  )  %>%
  as_tibble() %>%
  mutate(deaths = as.integer(deaths)) %>%
  na.omit(FIPS) %>%
  mutate(date = as.Date(format(as.Date(date, "%m/%d/%Y"), "%2020-%m-%d")))

JHU_long <- merge(cases_long, deaths_long) %>%
  as_tibble()

rm(cases_long, deaths_long, JHU_c, JHU_d)

# Split data Into Pre- & Post-Memorial Day May 25, 2020 ---------------------

#Before Memorial Day
JHU_longPRE <-
  JHU_long %>%
  filter(date <= '2020-06-08')

#After Memorial Day
JHU_longPost <-
  JHU_long %>%
  filter(date >= '2020-06-08')

rm(JHU_long)
# PRE-Labor Day Time Series Differences (Daily) --------------------------------------------
JHU_timePRE <-
  JHU_longPRE %>%
  arrange(FIPS, date) %>%
  mutate(FIPS_lead = lead(FIPS),
         FIPS_lag = lag(FIPS),
         case_lag = lag(cases),
         case_lead = lead(cases),
         deaths_lag = lag(deaths),
         Admin2_lead = lead(Admin2),
         Admin2_lag = lag(Admin2)) %>%
  mutate(LAG = ifelse(FIPS_lag == FIPS, ifelse(FIPS_lead == FIPS, 1, 0), 0)) %>%
  filter(Admin2 != "Unassigned",
         !str_detect(Admin2, "Out of")) 

# Calculate day to day change in deaths and cases 
JHU_L_PRE <-
  JHU_timePRE %>%
  filter(LAG == 1) %>%
  mutate(new_cases =  cases - case_lag,
         new_deaths = deaths - deaths_lag) %>%
  select(date, Population, new_deaths, new_cases, cases, case_lag, case_lead, FIPS, FIPS_lag, FIPS_lead, Admin2, Admin2_lead, Admin2_lag, LAG, everything()) %>%
  as_tibble()

# NO lag on first day
JHU_NL_PRE <-
  JHU_timePRE %>%
  filter(LAG == 0) %>%
  filter(date == "2020-01-22") %>%
  mutate(
    case_lag = 0,
    new_cases = 0,
    deaths_lag = 0,
    new_deaths = 0,
  ) %>%
  select(date, new_deaths, new_cases, cases, case_lag, case_lead, FIPS, FIPS_lag, FIPS_lead, Admin2, Admin2_lead,Admin2_lag, LAG, everything()) %>%
  as_tibble() 

JHU_PRE_timeSeries <-
  rbind(JHU_L_PRE, JHU_NL_PRE) %>%
  select(-case_lead, -FIPS_lag, -FIPS_lead, -Admin2_lead, -Admin2_lag)

# Remove Time Series
rm(JHU_timePRE, JHU_L_PRE, JHU_NL_PRE)


# POST-Labor Day Time Series Differences (Daily) --------------------------------------------
JHU_timePOST <-
  JHU_longPost %>%
  arrange(FIPS, date) %>%
  mutate(FIPS_lead = lead(FIPS),
         FIPS_lag = lag(FIPS),
         case_lag = lag(cases),
         case_lead = lead(cases),
         deaths_lag = lag(deaths),
         Admin2_lead = lead(Admin2),
         Admin2_lag = lag(Admin2)) %>%
  mutate(LAG = ifelse(FIPS_lag == FIPS, ifelse(FIPS_lead == FIPS, 1, 0), 0)) %>%
  filter(Admin2 != "Unassigned",
         !str_detect(Admin2, "Out of")) 

# Calculate day to day change in deaths and cases 
JHU_L_POST <-
  JHU_timePOST %>%
  filter(LAG == 1) %>%
  mutate(new_cases =  cases - case_lag,
         new_deaths = deaths - deaths_lag) %>%
  select(date, Population, new_deaths, new_cases, cases, case_lag, case_lead, FIPS, FIPS_lag, FIPS_lead, Admin2, Admin2_lead, Admin2_lag, LAG, everything()) %>%
  as_tibble() 

# NO lag on first day
JHU_NL_POST <-
  JHU_timePOST %>%
  filter(LAG == 0) %>%
  filter(date == "2020-05-25") %>%
  mutate(
    case_lag = 0,
    new_cases = 0,
    deaths_lag = 0,
    new_deaths = 0,
  ) %>%
  select(date, new_deaths, new_cases, cases, case_lag, case_lead, FIPS, FIPS_lag, FIPS_lead, Admin2, Admin2_lead,Admin2_lag, LAG, everything()) %>%
  as_tibble() 

JHU_POST_timeSeries <-
  rbind(JHU_L_POST, JHU_NL_POST) %>%
  select(-case_lead, -FIPS_lag, -FIPS_lead, -Admin2_lead, -Admin2_lag)

# Remove Time Series
rm(JHU_timePOST, JHU_L_POST, JHU_NL_POST)
# -------------------------------------------------------------------------


## Select Panel (Most Recent Day) #################################

#Before Labor Day ---------------------------------------
casesPRE <-
  JHU_PRE_timeSeries %>%
  filter(date == max(JHU_PRE_timeSeries$date))

deathsPRE <-
  JHU_PRE_timeSeries %>%
  filter(date == max(JHU_PRE_timeSeries$date))

varsMerge <- c("date", "FIPS", "Admin2", "Province_State",  "Country_Region", 
               "Lat", "Long_","Combined_Key", "Population", "deaths", "cases")

# Merge Before Labor Day JHU Data
JHU_PRE <- 
  merge(deathsPRE, casesPRE) %>% 
  as_tibble() %>%
  select(varsMerge)


## After Labor Day ---------------------------------------
casesPOST <-
  JHU_POST_timeSeries %>%
  filter(date == max(JHU_POST_timeSeries$date))

deathsPOST <-
  JHU_POST_timeSeries %>%
  filter(date == max(JHU_POST_timeSeries$date))

# Merge JHU Data
JHU_POST <- merge(deathsPOST, casesPOST) %>% 
  as_tibble() %>%
  select(varsMerge)

write.csv(JHU_PRE, paste0("JHU_PRE_", Sys.Date(),".csv"))
write.csv(JHU_POST, paste0("JHU_POST_", Sys.Date(),".csv"))

# Remove Cases & Deaths Long
rm(deathsPRE, casesPRE, casesPOST, deathsPOST)



########################################################################

## Census Data ----------------------------------------------------------

JHU <- JHU_POST
#############
JHU_POST_DATA <- JHU %>%
  mutate(date = as.Date('2020-07-23')) %>%
  select(date, everything())
#saveRDS(JHU_POST_DATA, "0_JHU_14")
w#rite.csv(JHU_POST_DATA, "0_JHU_POST_TODAY.csv", row.names = F)

JHU <- JHU_PRE



##############################

JHU_POST_DATA <- new_data %>%
  mutate(date = as.Date('2020-07-23')) %>%
  select(date, everything())
saveRDS(JHU_POST_DATA, "0_JHU_14")
write.csv(JHU_POST_DATA, "0_JHU_POST_TODAY.csv", row.names = F)

JHU <- JHU_PRE
##############################

JHU_PRE_DATA <- new_data  %>%
  mutate(date = as.Date('2020-06-07')) %>%
  select(date, everything())
saveRDS(JHU_POST_DATA,  paste0("0_JHU_PRE_14", Sys.Date(),".csv"))
write.csv(JHU_POST_DATA,  paste0("0_JHU_PRE_14.csv", Sys.Date(),".csv"), row.names = F)

Memorial_day <- rbind(JHU_POST_DATA, JHU_PRE_DATA)
write.csv(Memorial_day,  paste0("0_memorial_Day_TODAY", Sys.Date(),".csv"), row.names = F)



