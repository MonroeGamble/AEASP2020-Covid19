## Author: J Monroe Gamble IV
## Project: Covid-19 Exploratory Data
## Created: 06/29/2020
## Description: This code creates tables & figures for the COVID-19 project
## State & county rankings for cases and deaths output as CSV
## Figures output as PNG
###########################################################


library(fredr)
library(tidyverse)
#library(plotly)

key <- '***REMOVED FOR CONFIDENTIALITY. SEE FRED WEBITE FOR YOUR OWN KEY ***'
fredr_set_key(key)

setwd('***REMOVED FOR CONFIDENTIALITY***/Black_White_Wage_Gap')
source('***CustomTheme.R')


# 12-month Avg
Avg12 <- function(x){
  
  (lag(x, 0) + lag(x, 1) + lag(x, 2) + lag(x, 3) + lag(x, 4) + lag(x, 5) + 
     lag(x, 6) + lag(x, 7) + lag(x, 8) + lag(x, 9) + lag(x, 10) + lag(x, 11))/12
  
}
# 4-Quarter Avg
Avg4Q <- function(x){
  
  (lag(x, 0) + lag(x, 1) + lag(x, 2) + lag(x, 3) + lag(x, 4))/4
  
}


# Graph Data
graph <-
  function(x) {
    
    # BOD Theme
    col1 = '#2e5e8b' # dark blue
    col2 = '#8baf3e' # green
    col3 = '#fcc62d' # yellow
    col4 = '#88cde5' # light blue
    col5 = '#474747' # black
    col6 = '#FF9933' # orange
    col7 = '#A8A8A8' #panel.grid.major.y
    
    ggplot(x, aes(x = date, y = value, colour = series_id)) +
      get_recessions(min(x$date), max(x$date)) +
      geom_line(size = 1.1) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_x_date(date_breaks = "6 year", expand = c(0, 0), date_labels = "%Y") +
      get_bod_theme() +
      theme(
        legend.position = 'top'
      )
    
  }

# 

visDir <- '***REMOVED FOR CONFIDENTIALITY***/Visualizations/'
setwd(visDir)

# S&P 500 ------------------------------------------------
ggplot(fredr("SP500"), aes(x = date)) +
  geom_line(aes(y=value), color = col1, size = 1.2) +
  labs(title = "S&P 500 Index (2010-Present)", caption = "Source: S&P Dow Jones Indices LLC") +
  scale_x_date(date_breaks = "1 year", expand = c(0, 0), date_labels = "%Y") +
  ***_theme()

ggsave(paste0(visDir, 'SP500_2.png'), width =  8.64, height = 4.16)

# Unemployment 2010-Present ------------------------------------------------
graph(fredr('LNS14000024') %>% filter(date >= '2010-02-22'))  +
  scale_colour_manual('', values = c('LNS14000024' = col3), labels = c("Unemployment")) +
  labs(title = "Unemployment (2010-Present)", caption = "Source: Bureau of Labor Statistics") +
  scale_x_date(date_breaks = "1 year", expand = c(0, 0), limits = c(as.Date('2010-02-22'), Sys.Date()), date_labels = "%Y") +
  theme(legend.position = "none")

#ggsave('Unemp2010_2020.png', width =  8.64, height = 4.16))


# Unemployment By Race & Sex Data ------------------------------------------------
fred_codes <- c('LNS14000031', 'LNS14000032', 'LNS14000028', 'LNS14000029')

bls_data <- lapply(fred_codes, fredr) %>%
  bind_rows() %>% 
  mutate(series_id = gsub('LNS14000031', "Black Male", series_id),
         series_id = gsub('LNS14000032', "Black Female", series_id),
         series_id = gsub('LNS14000028', "White Male", series_id),
         series_id = gsub('LNS14000029', "White Female", series_id),
  )


# Unemployment Data Wide Format
bls_wide <- spread(bls_data, series_id, value) %>%
  mutate(male = `White Male` - `Black Male`, 
         female = `White Female` - `Black Female`) %>% 
  na.omit() %>% .[c(1,6,7)] %>% arrange(desc(date))

# Combined Sex & Race 2010-Present ------------------------------------------------
bls_data$series_id <- factor(bls_data$series_id, 
                             levels = c('Black Male', 'White Male', 'Black Female', 'White Female'))

graph(bls_data %>% filter(date >= '2010-02-22')) +
  scale_colour_manual('Race & Sex', values = c(col1, col5, col2, col3)) +
  labs(title = "Unemployment rates by sex & race (2010-Present)", caption = "Source: Bureau of Labor Statistics") +
  scale_x_date(date_breaks = "1 year", expand = c(0, 0), limits = c(as.Date('2010-02-22'), Sys.Date()+60), date_labels = "%Y")

#ggsave('Unemp_Sex&Race2010_2020.png', width =  8.64, height = 4.16))

# Combined Sex & Race 1979-Present ------------------------------------------------
graph(bls_data %>% filter(1979 <= year(date))) +
  scale_colour_manual('Race & Sex', values = c(col1, col5, col2, col3)) +
  labs(title = "Unemployment rates by sex & race (1979-Present)", caption = "Source: Bureau of Labor Statistics") +
  scale_x_date(date_breaks = "5 year", expand = c(0, 0), limits = c(as.Date('1979-01-01'), as.Date('2020-07-25')), date_labels = "%Y")

#ggsave('Unemp_Sex&Race1979_2020.png', width =  8.64, height = 4.16))
#Saving 8.64 x 4.16 in image

# ------------------------------------------------


##Unemployment All ------------------------------------------------
fred_codes <- c('LNS14000031', 'LNS14000032', 'LNS14000028', 'LNS14000029', 'LNS14000024')

bls_data <- lapply(fred_codes, fredr) %>%
  bind_rows() %>% 
  mutate(series_id = gsub('LNS14000031', "Black Male", series_id),
         series_id = gsub('LNS14000032', "Black Female", series_id),
         series_id = gsub('LNS14000028', "White Male", series_id),
         series_id = gsub('LNS14000029', "White Female", series_id),
         series_id = gsub('LNS14000024', "Unemployment", series_id)
  )


# Combined Sex
bls_data$series_id <- factor(bls_data$series_id, 
                             levels = c('Black Male', 'White Male', 'Black Female', 'White Female', "Unemployment"))

graph(bls_data %>% filter(1979 <= lubridate::year(date))) +
  scale_colour_manual('Race & Sex', values = c(col1, col5, col2, col4, col3)) +
  labs(title = "Unemployment rates by sex & race 1979-Present", caption = "Source: Bureau of Labor Statistics") +
  scale_x_date(date_breaks = "5 year", expand = c(0, 0), limits = c(as.Date("1979-01-01"), Sys.Date()+60), date_labels = "%Y")

ggsave('Unemp1979_2020.png', width =  8.64, height = 4.16)


##Average Hourly Wage -------------------------------------

library(Haver)
library(zoo)
library(lubridate)
library(reshape2)

#Average hours of all person at work USECON:LENCLWHN

#White Men    EMPL:HNX16WM
#White Women  EMPL:HNX16WF
#Black Men    EMPL:HNX16BM
#Black Women  EMPL:HNX16BF


haver_codes <- c('EMPL:HNX16WM', 'EMPL:HNX16WF', 'EMPL:HNX16BM', 'EMPL:HNX16BF')


Avg_hours <-
  haver.data(
    haver_codes,
    rtype = 'data.frame',
    frequency = 'q'
  ) 

Avg_hours <-
  Avg_hours %>% 
  rownames_to_column('date') %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q")) %>%
  #mutate(date = paste0(date, '-01')) %>%
  mutate(date = as.Date(date, format="%Y-%b-%d")) %>%
  rename(`White Male` = hnx16wm, `White Female` = hnx16wf, `Black Male` = hnx16bm, `Black Female` = hnx16bf) %>%
  as_tibble()


#Median Usual Weekly Earnings Full-time wage & Salary Workers (2000-Present)
#Unadjusted Seasonally
#White Men USECON:LBSWY
#White Women USECON:LBSWX
#Black Men USECON:LBSBY
#Black Women USECON:LBSBX

haver_codes2 <- c('USECON:LBSWY', 'USECON:LBSWX', 'USECON:LBSBY', 'USECON:LBSBX')

Med_wk_earn <-
  haver.data(
    haver_codes2,
    rtype = 'data.frame'
  )

Med_wk_earn <-
  Med_wk_earn %>% 
  rownames_to_column('date') %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q")) %>%
  mutate(date = as.Date(date, format="%Y-%b-%d")) %>%
  rename(`White MaleE` = lbswy, `White FemaleE` = lbswx, `Black MaleE` = lbsby, `Black FemaleE` = lbsbx) %>%
  as_tibble()


# Calculate Average Hourly Earnings
AvgEarnHourly <-
  left_join(Avg_hours, Med_wk_earn) %>% 
  filter(1979 <= year(date)) %>%
  mutate(WM_AvgEpH = `White MaleE`/`White Male`,
         WF_AvgEpH = `White FemaleE`/`White Female`,
         BM_AvgEpH = `Black MaleE`/`Black Male`,
         BF_AvgEpH = `Black FemaleE`/`Black Female`) %>%
  select(date, BM_AvgEpH, BF_AvgEpH, WM_AvgEpH, WF_AvgEpH)


# Load CPI99
CPI99 <- read_csv("C:/Users/L1JMG03/Dropbox (FRB SF)/Black_White_Wage_Gap/CPS_data/CPI99.csv") %>%
  select(date = `Data Year`, CPI99)  %>%
  filter(date >= '1978-01-01') %>%
  mutate(date = as.Date(paste0(date, '-01-01')))

# Melt Data Frame (No q4 adjustment)
AvgEarnPerHour2 <- 
  melt(AvgEarnHourly, id.vars = 'date', value.name = 'value', na.rm = T ) %>% 
  as_tibble() %>%
  left_join(CPI99) %>%
  na.locf() %>%
  #mutate(value = as.numeric(CPI99)*value) %>%
  select(date, series_id = variable, value)

AvgEarnPerHour2$series_id <- factor(AvgEarnPerHour2$series_id,
                                    levels = c('BM_AvgEpH', 'WM_AvgEpH', 'BF_AvgEpH', 'WF_AvgEpH'),
                                    labels = c('Black Male', 'White Male', 'Black Female', 'White Female'))

graph(AvgEarnPerHour2) +
  scale_colour_manual('Race & Sex', values = c(col1, col5, col2, col3)) +
  scale_y_continuous(labels = function(x) paste0("$",x)) +
  labs(title = "Average hourly earnings by sex & race (1979-Present)", caption = "Source: Bureau of Labor Statistics") +
  scale_x_date(date_breaks = "5 year", expand = c(0, 0), limits = c(as.Date('1979-01-01'), as.Date('2020-07-25')), date_labels = "%Y")
