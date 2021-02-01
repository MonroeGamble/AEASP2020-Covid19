## Author: J Monroe Gamble IV
## Project: Covid-19 Exploratory Data
## Created: 06/29/2020
## Description: This code creates tables & figures for the COVID-19 project
## State & county rankings for cases and deaths output as CSV
## Figures output as PNG
###########################################################

library(tidyverse)

# Set Directory
dir <- "****INPUT DIRECTORY****"
setwd(paste0(dir, "Visualizations"))

# Load custon ggplot theme
#source(paste0(dir, "R/0_Code.R"))
source('***CustomTheme.R')  #This has been edited for confidentiality. Choose your own theme.

#Ranking Total Deaths and Cases By State
Sate_Rankings <-
  JHU_timeSeries %>%
    group_by(Province_State) %>%
    summarise(totdeaths = sum(new_deaths),
              totcases = sum(new_cases)) %>%
    mutate(deaths_rank = dense_rank(desc(totdeaths)),
           cases_rank = dense_rank(desc(totcases))) %>%
    arrange(deaths_rank) %>%
  select(Province_State, `Death Ranking` = deaths_rank, `Total Deaths` = totdeaths, `Case Ranking` = cases_rank, `Total Cases` = totcases)

Deathtop10 <- Sate_Rankings %>% 
  filter(`Death Ranking` <= 10) %>%
  arrange(`Death Ranking`)

Casestop10 <- Sate_Rankings %>% 
  filter(`Case Ranking` <= 10) %>%
  arrange(`Death Ranking`)

#Ranking Total Deaths and Cases By County
County_Rankings <-
  JHU_timeSeries %>%
    group_by(FIPS) %>%
    summarise(totdeaths = sum(new_deaths),
              totcases = sum(new_cases)) %>%
    mutate(deaths_rank = dense_rank(desc(totdeaths)),
           cases_rank = dense_rank(desc(totcases))) %>%
    arrange(deaths_rank, cases_rank) %>%
    inner_join(distinct(JHU_timeSeries[c("FIPS", "Admin2")])) %>%
    select(FIPS, Admin2, everything())
    
Deathstop10_Plot <-
  ggplot(Deathtop10,
         aes(x = `Death Ranking`, y = `Total Deaths`, fill = Province_State)) +
    geom_bar(stat = "identity") +
  theme_***() +
    scale_fill_discrete(name = "State",
                        breaks = c(Casestop10$Province_State)) +
    scale_x_continuous(breaks = seq(0, 10, 1)) +
    scale_y_continuous(
      labels = scales::comma,
      expand = c(0, 0),
      breaks = seq(0, max(Casestop10$`Total Deaths`) + 1000, 5000),
      limits = c(0, max(Casestop10$`Total Deaths`) + 1500)
    ) +
    xlab('Ranking') +
    ylab("Total Deaths") +
  labs(caption = "Source: John's Hopkins University") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

Casestop10_Plot <-
  ggplot(Casestop10,
         aes(x = `Case Ranking`, y = `Total Cases`, fill = Province_State)) +
    geom_bar(stat = "identity") +
  theme_***() +
    scale_fill_discrete(name = "State",
                        breaks = c(Casestop10$Province_State)) +
    scale_x_continuous(breaks = seq(0, 10, 1)) +
    scale_y_continuous(
      labels = scales::comma,
      expand = c(0, 0),
      breaks = seq(0, max(Casestop10$`Total Cases`), 50000),
      limits = c(0, max(Casestop10$`Total Cases`) + 10000)
    ) +
    xlab('Ranking') +
    ylab("Total Cases") +
  labs(caption = "Source: John's Hopkins University") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )


# Saving Plots
plot_list <- list(Deathstop10_Plot, Casestop10_Plot)  
plot_names <- list('Deathstop10_Plot', 'Casestop10_Plot') 


for (i in 1:length(plot_list)) {
  ggsave(paste0(plot_names[i],"_", Sys.Date(),".png"), plot_list[[i]], width =  8.64, height = 4.16)
  
}

write.csv(Sate_Rankings, paste0(dir, "data/State_Rankings", Sys.Date(), ".csv"), row.names = F)


