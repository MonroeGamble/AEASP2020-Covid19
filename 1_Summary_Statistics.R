# AEASP2020-Covid19

## Author: J Monroe Gamble IV
## Project: Covid-19 Exploratory Data
## Created: 06/21/2020
## Description: Code performs exploartory analysis. Run after 0_MemorialDayCode.R


cases_density <-
  JHU_timeSeries %>%
    group_by(Province_State)

# Create Line Plots for Cases and Deaths
case_plot <- aggregate(cases ~ Province_State + date, cases_density, sum) %>%
  as_tibble()
death_plot <- aggregate(deaths ~ Province_State + date, cases_density, sum) %>%
  as_tibble()

ggplot(case_plot, aes(x=date, y=cases, color = Province_State)) +
  geom_line() +
  theme_classic()

ggplot(death_plot, aes(x=date, y=deaths, color = Province_State)) +
  geom_line() +
  theme_classic()


# State Rankings for Cases
State_Case_Rankings <-
  case_plot %>%
    #group_by(Province_State) %>%
    filter(as.Date(date) == Sys.Date()-1) %>%
    summarise(ranksum = cumsum(cases)) %>%
    mutate(rank = dense_rank(desc(ranksum))) %>%
    arrange(rank)

State_Death_Rankings <-
  death_plot %>%
  group_by(Province_State) %>%
  summarise(ranksum = cumsum(deaths)) %>%
  mutate(rank = dense_rank(desc(ranksum))) %>%
  arrange(rank)


ggplot() +
  geom_bar(case_plot, mapping = aes(x=date, y=cases), stat = "identity") +
  geom_bar(death_plot, aes(y=deaths), stat = "identity") +
  theme_classic()



ggplot(case_plot, aes(x=date)) +
  geom_line(case_plot, aes(x=date, y=cases)) +
  geom_line(death_plot, aes(x =date, y=deaths)) +
  theme_classic()

ggplot(death_plot, aes(x=date, y=deaths)) +
  theme_classic()



ggplot(cases_density, aes(x=cases, y=number)) +
  geom_bar(stat = "identity")


  JHU_long %>%
  mutate(date = date) %>% 
  mutate(cases = cumsum(cases),
         deaths = cumsum(deaths))


ggplot(California, aes(x=date, y = deaths)) +
geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%B")

ggplot(Cali, aes(x=date)) +
  geom_smooth(aes(y = deaths)) +
  geom_smooth(aes(y = cases))


Cali <- 
  aggregate(cases ~ Province_State + date + deaths, California, sum, drop = T) %>%
  as.tibble()

ggplot(Cali, aes(x=date, y=deaths, color = deaths)) +
  geom_bar(stat = "identity")






  ggplot(JHU_long, aes(x=date, y = cases)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%B")

ggplot(Cali, aes(x=date, y = cases)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%B")




ggplot(California, aes(x=date)) +
  geom_line(aes(y = deaths))



  scale_x_bd(business.dates = Cali$date)

install.packages("bdscale")
library(bdscale)  

scale_x_continuous()
  
  scale_x_date(date_labels = "%B")



ggplot(California, aes(x=date, y = cases)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%B")

