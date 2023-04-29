
library(tidyverse)
hw <- readr::read_csv('./data/Honeywell_Heating_Usage .csv')
hw %>% ggplot(aes(Date,`Heating usage (hours)`))+geom_col()
hw %>% ggplot(aes(Month,`Heating usage (hours)`))+geom_boxplot(aes(group=Month))

# usage vs temp
hw %>% ggplot(aes(`avg high temp`,`Heating usage (hours)`))+geom_point()
hw %>% ggplot(aes(`avg low temp`,`Heating usage (hours)`))+geom_point()
hw %>% ggplot(aes(`avg low temp`,`Heating usage (hours)`,colour=Year))+geom_point()+geom_smooth()

hw %>% ggplot(aes(`avg low temp`,`Heating usage (hours)`))+geom_point()+facet_wrap('Year')

hw %>% group_by(Year) %>% summarise(totaluse=sum(`Heating usage (hours)`)) %>% View()

hw %>% group_by(Month) %>% 
  summarise(avg=mean(`Heating usage (hours)`,na.rm=TRUE),
            min=min(`Heating usage (hours)`,na.rm=TRUE),
            max=max(`Heating usage (hours)`,na.rm=TRUE)) %>%
  View() 