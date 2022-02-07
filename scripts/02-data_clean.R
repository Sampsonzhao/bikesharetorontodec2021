### Preamble ###
# purpose: clean data from the December 2021 Bike Share Toronto Ridership
# Author: Sampson Zhao
# Contact: sampson.zhao@mail.utoronto.ca
# Date: 2022-02-04
# Pre-requisites: 
# - Need to have downloaded the Ridership data and saved it to inputs/data

#### Workspace setup ####
# install.packages("haven")
# install.packages("tidyverse")
# install.packages("lubridate")
library(haven)
library(tidyverse)
library(reshape2)    # reformat data for plot

### Read raw data ###
raw_data <- readr::read_csv(here::here("inputs/data/raw_data_Dec2021.csv"))

### Select the variables I'm interested in ###
filtered_data <- 
  raw_data %>% 
  select(Trip..Duration, 
         Start.Station.Id,
         Start.Time,
         End.Station.Id,
         End.Time,
         User.Type)
rm(raw_data)

### Add new column to determine if trip was considered long or not
filtered_data <- 
  filtered_data |>
  mutate(Long_Trip = ifelse(Trip..Duration > 900, 'Yes','No'))

### Filter data based on random number generator
start_selected_data <-
  filtered_data |>
  filter (Start.Station.Id %in% 
                  c('7249', '7338', '7293', '7662', '7107', '7428'))
end_selected_data <-
  filtered_data |>
  filter (End.Station.Id %in% 
            c('7249', '7338', '7293', '7662', '7107', '7428'))

# There was some issue with the data set having extra entries, so the last 2
# entries were removed so that the length of both start and end data are the same. 
end_selected_data <-
  end_selected_data|>
  slice(1:1012)


### Merge Count data for start and end station 
count_data_start <-
  start_selected_data|>
  count(Start.Station.Id, name = 'Start.Station')

count_data_start <-
  count_data_start |>
  rename(Station.ID=Start.Station.Id)

count_data_end <-
  end_selected_data |>
  count(End.Station.Id, name = 'End.Station')

count_data_end <-
  count_data_end |>
  rename(Station.ID=End.Station.Id)

count_data <-
  merge(count_data_start, count_data_end, by = 'Station.ID')

rm(count_data_start,count_data_end)

  
start_selected_data |>
  ggplot(mapping = aes(x = factor(Start.Station.Id), fill = User.Type)) + 
  geom_bar(position="dodge")+
  labs(title = 'Number of Users at 6 Random Start Stations in December 2021',
       x = 'Starting Station', y = 'Number of Riders') +
  geom_text(stat='count', aes(label=after_stat(count)), 
            position = position_dodge(width = 0.9), vjust = -1)

end_selected_data |>
  ggplot(mapping = aes(x = factor(End.Station.Id), fill = User.Type)) + 
  geom_bar(position="dodge")+
  labs(title = 'Number of Users at 6 Random End Stations in December 2021',
       x = 'Ending Station', y = 'Number of Riders') +
  geom_text(stat='count', aes(label=after_stat(count)), 
            position = position_dodge(width = 0.9), vjust = -1)


