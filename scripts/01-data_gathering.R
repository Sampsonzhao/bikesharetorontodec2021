### Preamble ###
# purpose: gather data from the December 2021 Bike Share Toronto Ridership
# Author: Sampson Zhao
# Contact: sampson.zhao@mail.utoronto.ca
# Date: 2022-02-04
# Pre-requisites: 
# - None

#### Workspace setup ####
library(tidyverse)
library(opendatatoronto)

#### Gather data ####
# Read in the raw data. 
# Based on: https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/
# Each dataset is part of a package of data, which each package having a unique
# ID, and can by found on the website where the data set is found. e.g. 
# https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/

package <- show_package("7e876c24-177c-4605-9cef-e50dd74c617f")

# within the package there are different resources that we need, and we use
# the unique ID to retrieve them. 

resources <- list_package_resources("7e876c24-177c-4605-9cef-e50dd74c617f")

# We want to choose a specific data set that has the bike share Toronto 
# information for December 2021.
datastore_resources <- filter(resources, tolower(format) %in% c('csv'))

# We want to then select the specific data set and assign it a variable so that 
# we can save it later.
data <- filter(datastore_resources, row_number() == 12) %>% get_resource()

#### Save Data ####
write_csv(data, "inputs/data/raw_data_Dec2021.csv")

