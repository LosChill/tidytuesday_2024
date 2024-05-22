library(tidyverse)
library(tidytuesdayR)

tuesdata <- tt_load(2024, week = 21)
emissions <- tuesdata$emissions

# How many of each commodity?
prods <- emissions %>% 
  group_by(commodity) %>% 
  summarize(count = n())
  ungroup()

# How many of each production unit?
prod_units <- emissions %>% 
  group_by(production_unit) %>% 
  summarize(count = n()) %>% 
  ungroup()

# Is the relationship between each production_unit and total emissions linear?
emissions_prodgrp <- emissions %>%
  group_by(production_unit)

ggplot(emissions_prodgrp, aes(x = production_value, y = total_emissions_MtCO2e, color = production_unit)) +
  geom_point()

# What is the linear relationship between each production_unit::total_emissions?
