library(tidyverse)
library(tidytuesdayR)
library(ggthemes)

blue_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "blue", fill = NA, linetype = 2),
    # color background 2)
    panel.background = element_rect(fill = "aliceblue"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "steelblue", face = "italic", family = "Courier"),
    axis.title = element_text(colour = "steelblue", family = "Courier"),
    axis.ticks = element_line(colour = "steelblue"),
    # legend at the bottom 6)
    legend.position = "right"
  )
}

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

# What is the linear relationship between each production_unit wrt/ total_emissions?
unit_slope <- emissions %>% 
  mutate(slope = total_emissions_MtCO2e/production_value) 

# What is the variance of this slope wrt/ each production_unit
ggplot(unit_slope %>% group_by(production_unit), aes(x = slope, colour = production_unit)) +
  geom_freqpoly(binwidth = .01) +
  labs(title = "Variance in Slope",
       x = "Slope",
       y = "Frequency") +
  theme_minimal()

# Closer look at "Million tonnes/yr"
mtyr <- unit_slope %>% 
  filter(production_unit == "Million tonnes/yr")

ggplot(mtyr %>% group_by(commodity), aes(x = slope, colour = commodity)) +
  geom_histogram() +
  labs(title = "Million Tonnes/yr Slope Variance",
       x = "Slope",
       y = "Frequency") +
  blue_theme()


       