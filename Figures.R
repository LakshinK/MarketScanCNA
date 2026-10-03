#Figures

library(tidyverse)

states <- map_data("state")

merged_map <- left_join(states, allStats %>%
                          mutate(state = tolower(state)) %>%
                          mutate(state = str_replace(state, "_", " ")), by = c("region" = "state"))

ggplot(merged_map, aes(long, lat, group = group, fill = dens)) +
  geom_polygon() +
  coord_quickmap() +
  theme_void()
