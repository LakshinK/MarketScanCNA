#Figures

library(tidyverse)
library(usmap)

states <- map_data("state")

merged_map <- left_join(states, allStats %>%
                          mutate(state = tolower(state)) %>%
                          mutate(state = str_replace(state, "_", " ")), by = c("region" = "state"))

ggplot(merged_map, aes(long, lat, group = group, fill = dens)) +
  geom_polygon() +
  coord_quickmap() +
  theme_void()

###State by Charlson Comorbidity Index Strength
strengthByState <- maleStrengthIndexCondition %>%
  ungroup() %>%
  group_by(state) %>% 
  summarise(sum = sum(strength, na.rm = T),
            avg = mean(strength, na.rm = T))

states <- map_data("state")

merged_map <- left_join(states, strengthByState %>%
                          mutate(state = tolower(state)) %>%
                          mutate(state = str_replace(state, "_", " ")), by = c("region" = "state"))

ggplot(merged_map, aes(long, lat, group = group, fill = avg)) +
  geom_polygon() +
  coord_quickmap()  + 
  labs(fill = "Average Cumulative log(Lift) of CCI Diseases") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 28),
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(size = 24)) + 
  guides(fill = guide_legend(title.position = "top")) + 
  scale_fill_gradient(low = "lightgreen", high = "darkred")

###State by Charlson Comorbidity Index Strength
degreeByState <- maleDegreeStats %>%
  ungroup() %>%
  group_by(state) %>% 
  summarise(sum = sum(sum))

states <- map_data("state")

merged_map <- left_join(states, degreeByState %>%
                          mutate(state = tolower(state)) %>%
                          mutate(state = str_replace(state, "_", " ")), by = c("region" = "state"))

ggplot(merged_map, aes(long, lat, group = group, fill = sum)) +
  geom_polygon() +
  coord_quickmap() +
  theme_void() +
  scale_fill_gradient(low = "lightgreen", high = "darkred")
