library(tidyverse)
library(readxl)

CWFStateStats <- read_csv("CWFStateStats.csv")

allStats <- netStats %>%
  left_join(CWFStateStats)

allStats <- allStats %>%
  #select(-state) %>%
  pivot_longer(-c(state, efficiency,transitivity,dens,avg_degree), names_to = "Var", values_to = "Estimate") %>%
  view()

ggplot(allStats, aes(x = dens,y = Estimate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkred") +
  facet_wrap(vars(Var), scales = "free")
