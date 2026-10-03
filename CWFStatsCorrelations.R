library(tidyverse)
library(readxl)
library(igraph)
library(broom)

CWFStateStats <- read_csv("CWFStateStats.csv")

allStats <- netStats %>%
  left_join(CWFStateStats)

allStats <- allStats %>%
  #select(-state) %>%
  pivot_longer(-c(state,sex,efficiency,transitivity,dens,avg_degree), names_to = "Var", values_to = "Estimate") %>%
  view()

ggplot(allStats, aes(x = dens,y = Estimate)) +
  geom_point(aes(color = sex)) +
  geom_smooth(aes(color = factor(sex)), method = "lm", formula = y ~ x, color = "darkred") +
  facet_wrap(vars(Var), scales = "free", labeller = label_wrap_gen(width = 50))

#Female graphs
allFemStats <- femaleNetStats %>%
  left_join(CWFStateStats)

allFemStats <- allFemStats %>%
  #select(-state) %>%
  pivot_longer(-c(state, efficiency,transitivity,dens,avg_degree), names_to = "Var", values_to = "Estimate") %>%
  view()

ggplot(allFemStats, aes(x = dens,y = Estimate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkred") +
  facet_wrap(vars(Var), scales = "free")

#Regression results
lmResultsMale <- allStats %>%
  pivot_longer(cols = c(efficiency, transitivity, dens, avg_degree), names_to = "Metric", values_to = "Value") %>%
  group_by(Metric, Var) %>%
  nest() %>%
  mutate(
    model = map(data, ~lm(Estimate ~ Value, data = .x)),
    results = map(model, tidy)
  ) %>%
  unnest(results) %>%
  select(-data, -model) %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, 1, 0))

g <- maleNets[[1]]
