library(tidyverse)
library(igraph)

nodeCats <- data.frame(
  condition = indexConditions
) %>%
  mutate(color = case_when(
    str_detect(condition, "neo_") ~ "#F26b5A",
    str_detect(condition, "resp_") ~ "#F5c518",
    str_detect(condition, "hiv") ~ "#246A78",
    str_detect(condition, "cvd_") ~ "#2E2B6B",
    str_detect(condition, "stroke") ~ "orange",
    str_detect(condition, "msk_") ~ "#5C6A2C",
    str_detect(condition, "cirrhosis") ~ "#B5562A",
    str_detect(condition, "cirrhosis") ~ "#D9A6B2",
    str_detect(condition, "diabetes") ~ "#9FD3E6",
    str_detect(condition, "ckd") ~ "black",
    str_detect(condition, "dementia") ~ "#F5E08A",
    str_detect(condition, "encephalitis") ~ "darkred",
    .default = "lightgray"
  )) %>%
  mutate(
    Category = case_when(
      str_detect(condition, "neo_") ~ "Neoplasm",
      str_detect(condition, "resp_") ~ "Pulmonary",
      str_detect(condition, "hiv") ~ "HIV",
      str_detect(condition, "cvd_") ~ "CVD",
      str_detect(condition, "stroke") ~ "Cerebrovascular",
      str_detect(condition, "msk_") ~ "Rheumatic",
      str_detect(condition, "digest") ~ "GI",
      str_detect(condition, "cirrhosis") ~ "Hepatic",
      str_detect(condition, "diabetes") ~ "Diabetes",
      str_detect(condition, "ckd") ~ "Renal",
      str_detect(condition, "dementia") ~ "Dementia",
      str_detect(condition, "encephalitis") ~ "Hemi/Paraplegia",
      .default = "lightgray"
  )) %>%
  filter(Category != "lightgray")

maleDegreeList <- lapply(maleNets, function(x) degree(x))
maleStrengthList <- lapply(maleNets, function(x) strength(x))

#Make state-level avg degree by 
maleDegreeStats <- bind_rows(maleDegreeList, .id = "state") 
maleDegreeStats <- maleDegreeStats %>%
  pivot_longer(cols = -state, names_to = "condition", values_to = "degree") %>%
  left_join(nodeCats %>%
              select(condition, Category)) %>%
  filter(!is.na(Category)) %>%
  group_by(state, Category) %>%
  summarise(sum = sum(degree, na.rm = T), mean = mean(degree, na.rm = T)) %>%
  left_join(CWFStateStats)

maleDegreeStats %>%
  ggplot(aes(x = `Uninsured adults ages 19–64, 2015–2024`,
             y = mean)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", color = "#246A78") +
  facet_wrap( ~ Category)


#Strength figure
maleStrengthStats <- bind_rows(maleStrengthList, .id = "state") 
maleStrengthStats <- maleStrengthStats %>%
  pivot_longer(cols = -state, names_to = "condition", values_to = "strength") %>%
  left_join(nodeCats %>%
              select(condition, Category)) %>%
  filter(!is.na(Category)) %>%
  group_by(state, Category) %>%
  summarise(sum = sum(strength, na.rm = T), mean = mean(strength, na.rm = T)) %>%
  left_join(CWFStateStats)

maleStrengthStats %>%
  ggplot(aes(x = `Uninsured adults ages 19–64, 2015–2024`,
             y = mean)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", color = "#246A78") +
  facet_wrap( ~ Category, scales = "free")
