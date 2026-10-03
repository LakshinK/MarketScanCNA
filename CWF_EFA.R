library(tidyverse)
library(psych)

CWFStateStats %>%
  select(-state) %>%
  cor() %>% view()

CWFStateStats %>%
  select(-state) %>%
  KMO()
