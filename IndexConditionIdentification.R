library(tidyverse)
library(readxl)
library(medicalcoder)

### Identifying index conditions' codes
medicalcoder::get_charlson_codes()

#Get all charlson index codes
charlson_codes <- get_charlson_codes()
charlson_codes <- charlson_codes %>%
  filter(charlson_beyrer2021 == 1) %>%
  pull(code)

#Read excel of MarketScan causes and codes
CausesAndCodes <- read_excel("CausesAndCodes.xlsx")

CausesAndCodes <- CausesAndCodes %>%
  select(acause, ICD10) %>%
  separate_longer_delim(ICD10, delim = ", ")

indexConditions <- CausesAndCodes %>%
  filter(ICD10 %in% charlson_codes) %>%
  distinct(acause) %>%
  filter()
  pull(acause)
