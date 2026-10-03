library(tidyverse)
library(psych)
library(REdaS)

FAData <- CWFStateStats %>%
  select(-state) 

colnames(FAData) <- c(
  "Adults_CareAvoidedCost",
  "Adults_CareAvoidedCost",
  "Adults_Vaccinated",
  "Children_Vaccinated",
  "Deaths_preventable_treatable",
  "EmployerInsurance_EmployeeContributions",
  "EmployerInsurance_EmployeeContributions_ShareIncome",
  "EmployerInsurance_EmployeeDeductible",
  "EmployerInsurance_EmployeeDeductible_ShareIncome",
  "EmployeeOutOfPocket",
  "EmployeeOutOfPocket_ShareIncome",
  "HighOutOfPocket",
  "EmployerInsurance_PrimaryCareSpend_ShareSpend",
  "Medicare_PrimaryCareSpend_ShareSpend",
  "EmployerInsurance_PrimaryCareSpend_EmployeeSpend",
  "Medicare_PrimaryCareSpend_EmployeeSpend",
  "MedicalDebt",
  "PublicHealthSpend",
  "Employer_Reimbursement_inpatient",
  "Employer_Reimbursement_Office",
  "Employer_Reimbursement_Total",
  "Medicare_Reimbursement_Total",
  "Medicare_Reimbursement_Ambulatory",
  "Medicare_Reimbursement_Home",
  "Medicare_Reimbursement_Inpatient",
  "Medicare_Reimbursement_SNF",
  "Employer_TotalPremium",
  "Employer_TotalPremium_Share",
  "UninsuredAdults"
)

corMat <- FAData %>%
  cor(use = "pairwise.complete.obs") 

KMO(CWFStateStats %>%
      na.omit())

eigen(corMat)$values

#Use 4 factors based on simplicity, eigen decomposition, and scree plot (maybe 5)
scree(FAData, pc = FALSE)

fit <- factanal(FAData %>%
                  na.omit(), 6, rotation = "promax")

print(fit, digits = 2, sort = TRUE)
