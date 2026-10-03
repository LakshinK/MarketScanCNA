library(tidyverse)
library(arrow)
library(readxl)
library(igraph)

#Read data
liftData <- read_parquet("marketscan_adjusted_lift_location.parquet")
cause_ids <- read_xlsx("Cause_ids_offline.xlsx")
location_ids <- read_xlsx("us_location_names.xlsx")

#Make location id spaces into underscores
location_ids$location_name <- gsub(" ","_", location_ids$location_name) 

#Make vector of sparse conditions (from Taylor)
sparse <- c(320,
  338,
  346,
  347,
  348,
  349,
  350,
  351,
  352,
  353,
  354,
  355,
  356,
  358,
  362,
  364,
  404,
  405,
  511,
  513,
  514,
  698,
  720,
  721,
  729,
  843,
  854,
  935,
  936,
  945,
  958,
  989,
  1005,
  1173,
  1174,
  1175)

includedCauses <- cause_ids %>%
  filter(!cause_id %in% sparse)

#Exclude maternal and neonatal diseases
excludedCauses <- cause_ids %>%
  filter(cause_id %in% sparse | 
           str_detect(cause_abb, pattern = "maternal") |
           str_detect(cause_abb, pattern = "neonatal") |
           str_detect(cause_abb, pattern = "inj_") |
           str_detect(cause_abb, pattern =  "neo_eye_other") | 
           str_detect(cause_abb, pattern = "resp_pneum_asbest") |
           str_detect(cause_abb, pattern = "neo_")) %>%
  pull(cause_abb)

#Name the columns and rows
liftData <- liftData %>%
  left_join(location_ids) %>%
  left_join(cause_ids %>%
              select(cause_id, cause_abb)) %>%
  mutate(location_id = location_name) %>%
  select(-location_name) %>%
  mutate(cause_id = cause_abb) %>%
  select(-cause_abb)

id_map <- setNames(cause_ids$cause_abb, cause_ids$cause_id)
liftData <- liftData %>%
  rename_with(~ ifelse(. %in% names(id_map), id_map[.], .))

#Split data into male and female (ask for together)
maleData <- liftData %>%
  filter(sex_id == 1)
femaleData <- liftData %>%
  filter(sex_id == 2)

#Make a list where each item is a dataframe
maleList <- split(maleData, maleData$location_id)
femaleList <- split(femaleData, femaleData$location_id)


#Process list items into adjacency matrices
makeAdjMat <- function(df, minRR){
  df <- df %>%
    select(-sex_id, -location_id) %>%
    as.matrix()
  
  #Name rows of matrix and remove name column
  rownames(df) <- df[,1]
  df <- df[,-1]
  
  #Get rid of rows and columns for which there is not a corresponding one
  intersect_ids <- intersect(rownames(df), rownames(df))
  df <- df[intersect_ids, intersect_ids]
  
  #Get rid of sparse/excluded rows/cols
  df <- df[setdiff(rownames(df), excludedCauses),
           setdiff(colnames(df), excludedCauses), drop = FALSE]
  
  mode(df) <- "double"
  #Make character values into numeric values
  #df[] <- as.numeric(df)
  
  #Set diagonal to 0
  diag(df) <- 0
  
  #Log10
  df <- ifelse(df != 0, log10(df), 0)
  
  #Filter edges
  df[df < minRR] <- 0
  
  #Only keep edges that are connected to >=1 node in index
  df[!(rownames(df) %in% indexConditions), !(colnames(df) %in% indexConditions)] <- 0

  df
}

maleListMat <- lapply(maleList, function(x) makeAdjMat(x, 0.30103))
femaleListMat <- lapply(femaleList, function(x) makeAdjMat(x, 2))

#Make igraph objects
maleNets <- lapply(maleListMat, function(x) graph_from_adjacency_matrix(x, mode = "undirected", weighted = T))
femaleNets <- lapply(femaleListMat, function(x) graph_from_adjacency_matrix(x, mode = "undirected", weighted = T))

#make df of each global statistic
maleNetStats <- data.frame(
  state = names(maleNets),
  efficiency = unlist(lapply(maleNets, function(x) global_efficiency(x))),
  transitivity = unlist(lapply(maleNets, function(x) transitivity(x))),
  #avg_path = unlist(lapply(maleNets, function(x) mean_distance(x))),
  dens = unlist(lapply(maleNets, function(x) edge_density(x))),
  avg_degree = unlist(lapply(maleNets, function(x) mean(degree(x))))
)

femaleNetStats <- data.frame(
  state = names(femaleNets),
  efficiency = unlist(lapply(femaleNets, function(x) global_efficiency(x))),
  transitivity = unlist(lapply(femaleNets, function(x) transitivity(x))),
  #avg_path = unlist(lapply(maleNets, function(x) mean_distance(x))),
  dens = unlist(lapply(femaleNets, function(x) edge_density(x))),
  avg_degree = unlist(lapply(femaleNets, function(x) mean(degree(x))))
)

# sampMat <- maleListMat[[1]]
# 
# sampMat %>%
#   as.data.frame() %>%
#   mutate(i = rownames(sampMat)) %>%
#   pivot_longer(-i, names_to = "j", values_to = "value") %>%
#   view()
netStats <- rbind(maleNetStats %>%
        mutate(sex = "Male"), 
      femaleNetStats %>%
        mutate(sex = "Female"))
