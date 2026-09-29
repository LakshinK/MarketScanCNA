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

#Name the columna and rows
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
  
  #Name rows of matrix and remove column
  rownames(df) <- df[,1]
  df <- df[,-1]
  
  #Get rid of rows and columns for which there is not a corresponding one
  intersect_ids <- intersect(rownames(df), rownames(df))
  df <- df[intersect_ids, intersect_ids]
  
  #Make character values into numeric values
  #df[] <- as.numeric(df)
  mode(df) <- "double"
  
  #Set diagonal to 0
  diag(df) <- 0
  
  #Filter edges
  df[df < minRR] <- 0
  df
}

maleListMat <- lapply(maleList, function(x) makeAdjMat(x, 2))

#Make igraph objects
maleNets <- lapply(maleListMat, function(x) graph_from_adjacency_matrix(x, mode = "undirected", weighted = T))

#make df of each global statistic
netStats <- data.frame(
  state = names(maleNets),
  efficiency = unlist(lapply(maleNets, function(x) global_efficiency(x))),
  transitivity = unlist(lapply(maleNets, function(x) transitivity(x))),
  #avg_path = unlist(lapply(maleNets, function(x) mean_distance(x))),
  dens = unlist(lapply(maleNets, function(x) edge_density(x))),
  avg_degree = unlist(lapply(maleNets, function(x) mean(degree(x))))
)

# sampDF <- maleListMat[[1]]
# sampDF <- sampDF %>%
#   as.data.frame() %>%
#   mutate(a = rownames(sampDF)) %>%
#   pivot_longer(cols = -a, names_to = "b", values_to = "RR") %>% view()
# 
# sampDF %>%
#   filter(RR > 2)
