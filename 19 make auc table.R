

# Make auc table from all the random forest models

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, cclust, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos, terra, fs, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dirs <- dir_ls('rData/rf_v1')
dirs <- as.character(dirs)

# Function to create the table --------------------------------------------
createTable <- function(dir){
  
  cat('Start\n')
  fls <- dir_ls(dir)
  fls <- as.character(fls)
  fls <- dir_ls(fls)
  
  auc <- grep('auc', fls, value = T)
  load(file = auc)
  auc <- tibble(project = basename(dir), run = glue('run_{1:length(auc)}'), auc = auc)
  return(auc)
  
  # ggplot(data = auc, aes(y = auc)) + geom_boxplot() + theme_minimal() 
  
}

# To apply the function ---------------------------------------------------
tbls <- map(dirs, createTable)
tbls
tbls <- bind_rows(tbls)

ggplot(data = tbls, aes(y = auc)) + 
  geom_boxplot() + 
  facet_wrap(~project) + 
  theme_minimal() 

write.csv(tbls, 'tble/auc_rf_allv1.csv', row.names = FALSE)

















