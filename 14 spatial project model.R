
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, cclust, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos, terra, fs, glue)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
myproj <- CRS('+proj=longlat +datum=WGS84')

# Source ------------------------------------------------------------------
source('https://raw.githubusercontent.com/fabiolexcastro/rfSalvador/master/FunctionsRFclustering.R')

# Functions to use --------------------------------------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  # occ = back_swd; nforest = 50; ntrees = 500; nVars = 8; nclasses = 2
  datRF_presences <- occ[,3:ncol(occ)] %>% as.data.frame()
  print(nrow(datRF_presences))
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  return(list(labelRF, clusterdata))
}

# Load data ---------------------------------------------------------------

# Clustered data 
dirs <- dir_ls('rData/rf_v1') %>% as.character()
dirs <- glue('{unlist(dirs)}/run_1')
map(dirs, dir_ls)

# Pseudo-absences 
fles.back <- dir_ls('tble/samples/v1', regexp = '.csv') %>% grep('swd', ., value = T) %>% as.character()

# Cluster analysis to pseudo-absences -------------------------------------
purrr::map(.x = 1:length(fles.back), .f = function(i){
  
  back_swd <- read_csv(fles.back[i]) %>% filter(pb == 0)
  proj_nme <- unique(back_swd$project)
  back_swd <- dplyr::select(back_swd, -project, -pb, -ID)
  back_swd <- drop_na(back_swd)
  
  if(nrow(back_swd) > 10000){
    message('Major than 10000 presences')
    back_swd <- sample_n(tbl = back_swd, size = round(nrow(back_swd) * 0.01, 0), replace = FALSE)
  } else if(nrow(back_swd) > 5000 & nrow(back_swd) < 10000) {
    message('Between 5000 and 10000 presences')
    back_swd <- sample_n(tbl = back_swd, size = round(nrow(back_swd) * 0.02, 0), replace = FALSE)
  }
  
  back_swd <- as.data.frame(back_swd)
  datRF <- as.data.frame(back_swd[,3:ncol(back_swd)])
  attach(datRF)
  no.forests <- 50
  no.trees <- 3
  no.absenceclasses <- 2
  distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)
  labelRF <- pamNew(distRF$cl1, no.absenceclasses)
  detach(datRF)
  classdata <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])
  
  # Read the clusteredpresdata
  name <- str_split(dirs[1], '/') %>% map(3) %>% unlist() %>% str_split('_') %>% map(1) %>% unlist()
  dirf <- grep(name, dirs[1], value = T)
  dir_ls(dirf) %>% grep('clustereddata', ., value = T) %>% as.character() %>% load()
  
  presvalue_swd  <- clusteredpresdata[,3:ncol(clusteredpresdata)] %>%
    cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
    na.omit() %>%
    as.data.frame() %>%
    mutate(cluster = cluster + no.absenceclasses)
  
  
})


bckclust <- rf.clust(occ = back_swd, nforest = 50, ntrees = 500, nVars = 8, nclasses = 2)



