

# Add uncertainty zones to the cluster areas
# Random Forest model 
# April 3 2023

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, fs, glue, crayon, exactextractr, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# List the directories ----------------------------------------------------
dirs <- dir_ls('rf/output/run_1/results') %>% as.character()
dirs

# Function to add limitations ---------------------------------------------
add_limitations <- function(dir){
  
  # dir <- dirs[1]
  
  cat('Processing: ', basename(dir), '\n')
  fls <- dir_ls(dir)
  prb <- terra::rast(grep('Prob', fls, value = T))
  cls <- terra::rast(grep('Clust', fls, value = T))
  unc <- terra::rast(grep('Unc', fls, value = T))
  
  # Read the presences
  rds <- dir_ls('rData/rf_v1') %>% grep(basename(dir), ., value = T) %>% as.character() %>% dir_ls() %>% dir_ls()
  load(file = as.character(grep('clustereddata', rds, value = T)))
  
  # Limitations -------------------------------------------------------------

  # Extract the threshold
  vls <- as_tibble(cbind(occr[,c('x', 'y')], terra::extract(prb, occr[,c('x', 'y')])))
  colnames(vls)[4] <- 'value'
  qnt <- quantile(vls$value, seq(0, 1, 0.01))
  thr <- qnt[6]
  
  # Matrix 
  mtx.prb <- matrix(c(0, thr, 0, thr, 1, 2), ncol = 3, byrow = T)
  mtx.cls <- matrix(c(0.5, 2 + 0.5, 0, 2 + 0.5, 2 + 5 + 0.5, 1), nrow = 2, byrow = T)
  
  prb.rcl <- terra::classify(prb, mtx.prb)
  cls.rcl <- terra::classify(cls, mtx.cls)
  
  dff <- prb.rcl - cls.rcl
  rsl <- cls
  rsl[which.lyr(dff == -1)] <- 2 + 5 + 1
  rsl[which.lyr(dff == 2)]  <- 2 + 5 + 1
  
  terra::writeRaster(x = rsl, filename = glue('{dir}/RF_limitations.asc'), overwrite = TRUE)
  
  # Mixed -------------------------------------------------------------------
  vls <- terra::extract(unc, occr[,c('x', 'y')])[,2]
  qnt <- quantile(vls, seq(0, 1, 0.01))
  thr.unc <- qnt[11]
  rsl[which.lyr(unc < thr.unc & prb > thr)] <- 9
  terra::writeRaster(x = rsl, filename = glue('{dir}/RF_uncertainty.asc'), overwrite = TRUE)
  cat('Finish everything!\n')
  return(rsl)
  
}

# To apply the functions --------------------------------------------------
map(dirs, add_limitations)

