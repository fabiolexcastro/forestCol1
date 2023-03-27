


# Get sample from the projects
# Mar 11 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
mask <- terra::rast('raster/input/mask/mask_250m.tif')
hnsn <- terra::rast('raster/input/variables/forest/hanse_thrs_com_2021.tif')
biom <- terra::vect('shp/biomes/biomes_geo.shp')

nmes <- biom$DeCodigo

# Make a substack 
hnsn <- hnsn[[c(1, 2)]]

# Project the mask 
mask <- terra::project(mask, biom)

# Function ----------------------------------------------------------------
my_resample <- function(nme){
  
  # Proof 
  # nme <- nmes[1]
  
  # Filtering biom
  bio <- biom[biom$DeCodigo == nme,]
  bio <- terra::project(bio, hnsn)
  
  # Extract by mask (HANSEN - BIOM)
  hns <- terra::crop(hnsn, bio)
  hns <- terra::mask(hns, bio)
  
  msk <- terra::crop(mask, bio)
  msk <- terra::mask(msk, bio)
  
  # Resample 
  frs <- terra::resample(hns[[1]], msk, method = 'near')
  lss <- terra::resample(hns[[2]], msk, method = 'near')
  
  rsl <- c(frs, lss)
  terra::writeRaster(x = rsl, filename = glue('raster/input/variables/forest/hansen_250m_{nme}_raw.tif'), overwrite = TRUE)
  cat('Done!\n')
  
}

# Apply the function ------------------------------------------------------
map(.x = nmes[2:length(nmes)], .f = my_resample)

# Check the results 
fles <- dir_ls('raster/input/variables/forest') %>% grep('250m', ., value = T) %>% as.character()
rstr <- map(fles, rast)
rstr <- sprc(rstr)
rstr <- mosaic(rstr)

rstr
terra::writeRaster(x = rstr, filename = 'raster/input/variables/forest/hansen_250m_Colombia_raw.tif')

# Read the results --------------------------------------------------------
hnsn <- terra::rast('raster/input/variables/forest/hansen_250m_Colombia_raw.tif')

# Forest / No Forest
frst <- list()
frst[[1]] <- hnsn[[1]]
frst[[1]][which.lyr(loss == 1)] <- 0 
names(frst[[1]]) <- glue('frst_nfrs_2001')

loss <- hnsn[[2]]

for(i in 2:21){
  
  cat(i + 2000, '\t')
  frst[[i]] <- frst[[i-1]]
  frst[[i]][which.lyr(loss == i)] <- 0
  names(frst[[i]]) <- glue('frst_nfrs_{2000 + i}')
  cat('Finish!\n')

}

frst <- do.call('c', frst)
frst <- c(hnsn[[1]], frst)
names(frst[[1]]) <- 'frst_nfrs_2000'

terra::writeRaster(x = frst, filename = './raster/input/variables/forest/hansen_250m_Colombia_frsnofrs.tif')

