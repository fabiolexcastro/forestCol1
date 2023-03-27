


# Check the names
# Mar 15 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# File name ---------------------------------------------------------------
fles <- dir_ls('tble/samples/v1', regexp = '.csv') %>% 
  grep('swd', ., value = T) %>% 
  as.character()

# Shape name --------------------------------------------------------------
znes <- terra::vect('shp/projects/projects.shp')

# Check  ------------------------------------------------------------------
nmes <- basename(fles)
nmes <- str_split(nmes, '_')
nmes <- unlist(nmes)
nmes <- nmes[grep('.csv', nmes, value = FALSE)]
nmes <- gsub('.csv', '', nmes)
nmes <- sort(nmes)

# Create a tibble
equi <- tibble(name_file = nmes, name_shape = sort(znes$MERGE_SRC))

write.csv(equi, 'equivalencia.csv', row.names = FALSE)
