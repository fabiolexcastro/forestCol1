

# Get sample from the projects
# Mar 10 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
znes <- terra::vect('shp/projects/projects.shp')

# Get a identifier for each project ---------------------------------------
lbls <- znes %>% as.data.frame %>% as_tibble %>% dplyr::select(MERGE_SRC)
lbls <- mutate(lbls, name = gsub('Projects\\\\', '', MERGE_SRC))
lbls <- mutate(lbls, ID = paste0('P_', 1:nrow(lbls)))

znes <- vect(inner_join(st_as_sf(znes), lbls, by = 'MERGE_SRC'))

# Read the variables ------------------------------------------------------
minr <- terra::rast('raster/input/variables/mineria/dist_mineria_proj.tif')
coca <- terra::rast('raster/input/variables/coca/dist_coca_proj.tif')
palm <- terra::rast('raster/input/variables/palma/dist_palma_proj.tif')
accs <- terra::rast('raster/input/variables/accesibility/biom/accs_allbioms_250m.tif')







