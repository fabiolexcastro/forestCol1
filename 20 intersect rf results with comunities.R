

# Intersection rf results - comunities

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, cclust, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos, terra, fs, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
proj <- terra::vect('gpkg/projects_3.gpkg')
biom <- terra::vect('shp/comunities/biomes.shp') %>% terra::project(., 'EPSG:4326')

# Comunities --------------------------------------------------------------
cmns <- terra::vect('shp/comunities/comunidades.shp') %>% terra::project(., 'EPSG:4326')
lbls <- suppressMessages(read_csv('shp/comunities/Community Type.csv'))
lbls <- filter(lbls, type %in% c('afro', 'indig'))

# Intersection between comunitites and biom -------------------------------
biom
cmns.biom <- terra::intersect(cmns, biom)
as_tibble(as.data.frame(table(cmns.biom$comunidad))) %>% arrange(desc(Freq))

# Random forest -----------------------------------------------------------
dirs <- as.character(dir_ls('rf/output/run_1/results'))

# Filtering dirs (just inigenous and afro) --------------------------------
pull(lbls, project)

dirs <- grep(paste0(pull(lbls, project), collapse = '|'), dirs, value = TRUE)

# Function to use v1 ------------------------------------------------------
my_function <- function(dir){
  
  # dir <- dirs[2]
  
  cat('To start: ', basename(dir), '\n')
  fls <- dir_ls(dir)
  trj <- 'Clust' # To change if we want
  rst <- terra::rast(as.character(grep('.asc', grep(trj, fls, value = TRUE), value = TRUE)))
  nme <- basename(dir)
  
  # The project is indigenous or afro?
  lbl <- filter(lbls, project == nme)
  tpe <- pull(lbl, type)
  bio <- pull(lbl, biome)
  
  # Filtering in the comunities shapefile
  cmn <- cmns.biom[cmns.biom$type == tpe & cmns.biom$redd == 0 & cmns.biom$biome == bio,]
  
  if(nrow(cmn) == 0){
   print(paste0('Nrow == 0 ', nme))
  }
  
  rst[which.lyr(rst %in% 1:2)] <- NA
  rst <- rst * 0 + 1
  pol <- as.polygons(rst)
  pol.cmn <- terra::intersect(pol, cmn)
  terra::writeVector(pol.cmn, filename = glue('gpkg/cmns_rfrs/rf_cmn_{nme}_{tpe}_{bio}.gpkg'), overwrite = TRUE) 
  cat('Done!\n')
  
}

# To apply the function ---------------------------------------------------
map(dirs[4:length(dirs)], my_function)
