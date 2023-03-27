
# Normalice names
# Mar 15 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, fs, glue, exactextractr, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
znes <- terra::vect('gpkg/projects.gpkg')
biom <- terra::vect('shp/biomes/biomes_geo.shp')
stck <- terra::rast('raster/input/variables/stack_allvars2.tif')

# Extract the landcover
pos.lcv <- grep('lcv', names(stck))
stck.lcvr <- stck[[pos.lcv]]

# Extract the forest
pos.frs <- grep('frst', names(stck))
stck.frst <- stck[[pos.frs]]

# Remove from the main stack 
pos <- c(pos.lcv, pos.frs)
dscr <- stck[[pos]]
stck <- stck[[-pos]]

# Mapping for the zonal ---------------------------------------------------

# Continous variables
znal.cntn <- map_dfr(.x = 1:nlyr(stck), .f = function(i){
  
  cat('... Processing: ', i, '\n')
  rst <- stck[[i]]
  znl <- exact_extract(rst, st_as_sf(znes), 'mean')
  rsl <- tibble(Bioma = znes$Bioma, Project = znes$names, vars = names(rst), values_mean = znl)
  return(rsl)
  
})

znal.cntn <- znal.cntn %>% spread(vars, values_mean)


# Discrete variables
znal.dscr <- map_dfr(.x = 1:nlyr(dscr), .f = function(i){
  
  cat('... Processing: ', i, '\n')
  rst <- dscr[[i]]
  znl <- exact_extract(rst, st_as_sf(znes), 'majority')
  rsl <- tibble(Bioma = znes$Bioma, Project = znes$names, vars = names(rst), values_mean = znl)
  return(rsl)
  
})

znal.dscr <- znal.dscr %>% spread(vars, values_mean) 












