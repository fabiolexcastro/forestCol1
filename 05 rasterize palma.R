

# Prepare palma
# Mar 6nd 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, fasterize, rmapshaper, gtools, rgeos, distanceto, raster)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data -----------------------------------------------------------------
fles <- dir_ls('shp/Agentes/Palma') %>% as.character() %>% grep('.shp$', ., value = T)
palm <- terra::vect(fles)
dpto <- vect('shp/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

# Intersection  -----------------------------------------------------------
palm <- terra::crop(palm, dpto)
writeVector(palm, 'shp/Agentes/Palma/points_palma_colombia.shp')

# Empty raster ------------------------------------------------------------
mask <- terra::rast('raster/input/mask/mask_250m.tif')
proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

# Project -----------------------------------------------------------------
proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'
palm <- terra::project(palm, proj)
dpto <- terra::project(dpto, proj)

palm <- terra::buffer(x = palm, width = 500)

# Distance to -------------------------------------------------------------
dpto <- st_as_sf(dpto)
col0 <- mutate(dpto, gid = 1)
col0 <- col0 %>% group_by(gid) %>% summarise() 
palm <- st_as_sf(palm)
palm <- mutate(palm, gid = 1)
palm <- palm %>% group_by(gid) %>% summarise()
dstn <- distance_raster(palm, cellsize = 250, st_bbox(col0), measure = 'geodesic', check = FALSE)

dstn <- terra::rast(dstn)
dpto <- vect(dpto)
dstn <- terra::crop(dstn, dpto)
dstn <- terra::mask(dstn, dpto)

dir_create('raster/input/variables/palma')
terra::writeRaster(x = dstn, filename = 'raster/input/variables/palma/dist_palma_proj.tif')













