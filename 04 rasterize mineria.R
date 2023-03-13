

# Prepare minery variables
# Mar 3rd 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, fasterize, rmapshaper, gtools, rgeos, distanceto, raster)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data -----------------------------------------------------------------
dirs <- dir_ls('shp/Agentes') %>% as.character()
fles <- map(dirs, dir_ls) %>% unlist %>% as.character %>% grep('.shp', ., value = T)

dpto <- vect('shp/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

# Empty raster ------------------------------------------------------------
mask <- terra::rast('raster/input/mask/mask_250m.tif')

# Licenses ----------------------------------------------------------------
lcns <- grep('Licenciadas', fles, value = T)
lcns <- vect(lcns)

# Project -----------------------------------------------------------------
mask <- terra::project(mask, lcns)
dpto <- terra::project(dpto, lcns)
proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

# Distance to -------------------------------------------------------------
lcns <- terra::project(lcns, proj)
dpto <- terra::project(dpto, proj)
lcns <- st_as_sf(lcns)
lcns <- mutate(lcns, gid = 1)
lcns_2 <- lcns <- lcns %>% group_by(gid) %>% summarise() %>% ungroup()
st_geometry_type(lcns_2)

dstn <- distance_raster(lcns_2, cellsize = 250, st_bbox(dpto), measure = 'geodesic', check = FALSE)
dstn <- terra::rast(dstn)
dstn <- terra::crop(dstn, dpto)
dstn <- terra::mask(dstn, dpto)

terra::writeRaster(x = dstn, filename = 'raster/input/variables/mineria/dist_mineria_proj.tif')









