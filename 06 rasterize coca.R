

# Prepare coca variables
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
fles <- map(dirs, dir_ls) %>% unlist %>% as.character %>% grep('.shp$', ., value = T)
dpto <- vect('shp/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

# Empty raster ------------------------------------------------------------
mask <- terra::rast('raster/input/mask/mask_250m.tif')

# Coca --------------------------------------------------------------------
coca <- grep('oca', fles, value = T)
coca <- vect(coca)

# Project -----------------------------------------------------------------
mask <- terra::project(mask, coca)
dpto <- terra::project(dpto, coca)
proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

# Distance to -------------------------------------------------------------
coca <- terra::project(coca, proj)
dpto <- terra::project(dpto, proj)
coca <- st_as_sf(coca)
coca <- mutate(coca, gid = 1)
coca_2 <- coca %>% group_by(gid) %>% summarise() %>% ungroup()
unique(st_geometry_type(coca))

dpto <- terra::project(dpto, coca_2)
dpto <- st_as_sf(dpto)

dstn <- distance_raster(coca_2, cellsize = 250, st_bbox(dpto), measure = 'geodesic', check = FALSE)
dstn <- terra::rast(dstn)
dstn <- terra::crop(dstn, dpto)
dstn <- terra::mask(dstn, dpto)

terra::writeRaster(x = dstn, filename = 'raster/input/variables/coca/dist_coca_proj.tif', overwrite = T)

dstn <- terra::rast('raster/input/variables/coca/dist_coca_proj.tif')
plot(dstn)







