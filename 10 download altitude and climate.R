



# Download altitude and prec/temp
# March 13 th 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper, elevatr)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Study zone --------------------------------------------------------------
biom <- terra::vect('shp/biomes/biomes_geo.shp')
mask <- terra::rast('raster/input/mask/mask_250m.tif')

# Download altitude  ------------------------------------------------------
srtm <- elevatr::get_elev_raster(locations = st_as_sf(biom), z = 10)
srtm <- terra::rast(srtm)

geog <- '+proj=longlat +datum=WGS84 +no_defs +type=crs'

# Project the mask --------------------------------------------------------
mask <- terra::project(mask, geog)

# To resampling -----------------------------------------------------------
srtm
srtm.rsmp <- map(.x = 1:nrow(biom), .f = function(i){
  
  cat(i, '\n')
  bio <- biom[i,]
  
  # Crop the SRTM by each biom
  srt <- terra::crop(srtm, bio)
  srt <- terra::mask(srt, bio)
  
  # Crop the mask by each biom
  msk <- terra::crop(mask, bio)
  msk <- terra::mask(msk, bio)
  
  # Resampling
  srt <- terra::resample(srt, msk, method = 'bilinear')
  return(srt)
  
})

srtm.rsmp <- sprc(srtm.rsmp)
srtm.rsmp <- mosaic(srtm.rsmp)
terra::writeRaster(x = srtm.rsmp, filename = 'raster/input/variables/srtm/srtm_250_raw.tif')
srtm.fill <- terra::rast('raster/input/variables/srtm/srtm_250_fill.tif')

# Download climate  -------------------------------------------------------
prec <- geodata::worldclim_country(country = 'COL', var = 'prec', path = 'tmpr')
tavg <- geodata::worldclim_country(country = 'COL', var = 'tavg', path = 'tmpr')

prec <- sum(prec)
tavg <- mean(tavg)

prec <- terra::crop(prec, biom)
prec <- terra::mask(prec, biom)

tavg <- terra::crop(tavg, biom)
tavg <- terra::mask(tavg, biom)

dout <- 'raster/input/variables/climate'
dir_create(dout)
terra::writeRaster(x = prec, filename = glue('{dout}/prec_1km.tif'), overwrite = T)
terra::writeRaster(x = tavg, filename = glue('{dout}/tavg_1km.tif'), overwrite = T)

# To make the GWR ---------------------------------------------------------
library(RSAGA)
env <- rsaga.env(path = 'C:/saga-7.9.0_x64')

file.srtm <- 'raster/input/variables/srtm/srtm_250_fill.tif'
file.prec <- glue('{dout}/prec_1km.tif')
file.tavg <- glue('{dout}/tavg_1km.tif')

# Prec
rsl <- rsaga.geoprocessor(
  lib = 'statistics_regression',
  module = 'GWR for Grid Downscaling',
  param = list(PREDICTORS = file.srtm,
               REGRESSION = 'raster/input/variables/climate/prec_250m.tif',
               DEPENDENT = file.prec),
  env = env)

prec_250 <- rast('raster/input/variables/climate/prec_250m.tif')

# Tavg
rsl <- rsaga.geoprocessor(
  lib = 'statistics_regression',
  module = 'GWR for Grid Downscaling',
  param = list(PREDICTORS = file.srtm,
               REGRESSION = 'raster/input/variables/climate/tavg_250m.tif',
               DEPENDENT = file.tavg),
  env = env)

tavg_250 <- rast('raster/input/variables/climate/tavg_250m.tif')

plot(tavg_250)


c(srtm.fill, tavg_250, prec_250)





