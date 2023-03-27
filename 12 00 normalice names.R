
# Normalice names
# Mar 15 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
znes <- terra::vect('shp/projects/projects.shp')
biom <- terra::vect('shp/biomes/biomes_geo.shp')

# To change the names -----------------------------------------------------
nmes <- znes$MERGE_SRC
nmes <- gsub('Projects\\\\', '', nmes)
nmes <- str_split(nmes, '\\\\')
nmes <- map(nmes, 1)
nmes <- unlist(nmes)

# To change the empty characters ------------------------------------------
nmes <- gsub('_', '-', nmes)
nmes <- gsub(' ', '-', nmes)
nmes <- tolower(nmes)

# Check the duplicated names (there arent)
duplicated(nmes) %>% table()
# this these
znes$names <- nmes
znes
znes <- znes[,-c(1, 6)]
znes <- znes[,-c(1, 2)]

terra::writeVector(znes, 'gpkg/projects.gpkg')

# Get the slope -----------------------------------------------------------
srtm <- terra::rast('raster/input/variables/srtm/srtm_250_fill.tif')
slpe <- terra::terrain(srtm, v = 'slope', unit = 'degrees')
terra::writeRaster(slpe, 'raster/input/variables/srtm/slope_250_fill.tif')











