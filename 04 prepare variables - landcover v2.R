

# Prepare landcover variables
# Mar 7 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
shpf <- vect('shp/landcover/Cobertura_tierra_2000_2002/shape/cover2000.gpkg')
biom <- terra::vect('shp/biomes/biomes.shp')
geog <- '+proj=longlat +datum=WGS84 +no_defs +type=crs'
biom <- terra::project(biom, geog)
writeVector(biom, 'shp/biomes/biomes_geo.shp')

nort <- biom[1,]
writeVector(nort, 'shp/biomes/norte_geo.shp')
