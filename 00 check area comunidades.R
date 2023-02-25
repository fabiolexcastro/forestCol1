

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, gtools, glue, openxlsx, crayon, xlsx, hablar, rgeos, gfcanalysis, raster, Synth, gsynth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
znes <- vect('data/gpk/zones_afro_indi.gpkg')
cmnd <- unique(znes$Comunidad)
biom <- vect('data/shp/Comunidades2/biomes.shp')

# Coordinate system
proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'
geog <- '+proj=longlat +datum=WGS84 +no_defs +type=crs'

# Map... ------------------------------------------------------------------
shpf <- purrr::map(.x = 1:length(cmnd), .f = function(i){
  
  cat(cmnd[i], '\n')
  cmn <- cmnd[i]
  zne <- znes[znes$Comunidad == cmn,]
  zne <- terra::project(x = zne, y = terra::crs(biom))
  
  if(nrow(zne) > 0){
    print('Major than 0')
    zne <- zne[1,]
  } else {
    print('No major than 0')
    zne
  }
  
  rsl <- terra::intersect(zne, biom)
  are <- terra::expanse(rsl)
  rsl$area <- are
  return(rsl)
    
})

# Choose the polygon with major area (biome) ------------------------------
rslt <- purrr::map(.x = 1:length(shpf), .f = function(i){
  
  cat(green(cmnd[i]), '\n')
  cmn <- shpf[[i]]
  cmn <- cmn[which.max(cmn$area),]
  zne <- znes[znes$Comunidad == cmnd[i],]
  zne <- zne[1,]
  zne$Bioma <- zne$Bioma
  cat(yellow('Done!\n'))
  return(zne)
  
})


fnal <- map(rslt, st_as_sf)
fnal <- bind_rows(fnal)
fnal <- vect(fnal)

writeVector(fnal, filename = 'data/gpk/zones_afro_indi_v2.gpkg')













