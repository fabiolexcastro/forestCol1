

# Prepare accesibility variables
# Feb 27th 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, fasterize, rmapshaper, gtools, rgeos, distanceto, raster)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load results and biom ---------------------------------------------------
biom <- vect('shp/biomes/biomes.shp')
accs <- terra::vect('gpkg/variables/road_rvrs.gpkg')
accs <- terra::project(accs, crs(biom))

dpto <- terra::vect('shp/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
mpio <- terra::vect('shp/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')

proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

# For by each biom --------------------------------------------------------
map(.x = 1:nrow(biom), .f = function(i){
  
  cat(i, '\n')
  bio <- biom[i,]
  
  # Crop for the biom area
  # bio <- terra::project(bio, biom)
  bio <- terra::project(bio, accs)
  acc <- terra::crop(accs, bio)
  
  # To project
  bio <- terra::project(bio, proj)
  acc <- terra::project(acc, proj)
  
  # To simple feature
  bio <- st_as_sf(bio)
  acc <- st_as_sf(acc)
  
  # Correct the topology
  acc <- st_make_valid(acc)
  acc <- mutate(acc, gid = 1)
  acc <- acc %>% group_by(gid) %>% summarise()
  
  # Calculation of the distance between the roads / rivers
  res <- 30
  dst <- distance_raster(acc, cellsize = res, st_bbox(bio), measure = 'geodesic', check = FALSE)
  dst <- rast(dst)
  dst <- terra::crop(dst, mpo)
  dst <- terra::mask(dst, mpo)  
  
  nme <- mpo$MPIO_CCNCT
  
  # Finish 
  terra::writeRaster(x = dst, filename = glue('raster/input/variables/accesibility/mpios/accs_{nme}_30m.tif'), overwrite = TRUE)
  cat('Finish!\n')
  return(dst)
  
})



# Load sf
library(sf)
# Load nc data
nc <- st_read(system.file("shapes/sids.shp", package="spData"))
st_crs(nc) <- "+proj=longlat +datum=NAD27"
# Select first 5 of nc
ncsub <- nc[1:5,]
# Generate a distance raster from some of nc within extent of all of nc
rst <- distance_raster(ncsub, 0.1, st_bbox(nc), measure = 'geodesic')







