

# Prepare accesibility variables
# Feb 27th 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, fasterize, rmapshaper, gtools, rgeos, distanceto, raster)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load rivers -------------------------------------------------------------
dir_ls('shapes/variables/Rivers')
rvrs <- vect('shapes/variables/Rivers/Navigable Rivers.shp')
road <- vect('shapes/variables/Roads/roads_dissolve.shp')

# Filtering paved roads
road <- road[road$TIPO_VIA %in% 1:5,]

# Tidy the shapefile ------------------------------------------------------
road$type <- 'road'
rvrs$type <- 'river'

road <- road[,'type']
rvrs <- rvrs[,'type']

rvrs <- st_as_sf(rvrs)
rvrs <- mutate(rvrs, gid = 1)
rvrs <- rvrs %>% group_by(type) %>% summarise() 
rvrs <- vect(rvrs)

dir_create('gpkg/variables')
writeVector(road, 'gpkg/variables/road.gpkg')
writeVector(rvrs, 'gpkg/variables/rvrs.gpkg')

accs <- rbind(road, rvrs)
writeVector(accs, 'gpkg/variables/road_rvrs.gpkg', overwrite = T)

# Load results and biom ---------------------------------------------------
biom <- vect('shp/biomes/biomes.shp')
accs <- terra::vect('gpkg/variables/road_rvrs.gpkg')
accs <- terra::project(accs, crs(biom))

dpto <- terra::vect('shp/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
mpio <- terra::vect('shp/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')

proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

# For by each biom --------------------------------------------------------
map(.x = 2:nrow(biom), .f = function(i){
  
  cat(i, '\n')
  
  try(expr = {
    
    bio <- biom[i,]
    
    # Crop for the biom area
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
    res <- 250
    dst <- distance_raster(acc, cellsize = res, st_bbox(bio), measure = 'geodesic', check = FALSE)
    dst <- rast(dst)
    dst <- terra::crop(dst, vect(bio))
    dst <- terra::mask(dst, vect(bio))  
    
    nme <- bio$DeCodigo
    
    # Finish 
    terra::writeRaster(x = dst, filename = glue('raster/input/variables/accesibility/biom/accs_{nme}_30m.tif'), overwrite = TRUE)
    rm(dst); gc()
    cat('Finish!\n')
    return(dst)
    
  })
  
  
})

# To make the mosic from the accesibility results 
fles <- dir_ls('raster/input/variables/accesibility/biom')
rstr <- map(fles, rast)
rstr <- sprc(rstr)
rstr <- terra::mosaic(rstr)

terra::writeRaster(x = rstr, 'raster/input/variables/accesibility/')



# Load sf
library(sf)
# Load nc data
nc <- st_read(system.file("shapes/sids.shp", package="spData"))
st_crs(nc) <- "+proj=longlat +datum=NAD27"
# Select first 5 of nc
ncsub <- nc[1:5,]
# Generate a distance raster from some of nc within extent of all of nc
rst <- distance_raster(ncsub, 0.1, st_bbox(nc), measure = 'geodesic')







