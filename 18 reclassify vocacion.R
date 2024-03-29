

# Reclassify 
# Vocacion uso del suelo 
# April 13th 2023

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, fasterize, DataEditR, gtools, rgeos, stringr, tidyverse, rmapshaper)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
shpf <- sf::st_read('shp/vocacion/shp_ag_100k_vocacion_uso_2017/vocacionUso.shp')
biom <- sf::st_read('shp/biomes/biomes_geo.shp')

biom <- biom[-1,]

# SF to vect --------------------------------------------------------------
shp <- terra::vect(shpf)
shp <- terra::project(shp, 'EPSG:4326')

# Landcover raster --------------------------------------------------------
lcvr <- rast('./raster/input/variables/landcover/1_100000/rclf_lc_years.tif')
names(lcvr) <- glue('lcvr_{2001:2020}')
sins <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
terra::crs(lcvr) <- sins

# Labels for landcover
lbls.lcvr <- read_csv('tble/labels_rclf_modis.csv')

# Get unique class --------------------------------------------------------
clss <- shpf %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  distinct(Vocacion) %>% 
  mutate(rcl = NA)

hmgn <- read_csv('tble/labels_rclf_modis.csv')
clss <- read.csv('tble/vocacion_rcl.csv', encoding = 'latin1')[,-1]
clss <- inner_join(clss, clss %>% distinct(rcl) %>% mutate(rcl_gid = 1:nrow(.)), by = 'rcl')
clss_unqe <- clss %>% distinct(rcl, rcl_gid)
lbls.vccn <- clss_unqe

lbls.lcvr
lbls.vccn


# To reclassify  ----------------------------------------------------------
to_reclassify <- function(bio){
  
  # bio <- biom[i,]
  
  cat('Processing: ', bio$DeCodigo, '\n')
  nme <- bio$DeCodigo
  bio <- terra::vect(bio)
  bio <- terra::project(bio, 'EPSG:4326')
  bio.prj <- terra::project(bio, sins)
  
  # Intersection 
  ovr <- terra::crop(shp, bio)
  ovr <- st_as_sf(ovr)
  
  # Join with the reclassify table 
  ovr <- inner_join(ovr, clss, by = c('Vocacion' = 'Vocacion'))
  mss <- ms_dissolve(input = ovr, field = 'rcl')
  mss <- st_as_sf(mss)
  mss <- inner_join(mss, clss_unqe, by = 'rcl')
  
  # Get the mask 
  msk <- lcvr[[1]] * 0 + 1
  names(msk) <- 'mask'
  msk <- raster(msk)
  msk <- raster::crop(msk, as(bio.prj, 'Spatial'))
  msk <- raster::mask(msk, as(bio.prj, 'Spatial'))
  
  # To project 
  mss <- st_transform(mss, sins)
  
  # To rasterize with fasterize
  rst <- fasterize(sf = mss, raster = msk, field = 'rcl_gid')
  rst <- rast(rst)
  cat('Finish!\n')
  return(rst)
  
}

rclf <- map(1:nrow(biom), function(i){
  fnl <- to_reclassify(bio = biom[i,])
})
rclf
msco <- sprc(rclf)
msco <- terra::mosaic(msco)

terra::writeRaster(msco, 'raster/input/variables/vocacion/all_bioms.tif', overwrite = TRUE)
terra::writeRaster(rclf[[1]], 'raster/input/variables/vocacion/andean.tif', overwrite = TRUE)
terra::writeRaster(rclf[[2]], 'raster/input/variables/vocacion/orinoco.tif', overwrite = TRUE)
terra::writeRaster(rclf[[3]], 'raster/input/variables/vocacion/pacific.tif', overwrite = TRUE)
terra::writeRaster(rclf[[4]], 'raster/input/variables/vocacion/amazonas.tif', overwrite = TRUE)

# Overlay with each project -----------------------------------------------
redm <- terra::vect('gpkg/projects_3.gpkg')
redm <- terra::project(redm, sins)

# Function to get the area ------------------------------------------------
get_area <- function(red){
  
  cat('To start\n!')
  
  # Get the name of the red + project
  nme <- red$names
  
  # Extract by mask - Vocacion
  vcc <- terra::crop(msco, red)
  vcc <- terra::mask(vcc, red)
  
  # Extract by mask - Land use (all years)
  lcv <- terra::crop(lcvr, red)
  lcv <- terra::mask(lcv, red)
  
  # To get the frequency ----------------------------------------------------
  vcc.frq <- inner_join(freq(vcc)[,2:3], lbls.vccn, by = c('value' = 'rcl_gid'))[,c('rcl', 'value', 'count')]
  vcc.frq <- mutate(vcc.frq, area_ha = count)
  vcc.frq <- dplyr::select(vcc.frq, -value)
  colnames(vcc.frq)[1] <- 'class_vocacion'
  vcc.frq <- transmute(vcc.frq, class_vocacion, year = 2017, area_ha)
  
  lcv.frq <- mutate(freq(lcv), layer = 2000 + layer) %>% rename(year = layer)
  lcv.frq <- inner_join(lcv.frq, lbls.lcvr, by = c('value' = 'gid'))
  lcv.frq <- dplyr::select(lcv.frq, -value)
  colnames(lcv.frq)[3] <- 'class_landcover'
  lcv.frq <- relocate(lcv.frq, class_landcover, year, count)
  colnames(lcv.frq)[3] <- 'area_ha'
  
  # Join both tables into only one ------------------------------------------
  colnames(vcc.frq)[1] <- 'class'
  colnames(lcv.frq)[1] <- 'class'
  vcc.frq <- mutate(vcc.frq, type = 'vocation')
  lcv.frq <- mutate(lcv.frq, type = 'landuse')
  fnl <- rbind(vcc.frq, lcv.frq)
  fnl <- mutate(fnl, project = nme, .before = class)
  fnl <- as_tibble(fnl)
  cat('Done!\n')
  return(fnl)
  
}

# To apply the function ---------------------------------------------------
rslt <- map(1:nrow(redm), function(i)get_area(red = redm[i,]))
rslt <- bind_rows(rslt)
write.csv(rslt, 'tble/landuse_vocaction.csv', row.names = FALSE)




