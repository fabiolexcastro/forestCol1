


# Get the area for land cover
# Mar 15 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, fs, glue, crayon, exactextractr, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- 'gpkg/projects.gpkg'
lcvr <- terra::rast('raster/input/variables/landcover/rclf_lc_years.tif')
lbls <- read_csv('tble/labels_rclf_modis.csv')

# Read path as shapefile 
znes <- vect(path)

# Coordinates system
geog <- '+proj=longlat +datum=WGS84 +no_defs +type=crs'
sinus <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

# Check the coordinate system ---------------------------------------------
if(is.lonlat(znes) == TRUE){
  cat(green('Coordinate system is WGS 1984\n'))
} else {
  cat(red('Coordinate system is NOT WGS 1984, to project\n'))
  znes <- terra::project(znes, geog)
}

# To project --------------------------------------------------------------
znes_proj <- terra::project(znes, sinus)

# Function to use ---------------------------------------------------------
count_area <- function(zne){
  
  # zne <- znes_proj[1,]
  
  nme <- zne$names
  cat('... Processing: ', nme, '\t')
  lcv <- terra::crop(lcvr, zne)
  lcv <- terra::mask(lcv, zne)
  
  cat('Get the frequency\t')
  frq <- freq(lcv)
  frq <- as_tibble(frq)
  colnames(frq) <- c('year', 'class', 'count_px')
  frq <- mutate(frq, year = year + 2000, zone = nme)
  frq <- relocate(frq, zone)
  ha  <- res(lcv)[1] * res(lcv)[2] / 1e4
  frq <- mutate(frq, has = count_px * ha)
  frq <- dplyr::select(frq, -count_px)
  frq <- frq %>% 
    group_by(zone, year) %>% 
    mutate(porc = has / sum(has) * 100) %>% 
    ungroup()
  frq <- inner_join(frq, lbls, by = c('class' = 'gid'))
  frq <- relocate(frq, zone, year, class_rcl)
  frq <- dplyr::select(frq, -class)
  cat('Done!\n')
  return(frq)
  
}

# To apply the function ---------------------------------------------------
areas <- map(.x = 1:nrow(znes), .f = function(j){
  rslt <- count_area(zne = znes_proj[j,])
  return(rslt)
})
areas <- bind_rows(areas)

# Change the path and the name
write.csv(areas, 'tble/count_area_projects.csv', row.names = FALSE)










