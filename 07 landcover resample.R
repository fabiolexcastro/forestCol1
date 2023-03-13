
# Prepare landcover variables
# Mar 8 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# How to use cat
for(i in 1:10){
  cat('Processing: ', i + 2000, '')
  # print(i)
  cat('Done!\n')
}

# Functions to use --------------------------------------------------------
extract_mask <<- function(rst){
  # rst <- rstr[[1]]
  nme <- gsub("[^[:alpha:]]", "", names(rst))
  nme <- unique(nme)
  bio <- biom[biom$DeCodigo == nme,]
  bio <- terra::project(bio, sinus)
  rst <- terra::crop(rst, bio)
  rst <- terra::mask(rst, bio)
  return(rst)
}

# Load data ---------------------------------------------------------------
fles <- dir_ls('raster/input/variables/landcover/250', regexp = '.tif$')
rstr <- map(fles, rast)
biom <- terra::vect('shp/biomes/biomes_geo.shp')

# SINUS projection
sinus <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

# To make the mosaic ------------------------------------------------------
make_mosaic <- function(year){
  
  # year <- 1
  
  cat(year + 2000, '\n')
  trra <- map(1:5, function(i){rstr[[i]][[year]]})
  nmes <- gsub("[^[:alpha:]]", "", map_chr(trra, names))
  trra <- map(trra, extract_mask)
  trra <- sprc(trra)
  trra <- mosaic(trra)

  terra::writeRaster(x = trra, filename = glue('raster/input/variables/landcover/250/yearly/lc_250_{year + 2000}_proj.tif'), overwrite = TRUE)
  cat('Done!\n')
  
}

map(1:20, make_mosaic)

# To reclassify  ----------------------------------------------------------

# Check the results -------------------------------------------------------
rslt <- dir_ls('raster/input/variables/landcover/250/yearly', regexp = '.tif$')
rslt <- as.character(rslt)
rstr <- terra::rast(rslt)

# Colors and categories
lbls <- read.table('tble/labels_color_modis_2.csv', sep = ',', header = T)
lbls <- as_tibble(lbls)
lbls <- lbls[,-1]
colnames(lbls) <- c('value', 'color', 'class', 'class_rcl')
lbls <- inner_join(lbls, tibble(class_rcl = unique(lbls$class_rcl), gid = 1:10), by = 'class_rcl')

# To check the categories -------------------------------------------------
unqe <- map(1:nlyr(rstr), function(i) freq(rstr[[i]]))
map_dbl(unqe, nrow) %>% unique()

# Checar el value 0
val0 <- rstr[[1]]
val0[which.lyr(val0 != 0)] <- NA

# To make the reclassification --------------------------------------------
freq(rstr[[1]])

rclf <- map(.x = 1:nlyr(rstr), .f = function(i){
  
  cat('... Processing: ', 2000 + i, '\t')
  rst <- rstr[[i]]
  tbl <- as.data.frame(rst, xy = T)
  tbl <- as_tibble(tbl)
  colnames(tbl)[3] <- 'value'
  tbl <- inner_join(tbl, lbls, by = 'value')
  tbl <- dplyr::select(tbl, x, y, gid)
  rst <- terra::rast(tbl, type = 'xyz')
  cat('Done!\n')
  return(rst)
  
})

rclf <- do.call('c', rclf)
names(rclf) <- glue('LC_{2001:2020}')
terra::writeRaster(x = rclf, filename = 'raster/input/variables/landcover/rclf_lc_years.tif', overwrite = TRUE)












