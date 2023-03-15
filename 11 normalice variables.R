

# Get sample from the projects
# Mar 13 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
znes <- terra::vect('shp/projects/projects.shp')

# Get a identifier for each project ---------------------------------------
lbls <- znes %>% as.data.frame %>% as_tibble %>% dplyr::select(MERGE_SRC)
lbls <- mutate(lbls, name = gsub('Projects\\\\', '', MERGE_SRC))
lbls <- mutate(lbls, ID = paste0('P_', 1:nrow(lbls)))

znes <- vect(inner_join(st_as_sf(znes), lbls, by = 'MERGE_SRC'))

# Read the variables ------------------------------------------------------
minr <- terra::rast('raster/input/variables/mineria/dist_mineria_proj.tif')
coca <- terra::rast('raster/input/variables/coca/dist_coca_proj.tif')
palm <- terra::rast('raster/input/variables/palma/dist_palma_proj.tif')
accs <- terra::rast('raster/input/variables/accesibility/biom/accs_allbioms_250m.tif')
frst <- terra::rast('raster/input/variables/forest/hansen_250m_Colombia_frsnofrs.tif')
lcvr <- dir_ls('raster/input/variables/landcover/250/yearly') %>% grep('.tif$', ., value = T) %>% as.character() %>% terra::rast()
prec <- terra::rast('raster/input/variables/climate/prec_250m.tif')
tavg <- terra::rast('raster/input/variables/climate/tavg_250m.tif')
srtm <- terra::rast('raster/input/variables/srtm/srtm_250_fill.tif')\

geog <- '+proj=longlat +datum=WGS84 +no_defs +type=crs'
mask <- terra::rast('raster/input/mask/mask_250m.tif')
mask <- terra::project(mask, geog)
base <- mask

# Check the coordinate system ---------------------------------------------

list <- list(minr, coca, palm, accs, frst, lcvr, prec, tavg, srtm)
crds <- map(list, crs)
table(crds)
crds
unique(crds)

chck <- map(.x = 1:length(list), .f = function(i){
  
  cat('Processing: ', '\n')
  lst <- list[[i]]
  cnd <- is.lonlat(lst)
  
  if(cnd == FALSE){
    cat(red('Coordinate system is not equal!\n'))
    lst <- terra::project(lst, geog)
  } else {
    cat(green('Coordinate system is equal!\n'))
    lst
  }
  
  return(lst)
  
})

# Resampling 
fnal <- map(.x = 1:length(chck), .f = function(i){
  
  cat(i, '\n')
  rsl <- terra::resample(chck[[i]], base)
  return(rsl)
  
})

fnal <- do.call('c', fnal)
names(fnal)[1:3] <- c('mineria', 'coca', 'palma')
names(fnal)[27:46] <- glue('lcv_{2001:2020}')

terra::writeRaster(x = fnal, filename = 'raster/input/variables/stack_allvars.tif')

# Check the EPSG ------------------------------------------
r <- rast()
crs(r)
crs(r, describe=TRUE, proj=TRUE)
crs(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
crs(r)
# You can use epsg codes
crs(r) <- "epsg:25831"
crs(r)
crs(r, describe=TRUE)$area
crs("epsg:25831", describe=TRUE)
