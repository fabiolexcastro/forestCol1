

# Prepare landcover variables
# Feb 27th 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- 'shp/landcover/Cobertura_tierra_2000_2002/shape'
mask <- terra::rast('raster/input/variables/accesibility/biom/accs_allbioms_250m.tif')
dpts <- terra::vect('shp/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
dpts <- terra::project(dpts, mask)

# Extract by mask 
mask <- terra::mask(mask, dpts)
terra::writeRaster(x = mask, filename = 'raster/input/variables/accesibility/biom/accs_allbioms_250m_v2.tif')

mask <- mask * 0 
terra::writeRaster(x = mask, filename = 'raster/input/mask/mask_250m.tif')

# 2002 --------------------------------------------------------------------

# Proof
fles <- dir_ls(path, regexp = '.shp$')

# Read as shapefile
shpf <- terra::vect(grep('.shp$', fles, value = T))

# Get the class
tbl <- as.data.frame(shp)
head(tbl)
tbl <- dplyr::select(tbl, CODIGO, NIVEL3)
tbl <- distinct(tbl)
  
lbl <- read_csv('tble/categories_2002.csv')
unq <- lbl %>% distinct(CATEGORIA) %>% mutate(gid = 1:nrow(.))
lbl <- inner_join(lbl, unq, by = 'CATEGORIA')

# To join class with the shapefile
shp <- inner_join(st_as_sf(shp), lbl, by = 'NIVEL3')
shp <- dplyr::select(shp, LEYENDA, CATEGORIA, gid, geometry)

shp_dss <- ms_dissolve(shp, field = 'gid')

st_write(shp, 'shp/landcover/Cobertura_tierra_2000_2002/shape/cover2000.gpkg')
st_write(shp, 'shp/landcover/Cobertura_tierra_2000_2002/shape/cobertura_tierra2000_2002V3.shp')

shpf <- vect('shp/landcover/Cobertura_tierra_2000_2002/shape/cobertura_tierra2000_2002V3.shp')

# Land cover clip by each biom --------------------------------------------
biom <- terra::vect('shp/biomes/biomes.shp')
biom <- terra::project(biom, shp)
shp <- vect(shp)

# Extract by each biom ----------------------------------------------------
purrr::map(.x = 1:nrow(biom), .f = function(i){
  
  cat(i, '\n')
  bio <- biom[i,]
  lcv <- terra::crop(shp, bio)
  
  
})





