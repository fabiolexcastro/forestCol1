
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, gtools, openxlsx, xlsx, hablar, rgeos, gfcanalysis, raster, Synth, gsynth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
znes <- vect('data/gpk/zones_raw.gpkg')
znes <- vect('data/shp/Comunidades2/Comunidades')

# Comunidades 
fles <- dir_ls('data/shp/Comunidades2/Comunidades', regexp = '.shp$')
fles <- grep('nonproj', fles, value = T) %>% as.character()
znes <- map(fles, vect)

# Afro
afro <- znes[[1]][,c('Comunidad', 'Bioma')] 
afro$PUEBLO <- afro$Comunidad
afro$tipo <- 'Afro'

# Indigenas
indi <- znes[[2]][,c('Comunidad', 'Bioma', 'PUEBLO')]
indi$tipo <- 'Indigenas'

# Join 
znes <- rbind(afro, indi)
writeVector(znes, 'data/gpk/zones_afro_indi.gpkg')

# Download gfcanalysis ----------------------------------------------------

# Check the tiles
tles <- calc_gfc_tiles(st_as_sf(znes))
dout <- 'data/tif/raw_comunidade_hansen_2021'

download_tiles(tles, dout, dataset = 'GFC-2021-v1.9')

dir_ls(dout)

# To extract by mask
znes <- st_as_sf(znes)
extr <- extract_gfc(aoi = znes, data_folder = dout, dataset = 'GFC-2021-v1.9')
thrs <- threshold_gfc(gfc = extr)

extr <- rast(extr)
thrs <- rast(thrs)

# To write the results
terra::writeRaster(thrs, 'data/tif/hanse_thrs_com_2021.tif', overwrite = TRUE)
terra::writeRaster(extr, 'data/tif/hanse_extr_com_2021.tif', overwrite = TRUE)

# Read the results --------------------------------------------------------
thrs <- terra::rast('data/tif/hanse_thrs_com_2021.tif')
extr <- terra::rast('data/tif/hanse_extr_com_2021.tif')
dir_ls('data/tif')
proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

# To extract by mask for each project -------------------------------------
extractMask <- function(zne){
  
  # Proof
  # zne <- znes[62,]
  
  # Starting
  cat('Processing, start: ', unique(zne$Comunidad), '\n')
  nme <- zne$Comunidad
  # str <- zne$Inicio
  
  cat('To extract by mask ----\t')
  rst <- terra::crop(thrs, zne)
  rst <- terra::mask(rst, zne)
  
  # Get the resolution
  msk <- terra::project(rst[[1]], proj)
  res <- (res(msk)[1] * res(msk)[2]) / 10000
  
  cat('To calculate the frequency of pixels\t')
  
  # Project area
  are <- sum(pull(freq(msk), count)) * res
  
  # Forest 2000
  lss <- tibble(value = c(0, 1), type = c('No bosque', 'Bosque'))
  
  frq.frs <- freq(rst[[1]])
  frq.frs <- as_tibble(frq.frs)
  frq.frs <- inner_join(frq.frs, lss, by = 'value')
  frq.frs <- mutate(frq.frs, has_bosque_2000 = count * res, nombre = nme)
  frq.frs <- dplyr::select(frq.frs, nombre, type, has_bosque_2000)
  
  # Loss
  frq.lss <- freq(rst[[2]])
  frq.lss <- as_tibble(frq.lss)
  frq.lss <- mutate(frq.lss, year = 2000 + value)
  frq.lss <- dplyr::filter(frq.lss, year != 2000)
  frq.lss <- mutate(frq.lss, has_perdida = count * res, nombre = nme)
  frq.lss <- dplyr::select(frq.lss, nombre, year, has_perdida)
  frq.lss <- mutate(frq.lss, year = paste0('x', year))
  frq.lss <- mutate(frq.lss, has_perdida_cum = cumsum(has_perdida))
  
  # To get forest / no forest (all the period)
  frs.00 <- frq.frs %>% filter(type == 'Bosque') 
  nfr.00 <- frq.frs %>% filter(type == 'No bosque')
  
  if(nrow(frs.00) == 0){
    frs.00 <- 0
  } else {
    frs.00 <- frs.00 %>% pull(3)
  }
  
  if(nrow(nfr.00) == 0){
    nfr.00 <- 0
  } else {
    nfr.00 <- nfr.00 %>% pull(3)
  }
  
  
  frq.lss <- mutate(frq.lss, has_bosque = frs.00 - has_perdida_cum)
  colnames(frq.lss) <- c('nombre', 'year', 'has_perdida', 'has_perdida_cum', 'has_bosque')
  frq.lss <- mutate(frq.lss, has_no_bosque = has_perdida_cum + nfr.00)
  frq.lss <- rbind(c(nme, 'x2000', NA, NA, frs.00, nfr.00), frq.lss) %>% retype()
  # frq.lss <- mutate(frq.lss, startProject = str)
  # frq.lss <- relocate(frq.lss, startProject, .before = year)
  frq.lss <- mutate(frq.lss, has_projectTotal = are)
  
  # Gain forest
  gai <- tibble(value = c(0, 1), class_gain = c('No gain', 'Gain'))
  frq.gai <- freq(rst[[3]])
  frq.gai <- inner_join(frq.gai, gai, by = 'value')
  frq.gai <- dplyr::select(frq.gai, class_gain, count)
  frq.gai <- mutate(frq.gai, has_class_gain = count * res)
  
  # Finish
  Sys.sleep(3)
  cat('Done!\n')
  return(list(frq.lss, frq.gai))
  
}

saveRDS(tbls, file = 'tbls.rds')
tbls <- purrr::map(.x = 1:nrow(znes), .f = function(i) extractMask(zne = znes[i,]))

base <- znes %>% st_drop_geometry() %>% distinct(Comunidad, bioma)
tbls <- inner_join(tbls, base, by = 'Comunidad')

tbls <- bind_rows(tbls)

xlsx::write.xlsx(x = as.data.frame(tbls), file = 'results/tables/forest_noforest_allYears.xlsx', sheetName = 'Tables', row.names = FALSE, append = FALSE)



