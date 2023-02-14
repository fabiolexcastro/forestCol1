
# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, gtools, rgeos, raster)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Loading data ---------------------------------------------------------------

# Loading Shapefiles
poly <- dir_ls('shapes/proyectos/area', regexp = '.shp$') %>% as.character()
bffr <- dir_ls('shapes/proyectos/buffer', regexp = '.shp$') %>% as.character()

# Rasters
frst <- dir_ls('raster/input/ideam/col', regexp = '.img$') %>% as.character()

# Labels
lbls <- tibble(value = 1:5, categoria = c('Bosque estable', 'No bosque', 'Bosque nuevo', 'Sin información', 'No bosque estable'))

# Function ----------------------------------------------------------------

clipfunc <- function(year){
  
  # Proof 
  # year <- 2010
  
  print(year)
  yea <- paste0('-', year, '_')
  pol <- terra::vect(grep(year, poly, value = T))
  frs <- terra::rast(grep(yea, frst, value = T))
  bff <- terra::vect(grep(year, bffr, value = T))
  
  # Projecting the shapefile (from geographic coordinates to cartesian coordinates)
  pol <- terra::project(pol, terra::crs(frs))
  nct <- nrow(pol)
  bff <- terra::project(bff, terra::crs(frs))
  
  # Erasing the mask 
  bff <- terra::erase(bff, pol)
  
  # Calculating the area (hectares)
  are <- terra::expanse(pol) / 10000
  are <- round(are, 0)
  pol$ha <- are
  
  # Mapping
  purrr::map(.x = 1:nct, .f = function(i){
    
    print(i)
    # finding polygons and buffers from i to (as many as there are)
    ct <- pol[i,]
    bf <- bff[i,]
    #Obtaining the name of the polygons from the variable "MERGE_SRC"
    nm <- ct$MERGE_SRC
    
    # Project area ------------------------------------------------------------
    
    # Extract by mask (to focus only on the colored region of the raster)
    # Identifying the project zone
    fr <- terra::crop(frs, ct)
    #Focusing specifically in the project polygon
    fr <- terra::mask(fr,  ct)
    
    # Repeating the previous step with the buffer...
    # Buffer area -------------------------------------------------------------
    
    # Identifying the project zone
    frbf <- terra::crop(frs, bf)
    #Focusing specifically in the project buffer
    frbf <- terra::mask(frbf, bf)
    
    #Calculating the area contained in the pixels in hectares...
    # ...by calculating the area of the pixels (m^2) and transforming it
    # into hectares... (/10000)
    # res: resolution which means side of the pixel 
    # res(fr)[1] = 30.71699  &   res(fr)[2] = 30.26244
    
    px <- res(fr)[1] * res(fr)[2] / 10000
    
    # TABLE: Frequency of each category 
    
    # Project area
    # Calcultating the frequency (fr[]) of pixels for each category
    fq <- as.data.frame(table(fr[])) %>% mutate(Var1 = as.numeric(as.character(Var1)))
    # Pasting the name (lbls) in a table
    fq <- mutate(fq, nombre = nm) %>% inner_join(., lbls, by = c('Var1' = 'value'))
    colnames(fq) <- c('Value', 'nPixeles', 'Nombre', 'Categoria')
    fq <- mutate(fq, year = year)
    fq <- mutate(fq, area_project_ha = ct$ha, area_ha = nPixeles * px, area_ha = round(area_ha, 1))
    
    # Buffer area
    bfar <- as.data.frame(table(frbf[])) %>% mutate(Var1 = as.numeric(as.character(Var1)))
    bfar <- mutate(bfar, nombre = nm) %>% inner_join(., lbls, by = c('Var1' = 'value'))
    colnames(bfar) <- c('Value', 'nPixelesBuffer', 'Nombre', 'Categoria')
    fq <- full_join(fq, bfar, by = c('Value', 'Nombre', 'Categoria'))
    fq <- mutate(fq, area_buffer_ha = nPixelesBuffer * px, area_buffer_ha = round(area_buffer_ha, 1))
    fq <- relocate(fq, Nombre, Categoria, year, nPixeles, nPixelesBuffer, area_project_ha, area_ha, area_buffer_ha)
    
    # Saving the results
    dr <- glue('raster/output/ideam/{year}')
    do <- glue('forest_{nm}.tif')
    # Deleting "Projects" and "\" from the file names
    do <- gsub('Projects', '', do)
    do <- gsub('\\\\', '', do)
    dir_create(dr)
    
    # Raster file
    terra::writeRaster(x = fr, filename = glue('{dr}/{do}'), overwrite = T)
    terra::writeRaster(x = frbf, filename = glue('{dr}/bffr_{do}'), overwrite = T)
    
    # CSV file
    dr <- glue('tables/ideam/{year}')
    dir_create(dr)
    do <- gsub('.tif', '.csv', do)
    write.csv(fq, glue('{dr}/{do}'), row.names = F)
    
    print('Done!')
    
  })
  
  # Giving R a break...
  Sys.sleep(5)
  
}

# To apply the function ---------------------------------------------------
years <- parse_number(basename(proj))
map(years, clipfunc)






