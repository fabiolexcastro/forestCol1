
# To adjust the model
# Mar 15 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, fs, glue, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
znes <- terra::vect('shp/projects/projects.shp')
biom <- terra::vect('shp/biomes/biomes_geo.shp')
stck <- terra::rast('raster/input/variables/stack_allvars.tif')

# Check the zones plotting
plot(biom, col = 'red')
plot(znes, add = TRUE, border = 'green')


# To make the sample ------------------------------------------------------
biom <- biom[-1,]

rslt <- purrr::map(.x = 1:length(biom), .f = function(i){
  
  bio <- biom[i,]
  nme <- bio$DeCodigo
  
  cat('... Processing: ', nme, '\n')
  
  stk <- terra::crop(stck, bio)
  stk <- terra::mask(stk, bio)
  
  # Get the zones for the biom
  zns <- znes[znes$Bioma == nme,]
  
  # Get the position forest rasters
  pos.frs <- grep('frst', names(stk), value = F)
  pos.lcv <- grep('lcv', names(stk), value = F)
  
  # Remove from the stack
  sub <- stk[[-c(pos.frs, pos.lcv)]]
  frs <- stk[[pos.frs]]
  lcv <- stk[[pos.lcv]]
  
  map(.x = 2:nrow(zns), .f = function(j){
    
    zne <- zns$MERGE_SRC[j]
    bse <- stk[[1]] * 0 + 1
    bsq <- frs
    lcv <- lcv
    
    get_sample(zne, bse, bsq, lcv)
    
  })
  

  
  
  
})


get_sample <- function(zne, bse, bsq, cvr){
  
  # Proof
  # zne <- zns$MERGE_SRC[2]
  # bse <- stk[[1]] * 0 + 1
  # bsq <- frs
  # cvr <- lcv
  
  # Start
  shp <- zns[zns$MERGE_SRC == zne,]
  
  # Get the mask for the project
  prs <- terra::crop(bse, shp)
  prs <- terra::mask(prs, shp)
  
  npx <- nrow(terra::as.data.frame(prs))
  
  # Get the sample from the raster 
  prc <- 10
  pnt <- as_tibble(randomPoints(mask = raster(prs), n = npx * prc / 100, lonlatCorrection = FALSE))
  pnt <- mutate(pnt, project = zne, pb = 1)
  
  # Background 
  bse[terra::extract(bse, pnt[,1:2], cell = TRUE)$cell] <- NA
  bck <- as_tibble(randomPoints(mask = raster(bse), n = nrow(pnt), lonlatCorrection = FALSE))
  bck <- mutate(bck, project = zne, pb = 0)
  
  # Join presences and pseudo-absences 
  all <- rbind(pnt, bck)
  all <- mutate(all, project = gsub('Projects\\\\', '', project))
  write.csv(all, glue('tble/samples/v1/presences_{shp$Bioma}_{unique(all$project)}.csv'), row.names = FALSE)
  
  # Get the year of the project start
  str <- shp$Inicio
  rng.str <- 2000 + c(grep(str, names(frs), value = F) - 10, grep(str, names(frs), value = F)) 
  rng.str <- rng.str[1]:rng.str[2]  
  rng.end <- 2000 + c(grep(str, names(frs), value = F), 21) 
  rng.end <- rng.end[1]:rng.end[2]
  
  bsq.str <- bsq[[grep(paste0(rng.str, collapse = '|'), names(bsq), value = FALSE)]]
  bsq.end <- bsq[[grep(paste0(rng.end, collapse = '|'), names(bsq), value = FALSE)]]
  
  # Get the modal (forest / no forest - before and after)
  bsq.str <- terra::modal(bsq.str)
  bsq.end <- terra::modal(bsq.end)
  
  # Landcover [after / before]
  lbl.lcv <- read_csv('tble/labels_color_modis_2.csv')[,-1]
  rcl.lcv <- read_csv('tble/labels_class_modis.csv', col_names = F) %>% as.matrix()
  
  # Reclassify 
  lcv.rcl <- map(.x = 1:nlyr(lcv), .f = function(k){
    cat('Landcover: ', k, '\n')
    rcl <- lcv[[k]]
    rcl <- classify(rcl, rcl.lcv)
    return(rcl)
  }) %>% do.call('c', .)
  
  lcv.str <- lcv.rcl[[grep(paste0(rng.str, collapse = '|'), names(lcv.rcl), value = FALSE)]]
  lcv.end <- lcv.rcl[[grep(paste0(rng.end, collapse = '|'), names(lcv.rcl), value = FALSE)]]
  
  # Get the modal (landcover - before and after)
  bsq.str <- terra::modal(bsq.str)
  bsq.end <- terra::modal(bsq.end)
  
  lcv.str <- terra::modal(lcv.str)
  lcv.end <- terra::modal(lcv.end)
  
  bsq.str <- floor(bsq.str)
  bsq.end <- floor(bsq.end)
  
  lcv.str <- floor(lcv.str)
  lcv.end <- floor(lcv.end)
  
  # To change the names (binary raster)
  names(lcv.str) <- 'lcv_str_modal'
  names(lcv.end) <- 'lcv_end_modal'
  names(bsq.str) <- 'bsq_str_modal'
  names(bsq.end) <- 'bsq_end_modal'
  
  # Add these raster to the stack
  sub.1 <- c(lcv.str, lcv.end, bsq.str, bsq.end, sub)
  names(sub.1)
  
  swd <- as_tibble(cbind(all, terra::extract(sub.1, all[,c(1, 2)])))
  
  # To write the table and the raster for spatial projection
  write.csv(swd, glue('tble/samples/v1/swd_{shp$Bioma}_{unique(all$project)}.csv'), row.names = FALSE)
  terra::writeRaster(x = sub.1, filename = glue('raster/input/stacks/run_1/rst_{shp$Bioma}_{unique(all$project)}.tif'))
  cat('Finish!!!!!!!!!!!\n')
  
}






