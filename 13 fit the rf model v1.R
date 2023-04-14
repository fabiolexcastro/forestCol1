
# To adjust the model
# Mar 15 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, fs, glue, crayon, randomForest, DataEditR, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Source ------------------------------------------------------------------
source('https://raw.githubusercontent.com/fabiolexcastro/rfSalvador/master/FunctionsRFclustering.R')

# Clustering functions ----------------------------------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  
  datRF_presences <- occ[,5:ncol(occ)]
  print(nrow(datRF))
  
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  
  return(list(labelRF, clusterdata))
  
}

# Load data ---------------------------------------------------------------
znes <- terra::vect('gpkg/projects_3.gpkg')
biom <- terra::vect('shp/biomes/biomes_geo.shp')
stck <- terra::rast('raster/input/variables/stack_allvars2.tif')

# Remove Caribean Biom
biom <- biom[-1,]

# Presences / pseudo-absences by each project -----------------------------
fles <- dir_ls('tble/samples/v1', regexp = '.csv') %>% 
  grep('swd', ., value = T) %>% 
  as.character()

znes$names <- tolower(znes$names)

# To create a function  ---------------------------------------------------
fle <- fles[31]

makeclusterRF <- function(fle){
  
  cat(green('... Processing: ', basename(fle), '\t'))
  tble <- suppressMessages(read_csv(fle))
  View(tble)
  tble <- drop_na(tble)
  binm <- map_chr(str_split(basename(fle), pattern = '_'), 2)
  name <- gsub('.csv', '', map_chr(str_split(basename(fle), pattern = '_'), 3))
  name <- tolower(name)
  
  cat(green('Processing: ', name, 'in the biom ', binm, '\n'))
  
  cat(green('Get the mask\n'))
  zone <- znes[znes$names == name,]
  mask <- terra::mask(terra::crop(stck[[1]], zone), zone) * 0 + 1
  
  cat(yellow('Bioma: ', binm, '\t'))
  occr <- filter(tble, pb == 1)
  occr <- drop_na(occr)
  back <- filter(tble, pb == 0)
  back <- drop_na(back)
  
  if(nrow(occr) > 10000){
  
    message('Major than 10000 presences')
    occr <- sample_n(tbl = occr, size = round(nrow(occr) * 0.01, 0), replace = FALSE)
    back <- sample_n(tbl = back, size = round(nrow(back) * 0.01, 0), replace = FALSE)

  } else if(nrow(occr) > 5000 & nrow(occr) < 10000) {
  
    message('Between 5000 and 10000 presences')
    occr <- sample_n(tbl = occr, size = round(nrow(occr) * 0.02, 0), replace = FALSE)
    back <- sample_n(tbl = back, size = round(nrow(back) * 0.02, 0), replace = FALSE)

  }
  
  cat(yellow('To fit the clustering\n'))
  env_values <- as.matrix(occr[,6:ncol(occr)]); nrow(env_values)
  datRF <- as.data.frame(occr[,6:ncol(occr)]); nrow(datRF)
  d <- dist(datRF, method = "euclidean")  
  rfClust <- rf.clust(occ = occr, nforest = 25, ntrees = 100, nVars = 5, nclasses = 5)
  labelRF <- rfClust[[1]]
  clusterdata <- rfClust[[2]]
  classdata <- cbind(pb = as.factor(labelRF), occr[,6:ncol(occr)])
  clusteredpresdata <- cbind(occr, cluster = labelRF) %>% na.omit() %>% tbl_df()
  no.clusters <- 5
  
  run <- 'run_1'
  dout <- glue('rData/rf_v1/{name}_{binm}/{run}')
  dir_create(dout)
  
  save(datRF, file = paste0(dout, '/datRF.rData'))
  save(clusterdata, file = paste0(dout, '/clusterdata.rData'))
  save(occr, clusteredpresdata, no.clusters, labelRF, file = paste0(dout, '/clustereddata.rData'))
  save(occr, file = paste0(dout, '/occr.rData'))
  save(back, file = paste0(dout, '/back.rData'))
  cat(green('---------------- Finish!\n'))
  
}

map(fles[32:length(fles)], makeclusterRF)

rslt <- dir_ls('rData/rf_v1') %>% 
  as.character()

# Cocoman 
coco <- grep('Cocoman', fles, value = T) %>% read_csv() %>% filter(pb == 1)
unique(coco$pb)
plot(biom)
points(coco$x, coco$y, pch = 16, col = 'red')
znes
coco_zone <- znes[znes$names == 'cocoman',]
plot(coco_zone, add = T, col = 'red')

# Intersection 
coco_zone <- terra::intersect(coco_zone, biom)
coco_zone$names <- paste0(coco_zone$names, '-', coco_zone$DeCodigo)
znes
coco_zone <- coco_zone[,c('Inicio', 'Area_Ha', 'Bioma', 'names')]
coco_zone_andn <- coco_zone[coco_zone$names == 'cocoman-Andean']
coco_zone_pacf <- coco_zone[coco_zone$names == 'cocoman-Pacifi',]

znes_new <- rbind(znes[znes$names != 'cocoman',], coco_zone_andn, coco_zone_pacf)
sort(znes_new$names)
writeVector(znes_new, 'gpkg/projects_2.gpkg')




