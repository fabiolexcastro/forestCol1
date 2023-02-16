
# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, crayon, tidyverse, gtools, rgeos, raster)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dirs <- dir_ls('raster/output/ideam') %>% as.character()
fles <- map(dirs, dir_ls, regexp = '.tif$') %>% map(., as.character)
lbls <- tibble(value = 1:5, categoria = c('Bosque estable', 'No bosque', 'Bosque nuevo', 'Sin información', 'No bosque estable'))

# Function ----------------------------------------------------------------
calcArea <- function(year){
  
  # Proof
  # year <- 2017
  
  print(paste0('Year: ', year, '-------------------------'))
  # Filtering
  fles <- grep(year, dirs, value = T) %>% dir_ls(., regexp = '.tif$') %>% as.character()
  nmes <- grep('bff', fles, value = T) %>% basename() %>% gsub('bffr_forest_', '', .) %>% gsub('.tif', '', .)
  nmes <- nmes[-grep('2021', nmes, value = F)]
  
  rslt <- purrr::map(.x = 1:length(nmes), .f = function(i){
    
    try(expr = {
      
      nme <- nmes[i]
      print(nme)
      fls <- grep(nme, fles, value = T)
      
      # Year N
      frs <- grep('/forest_', fls, value = T) 
      frs <- frs[-grep('2021', frs, value = F)]
      frs <- terra::rast(frs)
      
      bff <- grep('bffr', fls, value = T) 
      bff <- bff[-grep('2021', bff, value = F)]
      bff <- terra::rast(bff)
      
      # Year 2021
      f21 <- grep('/forest_', fls, value = T) 
      f21 <- grep('2021', f21, value = T)
      f21 <- rast(f21)
      
      b21 <- grep('/bffr_forest_', fls, value = T)
      b21 <- grep('2021', b21, value = T)
      b21 <- rast(b21)
      
      # Remove the attribute values (character)
      frs <- frs * 1
      bff <- bff * 1
      f21 <- f21 * 1
      b21 <- b21 * 1
      
      # To remove the 0 
      frs[which.lyr(frs == 0)] <- NA
      bff[which.lyr(bff == 0)] <- NA
      f21[which.lyr(f21 == 0)] <- NA
      b21[which.lyr(b21 == 0)] <- NA
      
      cnd1 <- identical(terra::crs(frs), terra::crs(bff))
      cnd2 <- identical(terra::crs(f21), terra::crs(b21))
      
      if(cnd1 == TRUE){
        cat(green('CRS identical\n'))
      } else {
        cat(red('CRS not identical\n'))
        bff <- terra::project(bff, terra::crs(frs))
        # cat(green('CRS identical\n'))
      }
      
      if(cnd2 == TRUE){
        cat(green('CRS identical\n'))
      } else {
        cat(red('CRS not identical\n'))
        b21 <- terra::project(b21, terra::crs(f21))
      }
      
      fr <- c(frs, f21)
      names(fr) <- c('start', 'end')
      
      b21 <- terra::resample(b21, bff, method = 'near')
      bf <- c(bff, b21)
      names(bf) <- c('start', 'end')
      
      fr.df <- terra::as.data.frame(fr, xy = T) %>% as_tibble()
      bf.df <- terra::as.data.frame(bf, xy = T) %>% as_tibble()
      
      # Join with the labels (categorical)
      tb <- inner_join(fr.df, lbls, by = c('start' = 'value')) %>% 
        setNames(c('x', 'y', 'start', 'end', 'ctg_start')) %>% 
        inner_join(., lbls, by = c('end' = 'value')) %>% 
        setNames(c('x', 'y', 'start', 'end', 'ctg_start', 'ctg_end')) %>% 
        mutate(comparison = paste0(ctg_start, ' - ', ctg_end))
      
      fq <- as.data.frame(table(tb$comparison))
      names(fq) <- c('Class', 'nPixels')
      fq <- mutate(fq, Year = year)
      fq <- mutate(fq, project = nme)
      fq <- relocate(fq, project, Year, Class, nPixels)
      fq <- mutate(fq, percentage = nPixels / sum(nPixels) * 100)
      fq <- mutate(fq, percentage = round(percentage, 2))
      fq <- mutate(fq, area = 'Area proyecto')
      
      # Buffer area 
      bf <- inner_join(bf.df, lbls, by = c('start' = 'value')) %>% 
        setNames(c('x', 'y', 'start', 'end', 'ctg_start')) %>% 
        inner_join(., lbls, by = c('end' = 'value')) %>% 
        setNames(c('x', 'y', 'start', 'end', 'ctg_start', 'ctg_end')) %>% 
        mutate(comparison = paste0(ctg_start, ' - ', ctg_end))
      
      bf <- as.data.frame(table(bf$comparison))
      names(bf) <- c('Class', 'nPixels')
      bf <- mutate(bf, Year = year)
      bf <- mutate(bf, project = nme)
      bf <- relocate(bf, project, Year, Class, nPixels)
      bf <- mutate(bf, percentage = nPixels / sum(nPixels) * 100)
      bf <- mutate(bf, percentage = round(percentage, 2))
      bf <- mutate(bf, area = 'Area buffer')
      
      al <- rbind(fq, bf)
      rm(bf, fq, tb, frs, bff, b21, f21, fr.df, bf.df, tb)
      gc()
      cat('Done all!\n')
      return(al)
      
    })
    
  })
  
  rslt <- bind_rows(rslt)
  dout <- glue('tables/ideam/{year}/freq_{year}.csv')
  write.csv(rslt, dout, row.names = F)
  Sys.sleep(1)
  return(rslt)
  
}

# To calculate the comparison area ----------------------------------------
yrs  <- basename(dirs)
yrs  <- yrs[-grep('2021', yrs, value = F)]
rslt <- map(yrs, calcArea)
alld <- bind_rows(rslt)
as_tibble(alld)

library(xlsx)
write.xlsx(alld, 'tables/ideam/freq_allYears.xlsx', sheet = 'FrequencyAll', append = FALSE)

