
# Prepare landcover variables
# Mar 8 2023

# Loading libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse, rmapshaper, gtools, fasterize, rgeos, distanceto, raster, rmapshaper)

# Cleaning workspace
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# List files --------------------------------------------------------------
fles <- dir_ls('raster/i')

