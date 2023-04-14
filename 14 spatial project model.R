
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, cclust, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos, terra, fs, glue)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
myproj <- CRS('+proj=longlat +datum=WGS84')

# Source ------------------------------------------------------------------
source('https://raw.githubusercontent.com/fabiolexcastro/rfSalvador/master/FunctionsRFclustering.R')

# Functions to use --------------------------------------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  # occ = back_swd; nforest = 50; ntrees = 500; nVars = 8; nclasses = 2
  datRF_presences <- occ[,3:ncol(occ)] %>% as.data.frame()
  print(nrow(datRF_presences))
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

# Clustered data 
dirs <- dir_ls('rData/rf_v1') %>% as.character()
dirs <- glue('{unlist(dirs)}/run_1')
map(dirs, dir_ls)

# Pseudo-absences 
fles.back <- dir_ls('tble/samples/v1', regexp = '.csv') %>% grep('swd', ., value = T) %>% as.character()

# Check the missing results files -----------------------------------------
chck <- as.character(dir_ls('rf/output/run_1/results'))
length(fles.back) - length(chck)
inpt <- basename(fles.back) %>% str_split(., '_') %>% map(3) %>% unlist() %>% gsub('.csv', '', .) %>% tolower()
chck <- basename(chck)
setdiff(inpt, chck)

# Cluster analysis to pseudo-absences -------------------------------------

# Error: 13

purrr::map(.x = 11:20, .f = function(z){
  
  back_swd <- read_csv(fles.back[z]) %>% filter(pb == 0)
  proj_nme <- unique(back_swd$project)
  back_swd <- dplyr::select(back_swd, -project, -pb, -ID)
  back_swd <- drop_na(back_swd)
  
  biom_nme <- str_split(basename(fles.back[z]), '_') %>% map(2) %>% unlist()
  
  # back_swd <- sample_n(tbl = back_swd, size = 2085, replace = FALSE)
  
  if(nrow(back_swd) > 10000){
    message('Major than 10000 presences')
    back_swd <- sample_n(tbl = back_swd, size = round(nrow(back_swd) * 0.01, 0), replace = FALSE)
  } else if(nrow(back_swd) > 5000 & nrow(back_swd) < 10000) {
    message('Between 5000 and 10000 presences')
    back_swd <- sample_n(tbl = back_swd, size = round(nrow(back_swd) * 0.02, 0), replace = FALSE)
  }
  
  back_swd <- as.data.frame(back_swd)
  datRF <- as.data.frame(back_swd[,3:ncol(back_swd)])
  attach(datRF)
  no.forests <- 50
  no.trees <- 50
  no.absenceclasses <- 2
  distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)
  labelRF <- pamNew(distRF$cl1, no.absenceclasses)
  detach(datRF)
  classdata <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])
  
  # Read the clusteredpresdata
  dirf <- grep(proj_nme, dirs, value = T)
  occr <- dir_ls(dirf) %>% grep('occr', ., value = T) %>% as.character()
  load(file = occr)
  clstrd <- dir_ls(dirf) %>% grep('clustereddata', ., value = T) %>% as.character()
  load(file = clstrd)
  
  presvalue_swd  <- clusteredpresdata[,6:ncol(clusteredpresdata)] %>%
    cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
    na.omit() %>%
    as.data.frame() %>%
    mutate(cluster = cluster + no.absenceclasses)
  
  presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))
  classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background
  
  dim(classdata_2); dim(presvalue_swd)
  presvalue_swd <- presvalue_swd %>% dplyr::select(-cluster)
  
  allclasses_swd <- rbind(classdata_2, presvalue_swd[,1:ncol(classdata_2)])
  unique(allclasses_swd$pb)
  name <- proj_nme
  dout <- glue('rf/input/points/run_1/{name}/all_classes_swd_{name}.csv')
  dir_create(dirname(dout))
  write.csv(allclasses_swd, dout, row.names = FALSE)
  
  # To make the random forest analysis --------------------------------------
  vrs <- colnames(allclasses_swd)[2:ncol(allclasses_swd)]
  model1 <- as.formula(paste('factor(pb) ~', paste(paste(vrs), collapse = '+', sep =' ')))
  rflist <- vector('list', 50) 
  auc <- vector('list', 50)
  
  samplesize <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) 
  NumberOfClusters <- 5
  
  for(repe in 1:50){ # 50 bosques
    
    print(repe)
    pressample <- list()
    
    for (i in 1:(NumberOfClusters+no.absenceclasses)){
      
      if(any(i==c(1:no.absenceclasses))) { 
        
        rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                       size = samplesize*NumberOfClusters/2/no.absenceclasses)
      } else {
        rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
      }
      pressample[[i]] <- allclasses_swd[rows,] 
    }
    
    species <- na.omit(do.call(rbind, pressample)) 
    head(species)
    Samplesplit <- sample(rownames(species)) 
    
    envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
    envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
    
    rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
    dir_create(glue('rf/output/run_1/models/{name}'))
    save(rfmodel, file = paste('rf/output/run_1/models/', name, '/', NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
    rflist[[repe]] <- rfmodel
    
    # AUC 
    predicted <- as.numeric(predict(rfmodel, envtest))
    observed <- as.vector(envtest[,'pb'])
    auc[[repe]] <- auc(observed, predicted) 
    rm(rfmodel)
    
    cat(auc[[repe]] ,'\n')
    
  }
  
  auc <- unlist(auc)
  rff <- do.call(randomForest::combine, rflist)
  importance <- as.data.frame(rff$importance)
  
  ddout <- glue('rData/rf_v1/{name}_{biom_nme}/run_1')
  save(rflist, file = paste(ddout, '/rflist_', NumberOfClusters, '.rdata', sep = ''))
  save(importance, file = paste0(ddout, '/importanceRF.rData'))
  save(auc, file = paste0(ddout, '/aucRF_dist.rData'))
  save(rff, file = paste0(ddout, '/rff_dist.rData'))
  
  # Variables stack
  all.fles <- dir_ls('raster/input/stacks/run_1')
  lyr <- stack(grep(biom_nme, grep(name, all.fles, value = T), value = TRUE))
  
  # Predict model
  climatevalues <- data.frame(getValues(lyr))
  NumberOfClusters <- 5
  
  rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
  rasterProbs_na <- na.omit(rasterProbs)
  sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)
  
  rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
  uncertainty <- apply(rasterProbs, 1, max)  
  
  rasterRFprob <- lyr[[1]]
  values(rasterRFprob) <- rasterRF 
  
  rasterRFuncertainty <- lyr[[1]]
  values(rasterRFuncertainty) <- uncertainty 
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- lyr[[1]]
  values(rasterRFclass) <- rasterRF
  
  plot(rasterRFprob)
  plot(rasterRFclass)
  
  dd <- glue('rf/output/run_1/results/{name}')
  dir_create(dd)
  
  writeRaster(rasterRFclass, paste0(dd, '/RF_5Clust_current.asc'), format = 'ascii', overwrite = T)
  writeRaster(rasterRFprob, paste0(dd, '/RF_5Prob_current.asc'), format = 'ascii', overwrite = T)
  writeRaster(rasterRFuncertainty, paste0(dd, '/RF_5Unc_current.asc'), format = 'ascii', overwrite = T)
  
})





