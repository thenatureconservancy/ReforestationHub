#!/usr/bin/env Rscript

#
###
# Mosaicing the reforestation opportunity tiles into CONUS rasters

# Libraries
library(raster)
library(gdalUtils)
library(rgdal)
library(plyr)
library(dplyr)
library(sf)
library(parallel)
library(fasterize)
library(maps)
library(OpenImageR)

# Setting working directory
setwd('/data/pubh-glob2loc/pubh0329/Reforestation_Hub')

# Creating directory
dir.create(paste0(getwd(),'/CONUS_Rasters'))

# Raster types
raster.types <- 
  list.files(path = paste0(getwd(),'/Raster_Outputs')) %>%
  gsub("_.*","",.) %>% unique(.)

# Full list of raster tiles
raster.tiles <- list.files(path = paste0(getwd(),'/Raster_Outputs'), full.names = TRUE)

# Function to do in parallel - can also run in a loop
# Looping through raster types to make CONUS rasters
mosaicrasterfunction <- function(r) {
#for(r in raster.types) {
  cat(r)
	# Getting tiles for that raster type
  # Getting tiles for that raster type
  if(gsub('tile','',r) != r | r %in% 'total') {
    raster.tiles.type <- raster.tiles[grepl(r, raster.tiles)]
  } else {
    raster.tiles.type <- raster.tiles[grepl(paste0(r,'_tile'), raster.tiles)]
  } 
  # And mosaicing for non-deducted
  # List of tiles to mosaic
  raster.tiles.nondeducted <- raster.tiles.type[!grepl('ded',raster.tiles.type)]
  # And mosaicing
  mosaic_rasters(raster.tiles.nondeducted,
                 dst_dataset = paste0(getwd(),'/CONUS_Rasters/CONUS_',r,'_opportunity_nondeducted.tif'),
                 co = 'COMPRESS=DEFLATE',
                 pred=2,
                 zlevel=3)
  
  # For troubleshooting if needed
  # tmp <- raster(paste0(getwd(),'/CONUS_Rasters/CONUS_opportunity_',r,'.tif'))
  # tmp
  
  # And mosaicing for deducted tiles
  # List of tiles to mosaic
  raster.tiles.deducted <- raster.tiles.type[grepl('ded',raster.tiles.type)]
  # And mosaicing
  if(length(raster.tiles.deducted)>0) { # Do not need to mosaic if there are no tiles - this happens for the land cover and land owner tiles, as the spatial patterning of these is identical regardless of whether the deductions are applied
    mosaic_rasters(raster.tiles.type[!grepl('ded',raster.tiles.type)],
                   dst_dataset = paste0(getwd(),'/CONUS_Rasters/CONUS_',r,'_opportunity_deducted.tif'),
                   co = 'COMPRESS=DEFLATE',
                   pred=2,
                   zlevel=3)
  }
}


mclapply(raster.types,mosaicrasterfunction, mc.cores=10)
