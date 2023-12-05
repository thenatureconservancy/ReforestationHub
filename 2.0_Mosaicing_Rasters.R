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

# Creating directory
# dir.create(paste0(getwd(),'/outputs/NY_Rasters'))
tmp_dir <- tempdir(); tmp_dir # get path to temp dir

# following command forces 'gdalUtils' to use GDAL installed in OSGeo
 gdal_setInstallation("C:\\OSGeo4W\\bin", rescan = TRUE)

# Raster types
raster.types <- 
  list.files(path = paste0(getwd(),'/outputs/Raster_Outputs')) %>%
  gsub("_.*","",.) %>% unique(.)

# Full list of raster tiles
raster.tiles <- list.files(path = paste0(getwd(),'/outputs/Raster_Outputs'), full.names = TRUE)

# r <- raster.types[17]

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
                 dst_dataset = paste0(getwd(),'/outputs/CONUS_Rasters/CONUS_',r,'_opportunity_nondeducted.tif'),
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
    mosaic_rasters(raster.tiles.deducted,
                   dst_dataset = paste0(getwd(),'/outputs/CONUS_Rasters/CONUS_',r,'_opportunity_deducted.tif'),
                   co = 'COMPRESS=DEFLATE',
                   pred=2,
                   zlevel=3)
  }
}


mclapply(raster.types,mosaicrasterfunction, mc.cores=1)
