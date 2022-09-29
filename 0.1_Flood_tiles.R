#!/usr/bin/env Rscript

# Loading packages -----
library(sf)
library(raster)
library(gdalUtils)
library(rgdal)
library(plyr)
library(dplyr)
library(parallel)
library(fasterize)
library(maps)
library(OpenImageR)

# Number of cores to use
n_cores = 

# making Fire Data Tiles
# setwd("/Users/maclark/Desktop/Reforestation_Hub")
setwd("/data/pubh-glob2loc/pubh0329/Reforestation_Hub/")

# Loading functions
source(paste0(getwd(),'/Scripts/0.0_Reforestation_Functions_30March2022.R'))

# Used for template
bps.dat <- paste0(getwd(),'/Raw Raster Data/LF_BiphysicalPotential/Tif/LC16_BPS_200.tif')


# Managing Raster Tiles
r <- raster(bps.dat)
n.side <-  20  # number of tiles per side
dx     <- (extent(r)[2]- extent(r)[1])/ n.side  # extent of one tile in x direction
dy     <- (extent(r)[4]- extent(r)[3])/ n.side  # extent of one tile in y direction
xs     <- seq(extent(r)[1], by= dx, length= n.side) #lower left x-coordinates
ys     <- seq(extent(r)[3], by= dy, length= n.side) #lower left y-coordinates
cS     <- expand.grid(x= xs, y= ys)

# Writing flood tiles
dir.create(paste0(getwd(),'/Raw Raster Data/Managed_Flood_Tiles'))

# Managing flood data
flood.tiles <-
  list.files(paste0(getwd(),'/Raw Raster Data/Flood Data/1in5_pluvial'), full.names=TRUE) %>%
  .[!grepl('.aux',.)]
# flood.tiles <- list.files(flood.tiles, full.names = TRUE)
flood.extents <- get.flood.extents(flood.tiles)

mclapply.flood.function <- 
  function(x) {
    # Loading bps data
    ex1 <- c(cS[x,1], cS[x,1]+dx, cS[x,2], cS[x,2]+dy)  # create extents for cropping raster
    
    # Bps tile
    bps.tile <- crop(raster(bps.dat), ex1)
    
    # Flooding
    # Getting extent of the flood tiles
    flood.extent <-
      projectExtent(bps.tile, crs = crs(raster(flood.tiles[1])))
    # Getting overlapping tiles
    flood.extent.overlaps <-
      get.flood.overlaps(flood.extent)
    
    if(nrow(flood.extent.overlaps) > 0) {# And merging these
      flood.wgs84 <- merge.flood.tiles(flood.extent.overlaps$tile_path)
	    # Writing raster
      writeRaster(flood.wgs84,
                  paste0(getwd(),'/Raw Raster Data/Managed_Flood_Tiles/Flood_tile_wgs84',x,'.tif'))
      
      
      # Destination file
      dest_file <- paste0(getwd(),'/Raw Raster Data/Managed_Flood_Tiles/Flood_tile_big_',x,'.tif')
      
      in_crs <- as.character(crs(flood.wgs84))
      out_crs <- as.character(crs(bps.tile))
      tr = c(30,30)
      e <- bbox(extent(bps.tile))
      # Resampling using rgdal
      
      gdalwarp(paste0(getwd(),'/Raw Raster Data/Managed_Flood_Tiles/Flood_tile_wgs84',x,'.tif'), 
               dest_file, 
               # t_srs = 'EPSG:6350',
               t_srs = crs(bps.tile),
               r = 'near',
               tr = c(30,30),
               te = c(e),
               # compress = 'DEFLATE',
               pred = 2,
               zlevel = 3)
      
      tmp.raster <- raster(dest_file)
      raster::writeRaster(tmp.raster,
                          gsub('big_','',dest_file),
                          overwrite = TRUE)
      # flood.tile <- raster(dest_file)
      file.remove(paste0(getwd(),'/Raw Raster Data/Managed_Flood_Tiles/Flood_tile_wgs84',x,'.tif'))
      file.remove(dest_file)
      rm(flood.wgs84)
      
      removeTmpFiles()
    } else {
      write.csv(data.frame(No_flood_data = 'None'),
                paste0(getwd(),'/Raw Raster Data/Managed_Flood_Tiles/Flood_tile_',x,'.csv'), row.names = FALSE)
    }
    
    
  }


mclapply(1:n.side^2,
       mclapply.flood.function,
       mc.cores = n_cores)
