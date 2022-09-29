#!/usr/bin/env Rscript

### Merging flood tiles

library(raster)
library(gdalUtils)
library(rgdal)
library(plyr)
library(dplyr)
setwd('/data/pubh-glob2loc/pubh0329/Reforestation_Hub')

flood.tiles <- 
  list.files(path = paste0(getwd(),'/Raw Raster Data/Flood Data/1in5_pluvial'), full.names = TRUE) %>%
  .[!(grepl('.aux',.,ignore.case=TRUE))]

mosaic_rasters(flood.tiles,
               dst_dataset = paste0(getwd(),'/Raw Raster Data/Flood Data/CONUS_one_in_five_mosaiced.tif'),
               co = 'COMPRESS=DEFLATE',
               pred=2,
               zlevel=3)

mosaic_rasters(flood.tiles,
               dst_dataset = paste0(getwd(),'/Raw Raster Data/Flood Data/CONUS_one_in_five_mosaiced_2.tif'),
               co = 'COMPRESS=DEFLATE',
               pred=2,
               zlevel=3,
               of="GTiff")


raster(dst_dataset)
