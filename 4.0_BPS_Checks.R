#!/usr/bin/env Rscript

# This is a third check to ensure we all data for CONUS is done
# And that included/excluded BPS tiles have been properly incorporated into the analysis

# Libraries
library(raster)
library(plyr)
library(dplyr)
library(parallel)

# Setting working direcotry
setwd("/data/pubh-glob2loc/pubh0329/Reforestation_Hub/")
#bps.dat <- paste0(getwd(),'/Raw Raster Data/LF_BiphysicalPotential/Tif/LC16_BPS_200.tif')
#bps.raster <- raster(bps.dat)

# Reprojecting BPS raster and saving
cat('Reprojecting Raster')
conus.raster <- raster(paste0(getwd(),'/CONUS_Rasters/CONUS_total_opportunity_deducted.tif'))
# reprojecting
bps.reprojected <- projectRaster(bps.raster,conus.raster,method = 'ngb')
# Saving
writeRaster(bps.reprojected,paste0(getwd(),'bps_check_raster.tif'))

# Overlaps between BPS and CONUS analysis
cat('Getting overlaps')
bps.reprojected <- raster('/data/pubh-glob2loc/pubh0329/bps_check_raster.tif')


# Managing Raster Tiles
r <- raster(conus.raster)
n.side <-  20  # number of tiles per side
dx     <- (extent(r)[2]- extent(r)[1])/ n.side  # extent of one tile in x direction
dy     <- (extent(r)[4]- extent(r)[3])/ n.side  # extent of one tile in y direction
xs     <- seq(extent(r)[1], by= dx, length= n.side) #lower left x-coordinates
ys     <- seq(extent(r)[3], by= dy, length= n.side) #lower left y-coordinates
cS     <- expand.grid(x= xs, y= ys)

# values to included
included.bps <- read.csv(paste0(getwd(),'/Other Data Inputs/mmc3.csv'))
excluded.grasslands <- read.csv(paste0(getwd(),'/Other Data Inputs/BPS_to_remove_grasslands.csv'),stringsAsFactors = FALSE)

# Checking - this should end up with an empty folder
dir.create(paste0(getwd(),'/BPS_Checks/'))

# Function to loop
loop.function <- function(i) {
	# Loading bps data
  ex1 <- c(cS[i,1], cS[i,1]+dx, cS[i,2], cS[i,2]+dy)  # create extents for cropping raster
  tmp.bps <- crop(bps.reprojected, ex1) # crop raster by extent
  tmp.conus <- crop(conus.raster,ex1)

  # data frame
  tmp.df <- data.frame(bps = getValues(tmp.bps),sequest = getValues(tmp.conus))


  tmp.df <- tmp.df %>% filter(!is.na(sequest)) %>% filter(sequest > 0)
  # filtering
  tmp.df.check <-
	  tmp.df %>%
	  filter(!(bps %in% included.bps$BPS_Groups))

  tmp.df.check <- tmp.df.check %>% dplyr::select(bps) %>% unique()

  tmp.df.check.grass <-
	  tmp.df %>%
	  filter(bps %in% excluded.grasslands$BPS_Groups) %>%
	  dplyr::select(bps) %>% unique()

  if(nrow(tmp.df.check)>0){write.csv(tmp.df.check,paste0(getwd(),'/BPS_Checks/Tile_included',i,'.csv'))}
  if(nrow(tmp.df.check.grass)>0){write.csv(tmp.df.check.grass,paste0(getwd(),'/BPS_Checks/Tile_grassland',i,'.csv'))}
}


mclapply(1:400,loop.function,mc.cores = 15)

