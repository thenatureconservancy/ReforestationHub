# Adding a crosswalk to the US PAD database

# Libraries
library(raster)
library(rgdal)
library(foreign)

# working directory
setwd('/data/pubh-glob2loc/pubh0329/Reforestation_Hub')
# importing dbf of PAD database
pad.dbf <-
  readOGR(paste0(getwd(),"/Raw Vector Data/Protected Areas/Not Reprojected/pad_tmp_shape.shp"))

shp.keep <- pad.dbf

pad.dbf$manager_numeric <-
  as.numeric(factor(pad.dbf$Mang_Name))
pad.dbf$SHAPE_Area <- # Rounding shape area - run into error if not doing this
  round(pad.dbf$SHAPE_Area, digits = 2)

# writing to new shape file
writeOGR(pad.dbf, 
         "/Users/maclark/Desktop/Reforestation_Hub/Raw Vector Data/Protected Areas/Not Reprojected/", 
         "pad_shape_manager_numeric",
         driver = 'ESRI Shapefile')  

# Reproject in QGIS


pad.shp <- readOGR('/Users/maclark/Desktop/Reforestation_Hub/Raw Vector Data/Protected Areas/Reprojected/us_pad_reprojected.shp')
pad.shp$desgination_numeric <- as.numeric(factor(pad.shp$Des_Tp))
pad.shp$SHAPE_A <- round(pad.shp$SHAPE_A,digits = 0)
pad.shp$SHAPE_Area <- round(pad.shp$SHAPE_Area,digits = 0)
writeOGR(pad.shp,
         "/Users/maclark/Desktop/Reforestation_Hub/Raw Vector Data/Protected Areas/Reprojected/", 
         'pad_shape_manager_designation_numeric',
         driver = 'ESRI Shapefile')

