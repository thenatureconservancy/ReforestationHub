#!/usr/bin/env Rscript


#####
# Overall premise
# Data redo for TNC Reforestation Hub

### 
# Short description
# Identify (a) reforestation estimates, (b) carbon stores, and (c) land ownership on pixel-by-pixel basis
# Summarise and aggregate to counties and states
# And further provide estimates in e.g. habitat corridors, riparian buffers, floodplains, etc

### 
# Short methods
# (a) Did land used to be forest (from  https://landfire.gov/viewer/viewer.html BPS data)
# (b) But not currently forest (From https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover)
# (c) and is not:
# lake, wetland, barren, or urban developed (from NLCD data https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover)
# not a road (TIGER - buffered to account for visibility and road medians)
# Not in wilderness (PAD-US (USGS version))
# And not in ag production with good soils (NLCD cropland classification, use gSSURGO - exclude classes 6-8, 4e (erodable soils), 5e (erodable), or 5w (waterlogged) )
# And is not recovering  from post-distrubance (NAFD https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1290)
# (d) with post processing corrections to account for imperfect satellite data
# visual inspection of whether land is in actual land cover
# visual inspection of whether forest is/is not forest

# Options ----
# Change next few lines to specify options for the analysis
# This includes e.g. carbon stores in afforestation, duration of afforestation, and so on

# Change the second column to FALSE to toggle whether different stores are included int eh carbon benefits
# Currently everything but soil organic, which is assumed to be 0.23 tonnes C/ha/yr as per Susan Cook-Patton
carbon.pools <-
  data.frame(matrix(c('Live tree',TRUE,
                      'Standing dead tree',TRUE,
                      'Understory',TRUE,
                      'Dead down wood',TRUE,
                      'Forest floor',TRUE,
                      'Soil organic',FALSE,
                      'Total nonsoil', FALSE), # this is the sum of everything except for soil organic
                    ncol = 2, byrow = TRUE)) 
names(carbon.pools) <- c('Carbon_pool','Include')

# This is to identify connectivity classes
connectivity.df <-
  data.frame(matrix(c('Diffuse flow (low)',FALSE,1,
                      'Diffuse flow (medium-low)',FALSE,2,
                      'Diffuse flow (medium-high)',FALSE,3,
                      'Diffuse flow (climate informed)',FALSE,4,
                      'Linkage/Concentrated flow area',TRUE,5,
                      'Concentrated climate flow/climate linkage',TRUE,6,
                      'Climate flow area',TRUE,7,
                      'Migration space for tidal habitat',FALSE,8),
                    ncol = 3, byrow = TRUE))
names(connectivity.df) <- c('Flow_type','keep','value')

# Change list below to exclude lands with different designations from reforestation opportunities
designations.exclude <- c('WSA','WA','SA')

# Change this to specify duration of C benefits.
# Outcomes reported as estimates per year, unless toggle below is changed
afforestation.duration <- 30 # Specify one of 0, 10, 30, 50, or 100

# Report outcome as benefits per year?
benefits.per.year = 'Yes' # Change to anything but yes if you want total benefits over duration specified by afforestation.duration

# Floodplain duration
# Needs to be one of: 5, 20, or 50
# Note that I have not unzipped these files, so you will need to do this on your end
floodplain.event <- 5


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

# Data management in QGIS/ArcGIS ----
#This is done in two steps:
# (1) Reproject Vectors/shapefiles to EPSG 6350
# This includes (a) roads data set, and (b) waterways dataset
# (2) Rasterize the reprojected vectors to the 30 x 30m resolution provided by USGS for the other layers

# Notes:
# This can be done in R using e.g. raster::rasterize() or fasterize()
# See code below for how this can be done using raster::rasterize()
# That being said, I recommend not trying to do this in R
# It is possible, but incredibly slow, and might possibly run into memory limits on smaller computers

# Data management ----
# Path to raw data
# setwd("/Users/maclark/Desktop/Reforestation_Hub")
#setwd("/data/pubh-glob2loc/pubh0329/Reforestation_Hub/")

# Loading functions
source(paste0(getwd(),'/scripts/0.0_Reforestation_Functions_30March2022.R'))

### Current land cover
# Important classes
# 11 = open water
# 12 = pernnial ice/snow
# 22 = urban, low intensity
# 23 = urban, medium intensity
# 24 = urban high intensity
# 21 = urban, open space
# 31 = barren
# 41-43 = forest
# 51-52 = shrublands
# 71-74 = herbaceous; 71 = grassland; 72 = sedge; 73 = lichens; 74 = moss
# 81-82 = planted; 81 = pasture/hay; 82 = cultivated crops
# 90 = woody wetlands
# 95 = emergant herbaceous wetlands
nlcd.dat <- paste0(getwd(),'/data/Raw Raster Data/NLCD_LandCov_Dat/nlcd_2019_land_cover_l48_20210604.img')

###
# Getting soils
soils.dat <- paste0(getwd(),'/data/Raw Raster Data/Soil Classification/CONUS_SSURGO_soils-002.tif')

### 
# Potential land cover
bps.dat <- paste0(getwd(),'/data/Raw Raster Data/LF_BiphysicalPotential/Tif/LC16_BPS_200.tif')
bps.values <- read.csv(paste0(getwd(),'/data/Raw Raster Data/LF_BiphysicalPotential/CSV_Data/LF16_BPS_200.csv'))
# Used for cross walk
bps.values.old <- read.csv(paste0(getwd(),'/data/Other Data Inputs/LF_140BPS_01122015.csv'))
# Getting burn age
burn.age.crosswalk <- read.csv(paste0(getwd(),'/data/Other Data Inputs/BpSMinTreeAge.csv'))

# Included bps classes - from Susan Cook Patton
included.bps <- read.csv(paste0(getwd(),'/data/Other Data Inputs/mmc3.csv'))

# Merging, and updating minimum burn age
bps.values.old <- 
  left_join(bps.values.old, 
            burn.age.crosswalk %>% mutate(Bps = as.character(Bps)) %>% dplyr::rename(BPS_MODEL = Bps)) %>%
  mutate(AgeMin = ifelse(AgeMin < 5, 5, AgeMin))
# Getting cross walk for bps to forest cover type
bps.type <- read.csv(paste0(getwd(),'/data/Other Data Inputs/BPS to USFS.csv'))

# Merging by old names and ids
# This is the cross walk done by sharon and co in the initial analysis
bps.usfs.old <-
  left_join(bps.type %>% dplyr::select(GROUPNAME, GROUPID, CLASS_NAME),
            bps.values.old %>% dplyr::select(VALUE, BPS_CODE, BPS_NAME, GROUPNAME, GROUPID, GROUPVEG, AgeMin)) %>%
  unique(.)

# Now matching with new values
bps.usfs.new <-
  left_join(bps.values %>% dplyr::select(VALUE, GROUPVEG, GROUPVEG),
            bps.usfs.old %>% dplyr::select(VALUE, CLASS_NAME, AgeMin)) %>%
  unique(.) %>%
  filter(VALUE %in% bps.usfs.old$VALUE)

# Table of C sequestration from afforestation
c.seq.rates <- 
  read.csv(paste0(getwd(),'/data/Other Data Inputs/C sequestration rates.csv'))

# Getting change from 0 to duration years
#Doing this in a loop because it's easy enough to do that way
c.seq.0 <- c.seq.rates %>% filter(Age %in% 0)
names(c.seq.0)[which(names(c.seq.0) %in% 'Live.tree') : ncol(c.seq.0)] <- 
  paste0(names(c.seq.0)[which(names(c.seq.0) %in% 'Live.tree') : ncol(c.seq.0)],'_age_0')

# Merging
c.seq.change <-
  left_join(c.seq.rates,
            c.seq.0 %>% filter(Age %in% 0) %>% dplyr::select(-Age))
# and subtracting
c.seq.change[,which(names(c.seq.change) %in% 'Live.tree') : which(names(c.seq.change) %in% 'Total.nonsoil')] <-
  c.seq.change[,which(names(c.seq.change) %in% 'Live.tree') : which(names(c.seq.change) %in% 'Total.nonsoil')] -
  c.seq.change[,which(names(c.seq.change) %in% 'Live.tree_age_0') : which(names(c.seq.change) %in% 'Total.nonsoil_age_0')]
# Limiting columns
c.seq.change <- c.seq.change[,1:which(names(c.seq.change) %in% 'Total.nonsoil')]
# And updating data frame
c.seq.rates <- c.seq.change

# Getting annual estimates if specified
if(benefits.per.year %in% 'Yes') {
  # Divide total benefit by years
  c.seq.rates[,which(names(c.seq.rates) %in% 'Live.tree') : which(names(c.seq.rates) %in% 'Total.nonsoil')] <- 
    c.seq.rates[,which(names(c.seq.rates) %in% 'Live.tree') : which(names(c.seq.rates) %in% 'Total.nonsoil')] / c.seq.rates[,'Age']
}

# Updating some classes manually
# Western Softwood = pinyon/juniper
c.seq.rates$CLASS_NAME[grep('wester.*softw',c.seq.rates$Tree, ignore.case = TRUE)] <- 'Pinyon/Juniper Group'

# Filtering only to get specified stores
c.seq.rates <-
  c.seq.rates[,c('Age','Region','CLASS_NAME',gsub(' ','.',carbon.pools$Carbon_pool[carbon.pools$Include %in% TRUE]))]
# Getting total column
c.seq.rates$total_seq <- rowSums(c.seq.rates[,(which(names(c.seq.rates) %in% 'CLASS_NAME') + 1) : ncol(c.seq.rates)])
# Adding SOC stores - from Susan Cook-Patton
c.seq.rates$total_seq <- c.seq.rates$total_seq + 0.23
# And updating region
c.seq.rates <-
  c.seq.rates %>%
  mutate(Region_code = ifelse(Region %in% 'Northern Lake States','NLS',
                              ifelse(Region %in% 'Northeast','NE',
                                     ifelse(Region %in% 'Southeast','SE',
                                            ifelse(Region %in% 'Northern Lake States','NLS',
                                                   ifelse(Region %in% 'Rocky Mountain, North','RMN',
                                                          ifelse(Region %in% 'Rocky Mountain, South', 'RMS',
                                                                 ifelse(Region %in% 'Pacific Southwest','PSW',
                                                                        ifelse(Region %in% c('Pacific Northwest, West','Pacific Northwest, East'), 'PNW',
                                                                               ifelse(Region %in% 'South Central','SC',
                                                                                      ifelse(Region %in% c('Central States','Great Plains'),'NPS','NOT MATCHED')))))))))))

# Merging these into the bps values
# And limiting to only target seuqestration duration and bps types that are potential reforestation opportunities
bps.values <- 
  left_join(bps.usfs.new, 
            c.seq.rates %>% filter(Age %in% afforestation.duration)) %>% # Filtering to only get target age
  unique(.) %>%
  filter(!is.na(CLASS_NAME)) %>%
  filter(CLASS_NAME != '') %>%
  mutate(AgeMin = ifelse(AgeMin < 5, 5, AgeMin)) %>%
  mutate(AgeMin = ifelse(is.na(AgeMin), 5, AgeMin)) %>%
  filter(VALUE %in% included.bps$BPS_Groups)

# Other excluded grassland bps (Ciara recreated based on Table 1 in FAQ for HUB update)
excluded.grasslands <- read.csv(paste0(getwd(),'/data/Other Data Inputs/BPS_to_remove_grasslands.csv'),stringsAsFactors = FALSE)

# And filtering again
 bps.values <-
 	bps.values %>%
 	filter(!(VALUE %in% excluded.grasslands$BPS_Groups))


# Region crosswalk
usfs.crosswalk <- 
  read.csv(paste0(getwd(),'/data/Other Data Inputs/state_regions.csv')) %>%
  mutate(region_name = ifelse(USFS_region %in% 'NE','Northeast',
                              ifelse(USFS_region %in% c('NLS'),'Northern Lake States',
                                     ifelse(USFS_region %in% 'PNW','Pacific Northwest',
                                            ifelse(USFS_region %in% "PSW", 'Pacific Southwest',
                                                   ifelse(USFS_region %in% 'RMN', 'Rocky Mountain North',
                                                          ifelse(USFS_region %in% 'RMS','Rocky Mountain South',
                                                                 ifelse(USFS_region %in% 'SC', 'South Central',
                                                                        ifelse(USFS_region %in% 'SE','Southeast',
                                                                               ifelse(USFS_region %in% 'NPS','Great Plains',NA))))))))))
# State lookup
counties.db <- readOGR(paste0(getwd(),'/data/Raw Vector Data/Counties/counties_nad6350_with_id.shp'))
states.db <- readOGR(paste0(getwd(),'/data/Raw Vector Data/State Boundaries/States_reprojected_with_numbers.shp'))
# Merging together
states.counties.db <-
  left_join(as(states.db, 'data.frame') %>% dplyr::select(STATEFP, STATE_NUM, NAME) %>% as.data.frame() %>% unique(),
            as(counties.db, 'data.frame') %>% dplyr::select(STATEFP, county_id, COUNTY_NAME = NAME))

# Merging in forest regions
state.counties.db <-
  left_join(states.counties.db, usfs.crosswalk %>% dplyr::select(NAME = State, Region, USFS_region))

###
# Historical forest dynamics
# Year by year data
# 3 = forest cover
# 4 = forest disturbance
# 0-2 = no forest/water/other
forest.dynamics.annual <-
  list.files(paste0(getwd(),'/data/Raw Raster Data/Forest_Cover_Trends/data'), full.names = TRUE) %>%
  .[!grepl('first|last',.)]

# First and last year of data
# 3 = forest cover
# 15-40 = year of disturbance (1970 + x), e.g. 15 = 1985 and 40 = 2010
# 0-2 = no forest/water/other cover
forest.dynamics.first.last <-
  list.files(paste0(getwd(),'/data/Raw Raster Data/Forest_Cover_Trends/data'), full.names = TRUE) %>%
  .[grepl('first|last',.)]

###
# Connectivity data
# Baesd on TNC analysis: http://www.conservationgateway.org/ConservationPractices/ClimateChange/Pages/RCN-Downloads.aspx#CONUS
# 1 = low connectivity; 2 = moderate connectivity; 3 = high connectivity
# Only including moderate and high connectivity in the analysis
connectivity.dat <- paste0(getwd(),'/data/Raw Raster Data/Connectivity/connectivity_compressed.tif')

###
# Road data: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.html
# This was filtered, reprojected, and rasterized in QGIS
roads.dat <- paste0(getwd(),"/data/Raw Raster Data/Roads_Data/roads_raster.tif")

###
# State boundaries
# from: https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/pad-us-data-overview?qt-science_center_objects=0#qt-science_center_objects
# The state census data
states <- paste0(getwd(),'/data/Raw Raster Data/State Boundaries/states.tif')

# See table here for translation between state id (as in the raster) and county names
states.shp <- readOGR(paste0(getwd(),'/data/Raw Vector Data/State Boundaries/states_nad6350.shp'))

###
# County boundaries
# from: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# County census data
counties <- paste0(getwd(),'/data/Raw Raster Data/County Boundaries/counties.tif')

# See table here for translation between county id (as in the raster) and county names
counties.shp <- readOGR(paste0(getwd(),"/data/Raw Vector Data/Counties/counties_nad6350_with_id.shp"))

# Getting table of county id to county fips codes
county.fips <- 
	data.frame(county_id = counties.shp$county_id,
		   FIPS_code = as.numeric(paste0(counties.shp$STATEFP,counties.shp$COUNTYFP)))

### 
# Waterways
# from: https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/National/HighResolution/GDB/
# Limited to class type StreamRiver (FCODE = 46000)
# Reprojected and rasterized in QGIS
nhd <- paste0(getwd(),'/data/Raw Vector Data/NHD Waterways/nhd_reprojected.shp')

###
# Getting wilderness areas
pad.shp <- readOGR(paste0(getwd(),'/data/Raw Vector Data/Protected Areas/Reprojected/pad_shape_manager_designation_numeric.shp'))

# tmp <- readOGR("/Users/maclark/Desktop/Reforestation_Hub/Raw Vector Data/Protected Areas/Not Reprojected/pad_shape_manager_numeric.shp")

# Getting data frame of pads
# Cross walk between object ID and manager
pad.crosswalk <-
  data.frame(pad.shp) %>% 
  dplyr::select(OBJECTI, Mng_Typ, Mang_Nm, Des_Tp, Own_Typ, Own_Nam) %>% # Getting manager name and type
  mutate(Mng_Type_Use = Mng_Typ) %>% # Creating column for corss walk
  mutate(Mng_Type_Use = ifelse(Mang_Nm %in% c('BLM','USFS'), Mang_Nm, Mng_Type_Use)) %>% # Separating out BLM and USFS
  mutate(Mng_Type_Use = ifelse(Mng_Type_Use %in% c('FED','STAT','PVT','BLM','USFS'),Mng_Type_Use,'Other')) %>% # Separating out other managers
  mutate(Mng_fill = ifelse(Mng_Type_Use %in% 'FED',1,
                           ifelse(Mng_Type_Use %in% 'STAT',2,
                                  ifelse(Mng_Type_Use %in% 'PVT',3,
                                         ifelse(Mng_Type_Use %in% 'USFS',4,
                                                ifelse(Mng_Type_Use %in% 'BLM',5,
                                                       ifelse(Mng_Type_Use %in% 'Other',6,NA))))))) %>% 
  mutate(Own_fill_use = ifelse(Own_Typ %in% c('DESG','DIST','FED','JNT','LOC','NGO','PVT','STAT','TRIB','TERR','UNK'), Own_Typ, 'Other')) %>%
  mutate(Own_name_use = ifelse(Own_Nam %in% c('NOAA','BLM','DOD','FWS','NPS','USFS'),Own_Nam,'Other')) %>%
  mutate(Own_fill = ifelse(Own_Typ %in% 'DESG',1,
			   ifelse(Own_Typ %in% 'DIST',2,
			   ifelse(Own_Typ %in% 'FED', 3,
				  ifelse(Own_Typ %in% 'JNT', 4,
					 ifelse(Own_Typ %in% 'LOC',5,
						ifelse(Own_Typ %in% 'NGO',6,
						       ifelse(Own_Typ %in% 'PVT',7,
							      ifelse(Own_Typ %in% 'STAT',8,
								     ifelse(Own_Typ %in% 'TERR',9,
									    ifelse(Own_Typ %in% 'TRIB',10,
										   ifelse(Own_Typ %in% 'UNK',11,12)))))))))))) %>%
  mutate(Own_name_fill = ifelse(Own_name_use %in% 'FWS', 1,
                           ifelse(Own_name_use %in% 'NOAA', 2,
                                 ifelse(Own_name_use %in% 'NPS', 3,
                                       ifelse(Own_name_use %in% 'BLM', 4,
                                                ifelse(Own_name_use %in% 'USFS', 5,
						       ifelse(Own_name_use %in% 'DOD',6,7))))))) %>%
  filter(!(Des_Tp %in% designations.exclude)) # Getting rid of wilderness areas

# 1 = Federal
# 2 = State
# 3 = Private
# 4 = USFS
# 5 = BLM
# 6 = Other


###
# Getting raster of wilderness areas
wilderness.areas <- paste0(getwd(),'/data/Raw Raster Data/Protected_areas/wilderness_areas.tif')

# Making 2x2 buffer
circle_r = matrix(0,nrow = 4, ncol = 4)
circle_r[2:3,2:3] <- 1

# Getting crs for burn date tiles
burn.crs <- crs(raster(paste0(getwd(),'/data/Raw Raster Data/Fire_Data/LBA_CU_1999_20200415_C01_V01_BF_L7.tif')))




# Creating folder for managed fire data
if('Managed_Fire_Tiles' %in% list.files(paste0(getwd(),'/data/Raw Raster Data'))) {
  # Do nothing
} else {
  # Getting raw burn data, and converting it into a tile with data of most recent burn
  dir.create(paste0(getwd(),'/data/Raw Raster Data/Managed_Fire_Tiles'))
}



###
# Getting extents to chunk rasters
# Doing this to make the computations more manageable on small computers
# Output from this is a list of extents for each raster chunk
# Where there are n x n chunks in the CONUS data


# Managing Raster Tiles
r <- raster(bps.dat)
n.side <-  20  # number of tiles per side
dx     <- (extent(r)[2]- extent(r)[1])/ n.side  # extent of one tile in x direction
dy     <- (extent(r)[4]- extent(r)[3])/ n.side  # extent of one tile in y direction
xs     <- seq(extent(r)[1], by= dx, length= n.side) #lower left x-coordinates
ys     <- seq(extent(r)[3], by= dy, length= n.side) #lower left y-coordinates
cS     <- expand.grid(x= xs, y= ys)

# Looks all set

# Creating function to loop through these tiles
# Creating it with a single input so that it can be easily parallelised using e.g. mclapply or foreach %dopar% or future.apply::future.lapply
# Where this single input is the extent of the raster tile to analyze

# Testing on a single raster chunk
# x <- split.extents[[50]]
# x <- 50

# Creating folder for outputs
if(!('CSV_Outputs' %in% list.files(paste0(getwd(),"/outputs")))) {
  dir.create(paste0(getwd(),"/outputs/CSV_Outputs"))
  dir.create(paste0(getwd(),"/outputs/Raster_Outputs"))
}

# 
# # Getting list of tiles you already have
# # Doing this to reduce processing time in case part of the analysis has already been conducted
# tiles.have <- list.files(paste0(getwd(),'/outputs/Raster_Outputs'), pattern = 'total.*ded', ignore.case=TRUE)
# tiles.have <- gsub('.*tile_','',tiles.have)
# tiles.have <- gsub('_ded.*','',tiles.have)
# tiles.have <- as.numeric(tiles.have)
# tiles.need <- 1:400
# tiles.need <- tiles.need[!(tiles.need %in% tiles.have)]
# # # # Printing tiles needed
#  r <- raster(bps.dat)
# # 
# # # Doing quick check to avoid looping over all tiles
#  max.values <- c()
# # # # 
#  for(x in tiles.need) {
# 
#    ex1 <- c(cS[x,1], cS[x,1]+dx, cS[x,2], cS[x,2]+dy)  # create extents for cropping raster
#    bps.tile <- crop(r, ex1) # crop raster by extent
# 
#  tmp.value <- maxValue(bps.tile)
#  cat(x, tmp.value)
#  if(tmp.value > 31) { # 11 = water, 31 = barren; if max value is either of these, then no reforestation opps
#  max.values <- c(max.values, x)
#  }
#  }
# 
#   tiles.need <- max.values
# 
# # If running with a batch file
# n = 2 # N = number of cores
# tiles.needed <- split(tiles.need, sort(tiles.need%%n))
# write.csv(tiles.needed$`0`, file = paste0(getwd(),'/data/tile.need1.csv'))
# write.csv(tiles.needed$`1`, file = paste0(getwd(),'/data/tile.need2.csv'))
# cat(tiles.need)
# 
# #tiles.need<-read.csv(paste0(getwd(),'/data/tile.need1.csv'))
# #tiles.need<-tiles.need$x
# 
#  tiles.need<-read.csv(paste0(getwd(),'/data/tile.need2.csv'))
#  tiles.need<-tiles.need$x


#tiles.need<-9

cat('Starting Loop')
#for(x in c(29,50,51)) { # For testing with a single tile
for(x in tiles.need) { #If running through a batch file
#for(x in rev(tiles.need)) {
  # Loading bps data
  ex1 <- c(cS[x,1], cS[x,1]+dx, cS[x,2], cS[x,2]+dy)  # create extents for cropping raster
  bps.tile <- crop(r, ex1) # crop raster by extent
  
  
  print(x)
  
  if(!exists('tam.raster')) {
    # Getting extent of tamalpais forests
    tam.raster <- raster(paste0(getwd(),'/data/Raw Raster Data/Tamalpais_forests/tamalpais_forest_opps.tif'))
    # Getting extent to check overlaps between bps tile and tamalapais opps
    tam.extent <- projectExtent(tam.raster, bps.tile)
    
  }
  
  
  # Checking to make sure the entire tile is not some combination ofland classes excluded from analyses
  # (a) not in water (value = 11)
  # (b) outside the CONUS (corresponds with value of -9999)
  # (c) perennial ice (value = 12)
  # (d) barren (value = 31)
  # Doing this to save time because reforestation potential in these land cover classes = 0
  if(maxValue(bps.tile)<= 31) {
    # Do nothing
  } else {
    ###
    # Getting cells that are potential forests
    cat('Getting potential forest')
    potential.forest <- potential.forests(bps.tile)
    
    # Checking whether the bps tile overlaps with the tamalpais opportunity
    if(tryCatch(!is.null(raster::crop(bps.tile,tam.extent)), error=function(e) return(FALSE))) {
      # if it does, reproject the raster
      tmp.extent <- projectExtent(bps.tile, tam.raster)
      tam.opp <- crop(tam.raster, tmp.extent)
      # Changing coordinate system and extent
      tam.opp <- raster::projectRaster(tam.opp, bps.tile, method = 'ngb')
      # Making sure grids align
      tam.opp <- raster::resample(tam.opp, bps.tile, method = 'ngb')
      
      # Converting to binary raster - don't need to do this, just a bit easier to work with
      # NAs and 0s= no opportunity
      # 1s = reforestation opportunity
      tam.opp[tam.opp>0] <- 1
      tam.opp[tam.opp %in% 0] <- NA
      
      # And adding this to potential forests
      potential.forest[tam.opp %in% 1] <- 1
      
      # Sequestration is incorproated later after sequestration rates by BPS value are calculated
    }
    
    #cat('Getting Current Forests')
    ###
    # Getting cells that are currently in forest based on nafd data
    # Current forests were defined by NAFD in the original analysis
    # They are now defined by NLCD cover in 2019

    # The commented out code below is to identify forest based on NAFD data
    # I've included it here if useful
    # Resampling to amke sure CRS/etc are the same
    # Using ngb to avoid interpolation of categorical variables
    # Logic check for projection
    #if(!identical(crs(bps.tile), crs((raster(forest.dynamics.annual[1]))))) {
    #  tmp.extent <- projectExtent(bps.tile, (raster(forest.dynamics.annual[1])))
    #  nafd.current <- crop(raster(forest.dynamics.annual %>% .[grep('2010.tif',.)]), tmp.extent)
    #  nafd.current <- raster::projectRaster(nafd.current, bps.tile, method = 'ngb')
    #  nafd.current <- raster::resample(nafd.current, bps.tile, method = 'ngb')
    #} else {
    #  nafd.current <- crop(raster(forest.dynamics.annual %>% .[grep('2010.tif',.)]), bps.tile)
    #  nafd.current <- raster::resample(nafd.current, bps.tile, method = 'ngb')
    #}
    
    # Getting current forests as per nafd
    #current.forest <- current.forests(nafd.current)
    
    
    cat('Getting Excluded Land Cover Classes')
    ###
    # Getting excluded land cover classes
    # This is open water (11); perennial ice/show (12), wetland (90, 95); developed low-high intensity (22-24; 21 = developed open space); or barren (31) lands
    # As identified by nlcd
    # nlcd cover
    # Projecting extent
    # This may throw a warning - everything looks ok
    if(!identical(crs(bps.tile), crs(raster(nlcd.dat)))) {
      tmp.extent <- projectExtent(bps.tile, (raster(nlcd.dat)))
      nlcd.tile <- crop(raster(nlcd.dat), tmp.extent)
      nlcd.tile <- raster::projectRaster(nlcd.tile, bps.tile, method = 'ngb')
      nlcd.tile <- raster::resample(nlcd.tile, bps.tile, method = 'ngb')
    } else {
      nlcd.tile <- crop(raster(nlcd.dat), bps.tile)
      nlcd.tile <- raster::resample(nlcd.tile, bps.tile, method = 'ngb')
    }
    
    # Getting Excluded land areas
    excluded.lands <- excluded.regions(nlcd.tile)
    
    # Getting current forests
    cat('Getting Current Forests')
    current.forest <- current.forests.nlcd(nlcd.tile)


    cat('Getting Wilderness Areas')
    # Wilderness tile
    tmp.extent <- projectExtent(bps.tile, (raster(wilderness.areas)))
    wild.tile <- crop(raster(wilderness.areas), tmp.extent)
    wild.tile <- raster::projectRaster(wild.tile, bps.tile, method = 'ngb')
    wild.tile <- raster::resample(wild.tile, bps.tile, method = 'ngb')
    wild.tile[!is.na(wild.tile)] <- 1 # 1 = wilderness area, 0 = anything else
    
    
    cat('Getting Soil Characteristics')
    ###
    # Getting crop cover
    # Intersected with poor soil quality
    ###
    # Poor soil quality interesected with crop
    tmp.extent <- projectExtent(bps.tile, raster(soils.dat))
    soil.tile <- crop(raster(soils.dat), tmp.extent)
    soil.tile <- raster::projectRaster(soil.tile, bps.tile, method = 'ngb')
    soil.tile <- raster::resample(soil.tile, bps.tile, method = 'ngb')
    soil.tile.opp <- soil.tile
    # Values > 0  are poor soils
    # NA values are good soils
    soil.tile.opp[soil.tile.opp > 0] <- 1
    soil.tile.opp[!(soil.tile.opp %in% 1)] <- NA
    # Getting crop land covers
    crop.tile <- nlcd.tile
    crop.tile[crop.tile %in% 82] <- 1 # Current crops
    crop.tile[crop.tile > 1] <- NA # Everything else goes to NA
    # And intersecting crop cover with poor soils
    soil.tile.opp[soil.tile.opp %in% 1 & crop.tile %in% 1] <- 5
    # And in a potential forest
    # This is: 1 = poor soils; 5 = poor soils & crop; 10 = poor soils & crop & potential forest
    soil.tile.opp[soil.tile.opp %in% 5 & potential.forest %in% 1] <- 10
    soil.tile.opp[soil.tile.opp %in% 10 & is.na(current.forest)] <- 15 # And not current forest - needed because this is from NAFD data, whereas crop is from nlcd data
    # And converting to 1s and NAs
    # Output of this is crops in high quality soils that are potential forest = 1, everything else = 0
    soil.tile.opp[soil.tile.opp < 15] <- NA
    soil.tile.opp[!is.na(soil.tile.opp)] <- 1
    soil.tile.crop <- soil.tile.opp
    
    # Doing the same for pasture
    # Don't need to worry about cropping out potential forest and current forest for this, as:
    # (1) pastures with good soils are not added back into the analysis when looking at opportunities
    # and (2) potenial forest and current forest are already accounted for when looking at pasture with poor soils
    soil.tile.opp <- soil.tile
    # Values > 0  are poor soils
    # NA values are good soils
    soil.tile.opp[soil.tile.opp > 0] <- 1
    soil.tile.opp[!(soil.tile.opp %in% 1)] <- NA
    # Getting crop land covers
    soil.tile.past <- nlcd.tile
    soil.tile.past[soil.tile.past %in% 81 & soil.tile.opp %in% 1] <- 1
    soil.tile.past[!(soil.tile.past %in% 1)] <- NA
    
    
    # Getting opportunity classes ----
    cat('Getting Connectivity Data')
    ###
    # Connectivity data
    # Location of habitat corridors for climate resilience
    # Getting extent
    if(!identical(crs(bps.tile), crs(raster(connectivity.dat)))) {
      tmp.extent <- projectExtent(bps.tile, (raster(connectivity.dat)))
      con.tile <- crop(raster(connectivity.dat), tmp.extent)
      con.tile <- raster::projectRaster(con.tile, bps.tile, method = 'ngb')
      con.tile <- raster::resample(con.tile, bps.tile, method = 'ngb')
    } else {
      con.tile <- crop(raster(connectivity.dat), bps.tile)
      con.tile <- raster::resample(con.tile, bps.tile, method = 'ngb')
    }
    con.opps <- connectivity.function(con.tile)
    
    
    cat('Getting Roads')
    ###
    # Roads data
    # 30m buffer around roads
    if(!identical(crs(bps.tile), crs(raster(paste0(getwd(),"/data/Raw Raster Data/Roads_Data/roads_raster.tif"))))) {
      tmp.extent <- projectExtent(bps.tile, (raster(paste0(getwd(),"/data/Raw Raster Data/Roads_Data/roads_raster.tif"))))
      roads.tile <- crop(raster(paste0(getwd(),"/data/Raw Raster Data/Roads_Data/roads_raster.tif")), tmp.extent)
      roads.tile <- raster::projectRaster(roads.tile, bps.tile, method = 'ngb')
      roads.tile <- raster::resample(roads.tile, bps.tile, method = 'ngb')
    } else {
      roads.tile <- crop(raster(paste0(getwd(),"/data/Raw Raster Data/Roads_Data/roads_raster.tif")), bps.tile)
      roads.tile <- raster::resample(roads.tile, bps.tile, method = 'ngb')
    }
    # Adding 2x2 buffer for roads tile
    # Doing this with python because it's faster than R
    input_array = matrix(roads.tile, byrow=TRUE, nrow = roads.tile@nrows) # Converting raster to matrix
    input_array[is.na(input_array)] <- 0
    esh_buffer = convolution(input_array,matrix(1,nrow=2,ncol=2))
    # Taking buffer python object and converting it into a raster in R
    roads.tile.convolved <- raster(esh_buffer, crs = crs(roads.tile)) # Converting to raster, setting CRS
    extent(roads.tile.convolved) <- extent(roads.tile) # Setting extent of raster
    
    
    cat('Getting Waterways')
    # Waterways data
    if(!identical(crs(bps.tile), crs(raster(paste0(getwd(),"/data/Raw Raster Data/Waterways/NHD Raster.tif"))))) {
      tmp.extent <- projectExtent(bps.tile, raster(paste0(getwd(),"/data/Raw Raster Data/Waterways/NHD Raster.tif")))
      nhd.tile <- crop(raster(paste0(getwd(),"/data/Raw Raster Data/Waterways/NHD Raster.tif")), tmp.extent)
      nhd.tile <- raster::projectRaster(nhd.tile, bps.tile, method = 'ngb')
      nhd.tile <- raster::resample(nhd.tile, bps.tile, method = 'ngb')
    } else {
      nhd.tile <- crop(raster(paste0(getwd(),"/data/Raw Raster Data/Waterways/NHD Raster.tif")), bps.tile)
      nhd.tile <- raster::resample(nhd.tile, bps.tile, method = 'ngb')
    }
    # Adding 2x2 buffer for waterways tile
    input_array = matrix(nhd.tile, byrow=TRUE, nrow = nhd.tile@nrows) # Converting raster to matrix
    input_array[is.na(input_array)] <- 0
    esh_buffer = convolution(input_array,matrix(1,nrow=2,ncol=2))
    # Taking buffer python object and converting it into a raster in R
    nhd.tile.convolved <- raster(esh_buffer, crs = crs(nhd.tile)) # Converting to raster, setting CRS
    extent(nhd.tile.convolved) <- extent(nhd.tile) # Setting extent of raster
    
    
    cat('Getting Protected Area Data')
    # US PAD data
    # This is land area idea, will be converted to e.g. manager etc later
    # For conversion between numbers (as in the .tif) and land manager, see the edited .shp file
    if(!identical(crs(bps.tile), crs(raster(paste0(getwd(),"/data/Raw Raster Data/Protected_areas/pad_ids.tif"))))) {
      tmp.extent <- projectExtent(bps.tile, (raster(paste0(getwd(),"/data/Raw Raster Data/Protected_areas/pad_ids.tif"))))
      pad.tile <- crop(raster(paste0(getwd(),"/data/Raw Raster Data/Protected_areas/pad_ids.tif")),bps.tile)
      pad.tile <- raster::projectRaster(pad.tile, bps.tile, method = 'ngb')
      pad.tile <- raster::resample(pad.tile, bps.tile, method = 'ngb')
    } else {
      pad.tile <- crop(raster(paste0(getwd(),"/data/Raw Raster Data/Protected_areas/pad_ids.tif")), bps.tile)
      pad.tile <- raster::resample(pad.tile, bps.tile, method = 'ngb')
    }
    
    # Separating out by manager where:
    # 1 = Federal
    # 2 = State
    # 3 = Private
    # 4 = USFS
    # 5 = BLM
    # 6 = Other
    # Crosswalk for other PAD ownername and ownertype can be found near the top of the script
    pad.ownername.tile <- pad.ownername.function(pad.tile)
    pad.ownertype.tile <- pad.ownertype.function(pad.tile)
    # Converting areas outside of the pad network to 99 - this corresponds with private unprotected lands
    pad.ownername.tile[is.na(pad.tile)] <- 99
    pad.ownertype.tile[is.na(pad.tile)] <- 99
    # And get land managers - see numbers immediately above for the crosswalk
    pad.tile <- pad.manager.function(pad.tile)

    cat('Getting Floodplains')
    # Flooding
    # Getting extent of the flood tiles
    flood.tile <-
      list.files(paste0(getwd(),'/data/Raw Raster Data/Managed_Flood_Tiles'), full.names = TRUE) %>%
      .[grepl(paste0("tile_",x,".tif"),.)] %>% .[!grepl('.aux',.)]
    if(length(flood.tile) %in% 0){
    flood.tile <- bps.tile
    flood.tile[!is.na(flood.tile)] <- 0
    flood.tile[is.na(flood.tile)] <- 0
    } else {
    flood.tile <- raster(flood.tile)
    }
    # raster(paste0(getwd(),'/Raw Raster Data/Managed_Floodplain_Data'))
    
    
    # And resampling
    # flood.tile <- raster::resample(flood.tile, bps.tile)
    #cat('Getting Burn Data')
    ###
    # Getting fire tile
    # burn.tile <- 
    #   raster(list.files(paste0(getwd(),'/data/Raw Raster Data/Managed_Fire_Tiles'),
    #                     full.names = TRUE,
    #                     pattern = paste0('_',x,'.tif')))
    # 
    # 
    # # Getting time to forest cover by bps class
    # min.recovery.age <- burn.age.bps(bps.tile)
    # # And a quick check to make sure there's a min age of 5 years if this comes up as NA and it is a potential forest
    # min.recovery.age[!is.na(potential.forest) & is.na(min.recovery.age)] <- 2005
    # 
    # # Year of last distrubance by year
    # tmp.extent <- projectExtent(bps.tile, crs(raster(paste0(getwd(),'/data/Raw Raster Data/Forest_Cover_Trends/data/VCT_30m_last.tif'))))
    # disturb.tile <- crop(raster(paste0(getwd(),'/data/Raw Raster Data/Forest_Cover_Trends/data/VCT_30m_last.tif')), tmp.extent)
    # disturb.tile <- raster::resample(disturb.tile, bps.tile, method = 'ngb')
    # disturb.tile <- raster::projectRaster(disturb.tile, bps.tile, method = 'ngb')
    # disturb.tile[!(disturb.tile %in% 15:40)] <- NA
    # disturb.tile <- disturb.tile+1970
    # 
    # # Getting difference between last disturbance and minimum recovery age
    # disturb.tile <- disturb.tile - min.recovery.age
    # 
    # # Splitting this into two rasters
    # # (1) Non-recovered forests after burn, e.g. post-burn opportunities
    # post.disturb.opp <- disturb.tile
    # post.disturb.opp[post.disturb.opp > 0] <- NA # Disturbance is older than min recovery
    # post.disturb.opp[!is.na(post.disturb.opp)] <- 1
    # 
    # # (2) Forests that aren't old enough to have recovered post disturbance
    # # Exclude these from reforestation opportunity
    # post.disturb.exclude <- disturb.tile
    # post.disturb.exclude[post.disturb.exclude <= 0] <- NA # Disturbance is newer than min recovery
    # 
    #
    # Geographic boundaries to be used for summing benefits
    # State
    # Don't need to do this because there is already a cross talk between counties and states
    # But here in case you want it
    # state.tile <- crop(raster(states),bps.tile)
    # state.tile <- resample(state.tile, bps.tile, method = 'ngb')
    cat('Getting County Data')
    # County
    if(!identical(crs(bps.tile), crs(raster(counties)))) {
      tmp.extent <- projectExtent(bps.tile, crs(raster(counties)))
      county.tile <- crop(raster(counties),tmp.extent)
      county.tile <- raster::projectRaster(county.tile, bps.tile, method = 'ngb')
      county.tile <- raster::resample(county.tile, bps.tile, method = 'ngb')
    } else {
      county.tile <- crop(raster(counties), bps.tile)
      county.tile <- raster::resample(county.tile, bps.tile, method = 'ngb')
    }

    cat('Calculating Sequestration Rates')
    # Sequestration rates
    # These are in tonnes C per hectare
    seq.tile <- sequestration.rates(bps.tile)
    
    # Adding in potential sequestration for tamalpais forests
    if(tryCatch(!is.null(raster::crop(bps.tile,tam.extent)), error=function(e) return(FALSE))) {
      # sequestration rates for tamalpais forests - these are for Oak/hickory tree class as per Joe and Eric at TNC
      seq.tile[tam.opp %in% 1] <- unique(bps.values$total_seq[bps.values$Region %in% 'South Central' & bps.values$CLASS_NAME %in% 'Oak/Hickory Group'])
    }
    
    
    cat('Identifying Forest Opportunities')
    # Forest opportunities
    forest.opps <- potential.forest
    forest.opps[!is.na(current.forest)] <- NA # Is not a forest
    forest.opps[!is.na(excluded.lands)] <- NA # Is not excluded land cover, including crops
    forest.opps[!is.na(soil.tile.crop)] <- 1 # But can be cropland in poor soil quality for crop- this raster already has current forest and potential forest included in the opportunity class
    forest.opps[!is.na(soil.tile.past)] <- 1 # But can be cropland in poor soil quality for crop- this raster already has current forest and potential forest included in the opportunity class
    forest.opps[!(roads.tile.convolved %in% 0)] <- NA # Is not in a road buffer
    # forest.opps[!is.na(post.disturb.exclude)] <- NA # Is not in a post disturbance landscape - add back in if useful
    forest.opps[!is.na(wild.tile)] <- NA # ANd is not in a wilderness area
    
    # Limiting to only total opps
    seq.tile[is.na(forest.opps)] <- NA
   
    # 0s in e.g. seq raster = 0 in opp raster
    # And 0s in opp raster = 0s in seq raster
    seq.tile[forest.opps%in%0] <- 0
    forest.opps[seq.tile%in%0] <- 0

    # And updating for BPS
    # If BPS has two potential sequestration values, this means they are on border of USFS region
    # In this case - take average of these values, rather than having a hard line at the USFS region
    bps.seq.df <-
	    data.frame(bps = getValues(bps.tile),
		       seq = getValues(seq.tile)) %>%
    		filter(!is.na(seq),
			seq > 0,
			!is.na(bps)) %>%
		unique()

	# Summary of above dataframe
	bps.seq.sum <-
		bps.seq.df %>%
		dplyr::group_by(bps) %>%
		dplyr::summarise(seq = mean(seq))

	# Checking to see if values need to be updated
	if(nrow(bps.seq.sum) == nrow(bps.seq.df)) {
	# Do nothing - number of rows are the same
	# Which means only unique values in bps and seq df
	} else {
	# Update values based on averages
	# Doing this in a loop based on bps tile
	# Which values are unique
	bps.seq.sum <-
		bps.seq.sum %>%
		filter(bps %in% bps.seq.df$bps &
		       !(seq %in% bps.seq.df$seq))
	
	# And looping to update
	for(bps in 1:nrow(bps.seq.sum)) {
		 seq.tile[bps.tile %in% bps.seq.sum$bps[bps] & !is.na(seq.tile)] <- bps.seq.sum$seq[bps]
		}
	}

    # Creating data frame to save raster tiles
    # This works for all opp classes except land manager and by land cover
    opp.df.rasters <- 
      data.frame(r_object = c('forest.opps','flood.tile','con.opps','nhd.tile.convolved',# 'post.disturb.opp', 
                              'soil.tile.crop', 'soil.tile.past'),
                 opp_class = c('total_opportunity_','floodplain_','corridors_','streamside_',#'postburn_', 
                               'challengingsoils', 'challengingpastures'))
    
    cat('Saving Non-Deducted Rasters')
    # Saving rasters for above opportunity classes
    save.raster.function(opp.df.rasters)
    # Saving rasters for land cover class - this also returns a raster brick of the land cover type
    land.cov.brick <- save.land.cov.function(nlcd.tile)
    # Saving rasters for land manager class - this also returns a raster brick of the manager type
    # Getting unclassified land owner
    pad.tile[is.na(pad.tile)] <- 7
    land.man.brick <- save.land.man.function(pad.tile)
    
    # Converting land cover tile into numeric values that mirror opportunity classes
    land.cov.tile <- land.cov.classes(nlcd.tile)
    
    
    cat('Saving Non-Deducted Estimates')
    # And getting opportunity classes
    # Doing this in a data frame because that is much faster than overlaying rasters in R
    opp.df <-
      data.frame(total_opportunity = getValues(forest.opps), # Could it be a forest, is not already a forest, and not in excluded land class
                 floodplain = getValues(flood.tile), # By floodplain buffer
                 streamside = getValues(nhd.tile.convolved), # By riparian buffer
                 corridors = getValues(con.opps), # Conservation corridor
#                 postburn = getValues(post.disturb.opp), # Post burn areas
                 chsoil_pasture = getValues(soil.tile.past), # Challenging pastures
                 chsoil_crop = getValues(soil.tile.crop),
                 land_owner = getValues(pad.tile), # Land manager
		 land_owner_type = getValues(pad.ownertype.tile), # Land owner type
		 land_owner_name = getValues(pad.ownername.tile), # Land owner name
                 current_land_cover = getValues(land.cov.tile), # Land cover
		 nlcd.cover = getValues(nlcd.tile), # Current land cover by nlcd class - this is a bit of a repeat above, but they're used in different ways
                 seq_rates = getValues(seq.tile), # Sequestration potential
                 county = getValues(county.tile), # By county
                 count = 1) # number of cells - used to calculate area and total sequestration
    
    # Limiting to forest opps
    # Not strictly needed, but will speed things up a fair bit
    which.forest.opps <- which(!is.na(opp.df$total_opportunity) & !is.na(opp.df$seq_rates))
    opp.df <- opp.df[which.forest.opps,]
    
    if(nrow(opp.df) > 0) {
      # Merging in county fips codes
	opp.df <- left_join(opp.df,county.fips %>% dplyr::rename(county = county_id))

    # Summing by opportunity class - this write .csv files
      opp.sum.function(opp.class = 'total_opportunity')
      opp.sum.function(opp.class = 'floodplain')
      opp.sum.function(opp.class = 'streamside')
      opp.sum.function(opp.class = 'corridors')
#      opp.sum.function(opp.class = 'postburn')
      opp.sum.function(opp.class = 'land_owner')
      opp.sum.function(opp.class = 'land_owner_type')
      opp.sum.function(opp.class = 'land_owner_name')
      opp.sum.function(opp.class = 'current_land_cover')
      opp.sum.function(opp.class = 'chsoil_pasture')
      opp.sum.function(opp.class = 'chsoil_crop')

      # And summing by every combination of the above
      # This is for faceting the website
      facet.sum.function(opp.df)
    }
    
    
    cat('Saving Deducted Rasters')
    ###
    # And adding in non-spatial deductions
    # This will save a different set of raster tiles
    # This gives a raster tile containing proportional reductions by nlcd class
    nlcd.deduction.tile <-
      nlcd.deductions(nlcd.tile)
    
    # Multiplying this by forest opps and total seq opps
    # This gives raster tiles with opportunitites after the non-spatial deductions have been incorporated
    forest.opps <- forest.opps * nlcd.deduction.tile
    seq.tile <- seq.tile * nlcd.deduction.tile
    
    cat('Saving Deducted Estimates')
    # And saving tiles again, after accounting for deductions
    # Making dataframe to do this
    opp.df.rasters <- 
      data.frame(r_object = c('forest.opps','flood.tile','con.opps','nhd.tile.convolved',#'post.disturb.opp',
                              'soil.tile.crop', 'soil.tile.past'),
                 opp_class = c('total_opportunity_','floodplain_','corridors_','streamside_',#'postburn_',
                               'challengingsoils', 'challengingpastures'))
    
    # Saving rasters for above opportunity classes
    # This saves rasters containing tonnes C/ha in each cell
    save.raster.function.deductions(opp.df.rasters)
    
    # Splitting by NLCD class
    # Saving rasters for land cover class - this also returns a raster brick of the land cover type
    land.cov.brick <- save.land.cov.function.deductions(nlcd.tile)
    
    # And saving csv files again
    # And getting opportunity classes
    # Doing this in a data frame because that is much faster than overlaying rasters in R
    opp.df <-
      data.frame(total_opportunity = getValues(forest.opps), # Could it be a forest, is not already a forest, and not in excluded land class
                 floodplain = getValues(flood.tile), # By floodplain buffer
                 streamside = getValues(nhd.tile.convolved), # By riparian buffer
                 corridors = getValues(con.opps), # Conservation corridor
#                 postburn = getValues(post.disturb.opp), # Post burn areas
                 chsoil_pasture = getValues(soil.tile.past), # Challenging pastures
                 chsoil_crop = getValues(soil.tile.crop),
                 land_owner = getValues(pad.tile), # Land manager
		 land_owner_type = getValues(pad.ownertype.tile), # Land owner type
                 land_owner_name = getValues(pad.ownername.tile), # Land owner name
                 current_land_cover = getValues(land.cov.tile), # Land cover
		 nlcd.cover = getValues(nlcd.tile), # Current land cover by nlcd class - this is a bit of a repeat of the above, but they're used in different ways
                 seq_rates = getValues(seq.tile), # Sequestration potential
                 county = getValues(county.tile), # By county
                 count = 1, # number of cells - used to calculate area and total sequestration
                 area_cell = getValues(forest.opps))
    
    # Limiting to forest opps
    # Not strictly needed, but will speed things up a fair bit
    which.forest.opps <- which(!is.na(opp.df$total_opportunity) & !is.na(opp.df$seq_rates))
    opp.df <- opp.df[which.forest.opps,]
    
    if(nrow(opp.df) > 0) {


	    opp.df <- left_join(opp.df,county.fips %>% dplyr::rename(county = county_id))
      # Summing by opportunity class - this write .csv files
      opp.sum.function.deduction(opp.class = 'total_opportunity')
      opp.sum.function.deduction(opp.class = 'floodplain')
      opp.sum.function.deduction(opp.class = 'streamside')
      opp.sum.function.deduction(opp.class = 'corridors')
#      opp.sum.function.deduction(opp.class = 'postburn')
      opp.sum.function.deduction(opp.class = 'land_owner')
      opp.sum.function.deduction(opp.class = 'land_owner_type')
      opp.sum.function.deduction(opp.class = 'land_owner_name')
      opp.sum.function.deduction(opp.class = 'current_land_cover')
      opp.sum.function.deduction(opp.class = 'chsoil_pasture')  
      opp.sum.function.deduction(opp.class = 'chsoil_crop')

      # Faceted results
      facet.sum.function(opp.df)
    }
    
    removeTmpFiles()
    gc()
  } # End of if statement checking if tile is not entirely water
} # End of loop across tiles

