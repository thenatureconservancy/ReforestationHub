#!/usr/bin/env Rscript

###
# Managing csv files

# Installing libraries
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(tigris)

# Setting working directory
setwd('/data/pubh-glob2loc/pubh0329/Reforestation_Hub')

# Getting list of file types
file.types <- 
  list.files(paste0(getwd(),'/CSV_Outputs')) %>%
  gsub(".*_by_","",.) %>% gsub("_tile_.*","",.) %>%
  unique() %>%
  .[!grepl('faceted',.,ignore.case=TRUE)]

# Getting list of all files
file.list <- 
  list.files(paste0(getwd(),'/CSV_Outputs'), full.names = TRUE)

# Creating folder for CONUS CSVs
dir.create(paste0(getwd(),'/CONUS_CSVs'))

cat('Starting Loop')
# Looping through file types
for(f in file.types) {
 	
  # Getting non-deducted estimates
  files.nondeducted <- 
    file.list[grepl(paste0(f,'_tile'),file.list)] %>%
    .[!grepl('ded',.)]
 
	length(files.nondeducted)

  # Template data frame
  out.df <- data.frame()
  
  # Looping through these
  for(i in files.nondeducted) {
	  
    out.df <- rbind(out.df, read.csv(i))
  }
  
  # Making dummy column for aggregating
  out.df[,'aggregate'] <- out.df[,f]
  
  # Summarising by state and county
  out.df <- 
    out.df %>%
    dplyr::group_by(State, County, aggregate) %>%
    dplyr::summarise(tot_opp_area_hectares = sum(tot_opp_area_hectares, na.rm = TRUE),
                     tot_sequestration_tonnes_c = sum(tot_sequestration_tonnes_c, na.rm = TRUE))
  
  # And filtering to only keep type we want
  if(f %in% c('floodplain','streamside','total_opportunity')) {
    out.df <- out.df[which(out.df[,'aggregate'] > 0 & !is.na(out.df[,'aggregate'])),]
    out.df <- 
      out.df %>%
      dplyr::group_by(State, County) %>%
      dplyr::summarise(tot_opp_area_hectares = sum(tot_opp_area_hectares, na.rm = TRUE),
                       tot_sequestration_tonnes_c = sum(tot_sequestration_tonnes_c, na.rm = TRUE))
  } else if(f %in% 'postburn') {
    out.df <- out.df[which(out.df[,'aggregate'] < 0 & !is.na(out.df[,'aggregate'])),]
    out.df <- 
      out.df %>%
      dplyr::group_by(State, County) %>%
      dplyr::summarise(tot_opp_area_hectares = sum(tot_opp_area_hectares, na.rm = TRUE),
                       tot_sequestration_tonnes_c = sum(tot_sequestration_tonnes_c, na.rm = TRUE))
    
    # And switching name back
    names(out.df)[names(out.df) %in% 'aggregate'] <- f
  }  else {
    out.df <- out.df[which(out.df[,'aggregate'] > 0 & !is.na(out.df[,'aggregate'])),]
    
    # And switching name back
    names(out.df)[names(out.df) %in% 'aggregate'] <- f
  }
  
  # Keeping select columns
  if(!(f %in% c('current_land_cover','land_owner','postburn','floodplain','streamside','total_opportunity'))) {
    out.df <- out.df[,-which(names(out.df) %in% f)]
  }
  
    
  # And saving
  write.csv(out.df, 
            paste0(getwd(),'/CONUS_CSVs/',f,'_nondeducted.csv'), 
            row.names = FALSE)
  
  # Getting deducted estimates
  files.deducted <- 
    file.list[grepl(paste0(f,'_tile'),file.list)] %>%
    .[grepl('ded',.)] %>%
    .[!grepl('facet',.,ignore.case=TRUE)]
  
  if(length(files.deducted) > 0) {
    # Template data frame
    out.df <- data.frame()
    
    # Looping through these
    for(i in files.deducted) {
      out.df <- rbind(out.df, read.csv(i))
    }
    
    
    # Making dummy column for aggregating
    out.df[,'aggregate'] <- out.df[,f]
    
    # Summarising by state and county
    out.df <- 
      out.df %>%
      dplyr::group_by(State, County, aggregate) %>%
      dplyr::summarise(tot_opp_area_hectares = sum(tot_opp_area_hectares, na.rm = TRUE),
                       tot_sequestration_tonnes_c = sum(tot_sequestration_tonnes_c, na.rm = TRUE))
    
    # And filtering to only keep type we want
    if(f %in% c('floodplain','streamside','total_opportunity')) {
      out.df <- out.df[which(out.df[,'aggregate'] > 0 & !is.na(out.df[,'aggregate'])),]
      out.df <- 
        out.df %>%
        dplyr::group_by(State, County) %>%
        dplyr::summarise(tot_opp_area_hectares = sum(tot_opp_area_hectares, na.rm = TRUE),
                         tot_sequestration_tonnes_c = sum(tot_sequestration_tonnes_c, na.rm = TRUE))
    } else if(f %in% 'postburn') {
      out.df <- out.df[which(out.df[,'aggregate'] < 0 & !is.na(out.df[,'aggregate'])),]
      out.df <- 
        out.df %>%
        dplyr::group_by(State, County) %>%
        dplyr::summarise(tot_opp_area_hectares = sum(tot_opp_area_hectares, na.rm = TRUE),
                         tot_sequestration_tonnes_c = sum(tot_sequestration_tonnes_c, na.rm = TRUE))
      
      # And switching name back
      names(out.df)[names(out.df) %in% 'aggregate'] <- f
    }  else {
      out.df <- out.df[which(out.df[,'aggregate'] > 0 & !is.na(out.df[,'aggregate'])),]
      
      # And switching name back
      names(out.df)[names(out.df) %in% 'aggregate'] <- f
    }
    
    # And switching name back
    names(out.df)[names(out.df) %in% 'aggregate'] <- f
    
    # Keeping select columns
    if(!(f %in% c('current_land_cover','land_owner','postburn','floodplain','streamside','total_opportunity'))) {
      out.df <- out.df[,-which(names(out.df) %in% f)]
    }
    
    
    # And saving
    write.csv(out.df, 
              paste0(getwd(),'/CONUS_CSVs/',f,'_deducted.csv'), 
              row.names = FALSE)
  }
}

cat('CONUS CSVs Saved')

# And formatting these for the reforestation hub website
# Column order is:
# State, county
# CO2 for: total_opportunity_c_ton	floodplain_c_ton	streamside_c_ton	corridors_c_ton	postburn_c_ton	urbanopenspace_c_ton	challengingcropland_c_ton	pasture_c_ton	forest_c_ton	shrub_c_ton	grassland_c_ton	federal_c_ton	state_c_ton	private_c_ton	other_c_ton	usfs_c_ton	blm_c_ton
# Area for: area_land_ac	area_total_ac	total_opportunity_ac	floodplain_ac	streamside_ac	corridors_ac	postburn_ac	urbanopenspace_ac	challengingcropland_ac	pasture_ac	forest_ac	shrub_ac	grassland_ac	federal_ac	state_ac	private_ac	other_ac	usfs_ac	blm_ac	display_name	region	trees_acre	trees_ha
# Other info: display_name	region	trees_acre	trees_ha

# Reforestation template
# template <- read.csv("/Users/macuser/Downloads/reforestation_hub_state_summary_2021_02_01(1).csv")
template <- read.csv(paste0(getwd(),'/Other Data Inputs/reforestation_hub_state_summary_2021_02_01.csv'))

# Getting list of unique states and counties
# This is used to get the FIPs codes
# See table here for translation between county id (as in the raster) and county names
counties.shp <- readOGR(paste0(getwd(),"/Raw Vector Data/Counties/counties_nad6350_with_id.shp"))

# Getting table of county id to county fips codes
county.fips <- 
  data.frame(county_id = counties.shp$county_id,
             FIPS_code = as.numeric(paste0(counties.shp$STATEFP,counties.shp$COUNTYFP)))

# List of files to manage
conus.files <- list.files(paste0(getwd(),'/CONUS_CSVs'), full.names = TRUE)

# And fips codes
data(fips_codes)

cat('Rbinding CONUS files')
county.template <- data.frame()
for(f in conus.files) {
  county.template <- rbind(county.template, read.csv(f) %>% dplyr::select(State, County)) %>% unique()
}


cat('Formatting Non Deducted Files')
# Getting non-deducted files
conus.files <- list.files(paste0(getwd(),'/CONUS_CSVs'), full.names = TRUE, pattern = 'non') %>%
	.[!(grepl('land_owner_name|land_owner_type',.,ignore.case=TRUE))]

# And looping through these to add in columns
county.nondeducted <- county.template
for(f in conus.files) {
	cat(f)
	print(f)
  # Updating names to avoid repeats
  tmp <- read.csv(f)
  
  # Reshaping land cover and land manager tiles
  if(grepl('current_land_cover',f)) {
    # Reshaping land estimates
    tmp.land <-
      tmp %>% dplyr::select(-tot_sequestration_tonnes_c) %>%
      spread(current_land_cover, tot_opp_area_hectares)
    # Updating names to merge
    names(tmp.land)[!names(tmp.land) %in% c('State','County')] <-
      paste0(names(tmp.land)[!names(tmp.land) %in% c('State','County')],'tot_opp_area_hectares')
    
    # Reshaping c estimates
    tmp.c <-
      tmp %>% dplyr::select(-tot_opp_area_hectares) %>%
      spread(current_land_cover, tot_sequestration_tonnes_c)
    # Updating names to merge
    names(tmp.c)[!names(tmp.c) %in% c('State','County')] <-
      paste0(names(tmp.c)[!names(tmp.c) %in% c('State','County')],'tot_sequestration_tonnes_c')
   cat('Merging')
  head(tmp.c)
 head(tmp.land) 
    # And joining
    tmp <- left_join(tmp.c, tmp.land)
  } else if(grepl('land_owner',f)) { 
    # Reshaping land estimates
    tmp.land <-
      tmp %>% dplyr::select(-tot_sequestration_tonnes_c) %>%
      spread(land_owner, tot_opp_area_hectares)
    # Updating names to merge
    names(tmp.land)[!names(tmp.land) %in% c('State','County')] <-
      paste0(names(tmp.land)[!names(tmp.land) %in% c('State','County')],'tot_opp_area_hectares')
    
    # Reshaping c estimates
    tmp.c <-
      tmp %>% dplyr::select(-tot_opp_area_hectares) %>%
      spread(land_owner, tot_sequestration_tonnes_c)
    # Updating names to merge
    names(tmp.c)[!names(tmp.c) %in% c('State','County')] <-
      paste0(names(tmp.c)[!names(tmp.c) %in% c('State','County')],'tot_sequestration_tonnes_c')
    
    # And joining
    tmp <- left_join(tmp.c, tmp.land)
  } else {
    names(tmp)[!(names(tmp) %in% c('State','County'))] <- paste0(f %>% gsub(".*CONUS_CSVs/",'',.) %>% gsub('_(non)?ded.*','',.), names(tmp)[!(names(tmp) %in% c('State','County'))])
  }
  if(nrow(tmp) > 0) {
    county.nondeducted <- left_join(county.nondeducted, tmp)
  }
}

# Updating names to match with reforestation hub template
names(county.nondeducted)[grepl('tonnes_c', names(county.nondeducted))] <- gsub('tot_sequestration_tonnes_c','_c_ton',names(county.nondeducted)[grepl('tonnes_c', names(county.nondeducted))])
names(county.nondeducted)[grepl('hectares', names(county.nondeducted))] <- gsub('tot_opp_area_hectares','_ac',names(county.nondeducted)[grepl('hectares', names(county.nondeducted))])
# Updating challenging crop names
names(county.nondeducted)[grepl('chsoil_crop',names(county.nondeducted))] <- gsub('chsoil_crop','challengingcropland',names(county.nondeducted)[grepl('chsoil_crop',names(county.nondeducted))])

# Getting list of columns in both files
cols.order <- names(template)[names(template) %in% names(county.nondeducted)]
# And adding the state and county info, as well as the non-classified land owner in case this is helpful
cols.order <- c('State','County',cols.order,'not_classified_c_ton','not_classified_ac')
# Keeping columns
county.nondeducted <- county.nondeducted[,cols.order]
# The 'not_classified' columns refer to unclassified land managers, so changing the name of these
names(county.nondeducted)[grepl('not_classified',names(county.nondeducted))] <- paste0('land_manager_',names(county.nondeducted)[grepl('not_classified',names(county.nondeducted))])
# And changing units - tonnes c to tonnes CO2, and hectares to acres
county.nondeducted[,grepl('_ac',names(county.nondeducted))] <- # Converting hectares to acres
  county.nondeducted[,grepl('_ac',names(county.nondeducted))] * 2.47105
county.nondeducted[,grepl('_c_ton',names(county.nondeducted))] <- # Converting c to co2
  county.nondeducted[,grepl('_c_ton',names(county.nondeducted))] * 44/12

# Adding FIPs codes
county.nondeducted <-
  left_join(county.nondeducted, # Need fips code for states
            fips_codes %>% dplyr::select(State = state_name, state_code) %>% unique(.)) %>%
  left_join(.,
            data.frame(state_code = counties.shp$STATEFP,
                       County = counties.shp$NAME,
                       state_fips = counties.shp$STATEFP,
                       county_fips = counties.shp$COUNTYFP) %>% 
              unique(.) %>%
              mutate(fips = paste0(state_fips,county_fips)) %>%
              unique(.))

# flagging duplicates - some counties and cities have multiple FIPs codes - why does US Census do this?!
county.duplicates <-
  county.nondeducted %>%
  dplyr::group_by(State,County) %>%
  dplyr::summarise(count = n()) %>%
  filter(count > 1)

# and flagging these in the csv files
county.nondeducted <-
  county.nondeducted %>%
  mutate(duplicated_fips = NA) %>%
  mutate(duplicated_fips = ifelse(State %in% county.duplicates$State &
                                    County %in% county.duplicates$County, 'Duplicated',duplicated_fips)) %>%
  filter(!is.na(county_fips)) # Removing US virgin islands
  
# And saving file
write.csv(county.nondeducted,
          paste0(getwd(),'/CONUS_CSVs/reforestation_hub_redo_nondeducted.csv'),
          row.names = FALSE)

cat('Formatting Deducted Files')
# Repeating with deducted files
# Getting non-deducted files
conus.files <- list.files(paste0(getwd(),'/CONUS_CSVs'), full.names = TRUE, pattern = '_ded') %>%
  .[!(grepl('land_owner_name|land_owner_type',.,ignore.case=TRUE))]

# And looping through these to add in columns
county.deducted <- county.template
for(f in conus.files) {
  # Updating names to avoid repeats
  tmp <- read.csv(f)
  
  # Reshaping land cover and land manager tiles
  if(grepl('current_land_cover',f)) {
    # Reshaping land estimates
    tmp.land <-
      tmp %>% dplyr::select(-tot_sequestration_tonnes_c) %>%
      spread(current_land_cover, tot_opp_area_hectares)
    # Updating names to merge
    names(tmp.land)[!names(tmp.land) %in% c('State','County')] <-
      paste0(names(tmp.land)[!names(tmp.land) %in% c('State','County')],'tot_opp_area_hectares')
    
    # Reshaping c estimates
    tmp.c <-
      tmp %>% dplyr::select(-tot_opp_area_hectares) %>%
      spread(current_land_cover, tot_sequestration_tonnes_c)
    # Updating names to merge
    names(tmp.c)[!names(tmp.c) %in% c('State','County')] <-
      paste0(names(tmp.c)[!names(tmp.c) %in% c('State','County')],'tot_sequestration_tonnes_c')
    
    # And joining
    tmp <- left_join(tmp.c, tmp.land)
  } else if(grepl('land_owner',f)) { 
    # Reshaping land estimates
    tmp.land <-
      tmp %>% dplyr::select(-tot_sequestration_tonnes_c) %>%
      spread(land_owner, tot_opp_area_hectares)
    # Updating names to merge
    names(tmp.land)[!names(tmp.land) %in% c('State','County')] <-
      paste0(names(tmp.land)[!names(tmp.land) %in% c('State','County')],'tot_opp_area_hectares')
    
    # Reshaping c estimates
    tmp.c <-
      tmp %>% dplyr::select(-tot_opp_area_hectares) %>%
      spread(land_owner, tot_sequestration_tonnes_c)
    # Updating names to merge
    names(tmp.c)[!names(tmp.c) %in% c('State','County')] <-
      paste0(names(tmp.c)[!names(tmp.c) %in% c('State','County')],'tot_sequestration_tonnes_c')
    
    # And joining
    tmp <- left_join(tmp.c, tmp.land)
  } else {
    names(tmp)[!(names(tmp) %in% c('State','County'))] <- paste0(f %>% gsub(".*CONUS_CSVs/",'',.) %>% gsub('_(non)?ded.*','',.), names(tmp)[!(names(tmp) %in% c('State','County'))])
    
  }
  if(nrow(tmp) > 0) {
    county.deducted <- left_join(county.deducted, tmp)
  }
}

# Updating names to match with reforestation hub template
names(county.deducted)[grepl('tonnes_c', names(county.deducted))] <- gsub('tot_sequestration_tonnes_c','_c_ton',names(county.deducted)[grepl('tonnes_c', names(county.deducted))])
names(county.deducted)[grepl('hectares', names(county.deducted))] <- gsub('tot_opp_area_hectares','_ac',names(county.deducted)[grepl('hectares', names(county.deducted))])
# Updating challenging crop names
names(county.deducted)[grepl('chsoil_crop',names(county.deducted))] <- gsub('chsoil_crop','challengingcropland',names(county.deducted)[grepl('chsoil_crop',names(county.deducted))])

# Getting list of columns in both files
cols.order <- names(template)[names(template) %in% names(county.deducted)]
# And adding the state and county info, as well as the non-classified land owner in case this is helpful
cols.order <- c('State','County',cols.order,'not_classified_c_ton','not_classified_ac')
# Keeping columns
county.deducted <- county.deducted[,cols.order]
# The 'not_classified' columns refer to unclassified land managers, so changing the name of these
names(county.deducted)[grepl('not_classified',names(county.deducted))] <- paste0('land_manager_',names(county.deducted)[grepl('not_classified',names(county.deducted))])
# And changing units - tonnes c to tonnes CO2, and hectares to acres
county.deducted[,grepl('_ac',names(county.deducted))] <- # Converting hectares to acres
  county.deducted[,grepl('_ac',names(county.deducted))] * 2.47105
county.deducted[,grepl('_c_ton',names(county.deducted))] <- # Converting c to co2
  county.deducted[,grepl('_c_ton',names(county.deducted))] * 44/12

# And adding in fips codes
county.deducted <-
  left_join(county.deducted, # Need fips code for states
            fips_codes %>% dplyr::select(State = state_name, state_code) %>% unique(.)) %>%
  left_join(.,
            data.frame(state_code = counties.shp$STATEFP,
                       County = counties.shp$NAME,
                       state_fips = counties.shp$STATEFP,
                       county_fips = counties.shp$COUNTYFP) %>% 
              unique(.) %>%
              mutate(fips = paste0(state_fips,county_fips)) %>%
              unique(.))

# flagging duplicates - some counties and cities have multiple FIPs codes - why does US Census do this?!
county.duplicates <-
  county.deducted %>%
  dplyr::group_by(State,County) %>%
  dplyr::summarise(count = n()) %>%
  filter(count > 1)

# and flagging these in the csv files
county.educted <-
  county.deducted %>%
  mutate(duplicated_fips = NA) %>%
  mutate(duplicated_fips = ifelse(State %in% county.duplicates$State &
                                    County %in% county.duplicates$County, 'Duplicated',duplicated_fips)) %>%
  filter(!is.na(county_fips)) # Removing US virgin islands

# And saving file
write.csv(county.deducted,
          paste0(getwd(),'/CONUS_CSVs/reforestation_hub_redo_deducted.csv'),
          row.names = FALSE)

# And removing the intermediate files
file.remove(list.files(path = 'CONUS_CSVs', full.names = TRUE) %>% .[!grepl('reforestation_hub_redo',.)])


